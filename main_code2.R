"Second approach: first iteration.

HeH urgent: concrete questions for next meeting with Cory
HeH todo: next steps todo
HeH discuss: to be discussed in term paper
HeH: annotations that add helpful information
"

library(decisionSupport)
library(tidyverse)
library(readxl)


# Read input tables----
# The input tables are divided into:
# (1) data_economical: data retrieved out ofreport from planning agency
# (2) data_biophysical: data estimated otherwise # HeH todo
# (3) data_estimates: variables that are guess really (or "parameters")
input_uncertainities <- read_excel("Data/uncertain_variables.xlsx", na = "NA")  # HeH: switch from xlsx to csv
input_data <- read_excel("data/data_estimates.xlsx", na = "NA") # bio-physical 
input_data_agency <- read_excel("data/data_agency.xlsx", na = "NA") # bio-physical 

estimate_data <- rbind(input_uncertainities, input_data_agency, input_data)
#estimate_data <- rbind(input_uncertainities, input_data)

## Model programming utilities----
## "make variables" function to create point estimates as global variable
## (because when dec. function runned in mcSimulation, local variables only)
## from Whitney et al. (Lecture material DA)
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

# create global vars for programming process
#make_variables(as.estimate(input_uncertainities))
#make_variables(as.estimate(input_data))
make_variables(as.estimate(estimate_data)) # bound data


# Scenario input
# HeH todo: into input table?
# HeH discuss: number trees in concept : 2 planting periods (yr1, yr 5), 
# also idea that 90 % abgängig -> replanting with waltnus up to 30 % ?! 
# preserved habitat: at least 100 trees)
timespan <- 55
n_trees <- 114
field_size_ha <- 1.3
#discount_rate <- 1.05 # HeH discuss: differentiate between agriculture and else!!
scenario_joint_machinery <- FALSE
machinery_joint_participants <- 10
investment_subsidy_scenario <- TRUE #HeH investment

################################################################################

# Decision function----
orchard_revitalization <- function(){
  
  # Variable declarations----
  # time span of simulation
  n_years <- timespan

  # preparation of empty vectors and lists for calculations
  # NAs whether the vector is filled (as control if each function runs)
  costs_establishment_hay_Eur <- rep(0, n_years)
  labor_fruit_establishment_h <- rep(0, n_years)
  labor_fruit_mainteance_h <- rep(NA, n_years)
  trees_dieback_number <- rep(0, n_years)
  trees_fruit_quantity_kg <- rep(NA, n_years)
  trees_fruit_quality_median_percent <- rep(NA, n_years)
  trees_fruit_reliability_percent <- rep(NA, n_years)
  costs_establishment_trees_Eur <- rep(0, n_years)
  costs_mainteance_trees_machinery_Eur <- rep(0, n_years)
  tree_fruit_price_Eur_per_kg <- rep(NA, n_years)
  trees_fruit_revenue_Eur <- rep(NA, n_years)
  trees_subsidies_establishment_Eur <- rep(0, n_years)
  trees_timber_revenue_Eur <- rep(0, n_years)
  trees_timber_labor_harvest_h <- rep(0, n_years)
  costs_timber_harvest_Eur <- rep(0, n_years)
  

  # tree age dataframe: col = tree ID, row = simluation year
  trees_ages <- data.frame(matrix(0, nrow = n_years, ncol = n_trees)) %>%
    set_names(paste0("tree", 1:n_trees)) %>%
    mutate(across(everything(), 
                  ~ replace(., row_number() == 1, 1))) # 1rst row (year): all to 1
  
  
  # Risk occurences----
  ## Probabilities of each risk to occur
  ## The damage (respectively for diebacks, quality and quantity reduction
  ## follows later)
  
  ## drought
  events_drought <- chance_event(risk_yearly_drought,
                                 value_if = 1,
                                 value_if_not = 0,
                                 n = n_years)
  #plot(events_drought)

  ## spring frost
  events_frost <- chance_event(risk_spring_frost,
                               value_if = 1,
                               #value_if_not = rep(0, n_years),
                               value_if_not = 0,
                                n = n_years )
  # plot(events_frost)
  
  ## disease/pests
  events_disease <- chance_event(risk_disease, # Heh: risk = porb * damage (so this is only the chance of desease)
                                 value_if = 1,
                                 #value_if_not = rep(0, n_years),
                                 value_if_not = 0,
                                 n = n_years)
  #plot(events_disease)
  

  
  # Hay benefit----
  # HeH todo: Include risk damage! And refine the mainteance/est costs eventually
  # HeH: discuss - too humid also bad, bud not included explicitely 
  # Heh: discuss - simplified: drought -> no yield (means that less bad cases)
  # with harvest labor costs even tough yield is low)
  hay_yield_t_max <- vv(hay_mean_t, hay_var_t, n_years)
  hay_yield_t <- hay_yield_t_max*events_drought # HeH todoadd the damage as well!
  hay_yield_revenue_Eur <- 2*hay_yield_t*(vv(hay_price_mean_Eur_t, 
                                             hay_price_var_Eur_t, 
                                             n_years)) # 2 yields per year
  
  # hay costs
  # Buying mow machine
  costs_establishment_hay_Eur[1] <- hay_costs_establishment_Eur
  
  # labor costs: only if hay grows (no drought) - otherwise, no mainteance costs
  # HeH todo: will change if risk damage included!
  labor_mainteance_hay_h <- case_when(
    hay_yield_t != 0 ~ vv(hay_labor_harvest_mean_h, hay_labor_harvest_var_h, n_years),
    TRUE ~ 0
  )
  costs_mainteance_hay_Eur <- labor_mainteance_hay_h * labor_wage_Eur_per_h_brutto
  
  # hay benefit
  benefits_hay <- hay_yield_revenue_Eur - 
    costs_establishment_hay_Eur - 
    costs_mainteance_hay_Eur
  # plot(benefits_hay)
  
  # Orchard----
  
  ## Fruit quality and quantity----
  # maximum yield dependent on tree ages in all years
  # modelled as gompertz curve
  # with estimates from report
  fruit_yield_per_age <- gompertz_yield(
    max_harvest = fruit_mean_kg_per_tree,
    time_to_first_yield_estimate = fruit_time_to_first_yield_est,
    time_to_second_yield_estimate = fruit_time_to_first_yield_est+1,
    first_yield_estimate_percent = fruit_first_yield_percent,
    second_yield_estimate_percent = fruit_second_yield_percent,
    n_years = n_years,
    var_CV = fruit_yield_var_per_tree,
    no_yield_before_first_estimate = TRUE
  )
  # plot(fruit_yield_per_age)

  
  ## Influence of risks----
  
  ### Tree_vulnerability----
  # HeH discuss - Tree vulnerability interpreted as factor: how much 
  # will the tree respond to an estimated yield reduction caused by risks?
  # Depends on age and unknown vulnerability factors
  # 90 % vulnerability == 90 % response to risks

  
  #### age influence
  # assuming it as parameterized quadratic function (curve with parameters p1, p2)
  # idea: p1 and p2 are very uncertain -> identify whether they need more detail
  # p1: age with minimum vulnerability (mature phase; vertex of curve)
  # p2: how high vulnerabilities become at beginning and end (curve compression)
  year <- 1:timespan
  tree_vulnerability_age_influence <- (uncert_tree_parameter_age_2*0.0001)*
    (year - uncert_tree_parameter_age_1)^2
  # plot(tree_vulnerability_age_influence)
  
  #### unknown risks
  # HeH discuss - included placeholder as approach focuses on risks and wants to 
  # identify how important this gap might be?
  tree_vulnerability_unknown <- vv(uncert_tree_vulnerability_mean,
                                   uncert_tree_vulnerability_var,
                                   n = n_years,
                                   relative_trend = 0.5)
  # plot(tree_vulnerability_unknown)

  # final vulnerability: add the age influence and the placeholder risk
  # maximized to 1 (100 % vulnerbability = 100 % response to risks)
  tree_vulnerability_per_age <- pmin(tree_vulnerability_unknown +
                                       tree_vulnerability_age_influence,
                                     1) 
  #plot(tree_vulnerability_per_age) 

  

  ### Risks for dieback
  # depends on disease, or drought in first and last years
  # "How high is the risk damage probability that could lead to dieback?"
  # Is processed in an chance_event later
  risk_sum_causing_diebacks <- {events_disease * vv(risk_disease_dieback_mean,
                                           risk_disease_dieback_var,
                                           n_years) +
    events_drought * vv(risk_drought_dieback_mean,
                        risk_drought_dieback_var,
                        n_years) } |> 
    pmin(1)
  # plot(risk_sum_causing_diebacks)
  
  
  ## Risks for yield reduction
  ## depends on frost and disease
  ## damage: "how much yield % loss if risk occurs"
  risk_sum_yield_reduction <- { events_disease * vv(risk_disease_yield_red_mean,
                                                  risk_disease_yield_red_var,
                                                  n_years,
                                                  relative_trend = 0.1) +
    events_frost * vv(risk_frost_yield_red_mean,
                      risk_frost_yield_red_var,
                      n_years, 
                      relative_trend = 0.2) } |> 
    pmin(1)
  
  ## risks for quality reduction
  # depends on disease, drought 
  # damage: "how much quality loss when fully affected [%]"
  risk_sum_quality_reduction <- { events_disease * vv(risk_disease_quali_red_mean,
                                                    risk_disease_quali_red_var,
                                                    n_years,
                                                    relative_trend = 0.1) +
    events_drought * vv(risk_frost_quali_red_mean,
                        risk_frost_quali_red_var,
                        n_years,
                        relative_trend = 0.2) } |> 
    pmin(1)
  
  
  # plot(risk_sum_quality_reduction)
  
  ## Supply chain----
  # principle: labor into networking to build a local supply chain
  # if enough for building a good supply chain (== regional conditions),
  # the amount of directly marketable walnuts increases
  
  labor_supply_chain_building_h <- vv(	
                                supply_chain_invest_mean_h,
                                supply_chain_invest_var,
                                n_years,
                                relative_trend = -0.5) # assume less work once the chain is build
                              #plot(labor_supply_chain_building_h)
                              
  labor_needed_for_good_market_h <- vv(uncert_invest_until_good_market_mean_h,
                                     uncert_invest_until_good_market_var,
                                     n_years,
                                     relative_trend = -0.5) # same trend as invested
  
  # calculate the direct markting capacities
  direct_marketing_maximum_kg <- ifelse(
    # check if in this year, enough labor is invested to build supply chain
    labor_supply_chain_building_h >= labor_needed_for_good_market_h,
    uncert_good_direct_market_maximum_sells_kg, # if yes, good market - higher sells
    uncert_poor_direct_market_maximum_sells_kg) # if not, bad market - lower sells


  ## Yearly loops----
  # On individual, not population level! (HeH discuss: general effect on ageing pop is viewed on ind level (discuss that))
  # This loop makes annual steps and first inspects each tree individually.
  # Dependent on the year's risk and the tree's age, trees might die. 
  # This causes replantings (causing according labor and subsidies); also the tree 
  # age is reset and the tree takes time to mature and give yield again.
  # Subsequently, depending on the tree's age: yield quality and quantity summed
  # up for all trees.
  # Resulting yield reliability and market price (depends on fruit quali and quanti).
  
  for (i in 1:n_years) {
    ### Diebacks----
    # HeH discuss: include parameter for motivation_influence? i.e. watering to prevent dieback
    
    # This section gives back a vector with 0 when dieback and 1 if survives
    trees_surviving <- trees_ages[i, ] %>% # pick each tree iteratively
      map_dbl(~ {
        age <- .x   # extract the individual tree age
        vulnerability <- tree_vulnerability_per_age[age] # extract respective vulnerability
        
        # Dependent on year's risks and the age-specific vulnerability: dieback?
        # 1 if not (survive), 0 if no (dieback)
        chance_event(risk_sum_causing_diebacks[i] * vulnerability, 
                     value_if = 0, value_if_not = 1)  
      })
    
    
    # Dieback numbers (caution, 0 == death!)
    ## which is, the initial number of trees minus the ones surviving
    dieback_numbers <- n_trees - sum(trees_surviving)
    
    ## store for output
    trees_dieback_number[i] <- dieback_numbers
    
    ## update tree ages for the following year:
    # trees surviving: "null" were trees die, then all together ageing 1 year
    trees_ages[i+1,] <- (trees_ages[i,]*trees_surviving)+1

      
    ## Tree age influences----
    # On quantity - per tree individually
    trees_fruit_quantity_kg[i] <- 
      trees_ages[i, ] %>% # take age of each individual
      unlist() %>%
      map_dbl(~ {
        tree_vulnerability_percent <- tree_vulnerability_per_age[.x] # Extract age dep. vulnerability 
       risks_yield_reduction_percent <- risk_sum_yield_reduction[i]  # "how much yield % loss if risk occurs"
       
      # Maximum yield reduced by risk damage dependent on vulnerability
      fruit_yield_per_age[.x] * ( 1 - (risks_yield_reduction_percent * tree_vulnerability_percent))
      }) %>%
      sum()
    
    # On quality
    # Heh discuss - not vulnerability dependent (responds to drought, diseases)?
      trees_fruit_quality_median_percent[i] <- trees_ages[i, ] %>%
        unlist() %>%
        map_dbl(~ {
          tree_vulnerability <- tree_vulnerability_per_age[.x]
          risk_factor <- risk_sum_quality_reduction[i] # "how much % loss
          
          # Ensure the value does not go below 0
          # HeH2: reassign the variables, rework over pmax
          # From 100 % fruit quality, substract possible risk influences
          pmax(0, 1 - (risk_factor * tree_vulnerability))
        }) %>%
       median() # HeH urgent: mean correct or median?
      
      ## Mainteance----
      
      # check for age stage 
      # planting: hours are calculated later (mor efficient)
      labor_fruit_mainteance_h[i] <- trees_ages[i, ] %>%
        unlist() %>%
        map_dbl(~ {
          age <- .x
          
          # if juvenile: 
          if (age <= 5) {
            # pruning + management (check for diseases, clearance of)
            labor_fruit_pruning_juvenile_h + 
              labor_fruit_management_juvenile_h
            
          } else if (age <= 12) {
            # if expanding:
            # pruning + management + harvest
            labor_fruit_pruning_expanding_h + 
              labor_fruit_management_expanding_h + 
              labor_fruit_harvest_expanding_h
            
          } else if (age > 12) {
            # if mature or older: 
            # pruning (every 3 years) + management + harvest
            labor_fruit_harvest_mature_h + 
              labor_fruit_management_mature_h  
              ifelse(i %% 4 == 0) # every 4 years (dividable by 4)
              labor_fruit_pruning_mature_h,
              0 # else no pruning required
              ) +
          }

        }) %>%
        sum()

      # if mature or older: 
          # pruning (every 3 years) + maint + harvest
      
      costs_mainteance_trees_pruning_Eur <- labor_mainteance_h *
        labor_wage_Eur_per_h_brutto
      
      ## Walnut price (supply chain) ----
      ## Idea:
      ## If too much to sell by themselves or quali too bad: wholesale 
      ## HeH discuss - we assume that a farmer can only to either - sell directly
      # or to wholesail. Selling surpluses form direct market would not work
      # as too low amounts and not reliable enough for wholesale.
      if (trees_fruit_quantity_kg[i] > direct_marketing_maximum_kg[i] ||  
          trees_fruit_quality_median_percent[i] < uncert_minimum_quali_for_direct_percent) {
        
        tree_fruit_price_Eur_per_kg[i] <- walnut_price_wholesale_Eur_per_kg
      
      ##  If within local market capacity and in sufficient quality: direct
      } else if (trees_fruit_quantity_kg[i] <= direct_marketing_maximum_kg[i] && 
          trees_fruit_quality_median_percent[i] >= uncert_minimum_quali_for_direct_percent) {
        
        tree_fruit_price_Eur_per_kg[i] <- walnut_price_direct_Eur_per_kg

        
      } else { 
        # print("mistake in pricing logic.")
        tree_fruit_price_Eur_per_kg[i] <- walnut_price_direct_Eur_per_kg }
      
      
      ## Fruit revenue----
      trees_fruit_revenue_Eur[i] <- trees_fruit_quantity_kg[i] * tree_fruit_price_Eur_per_kg[i]
      
      ## Yield reliability----
      # Calculates yield reliabaility - HeH: discuss idea:
      # each year in percent (0 if no yield): 
      # (1) calculates the standard deviation of the difference of maximum yield
      #     minus actual yield (which is, sd of the yield damage) up to current yr
      # (1) calculates the share of the standard deviation from the previous
      # harvests in this year's harvest [%] = yield_unreliability
      # (2) yield_reliability = 100 - yield_unreliability
      trees_fruit_reliability_percent[i] <- ifelse(
        trees_fruit_revenue_Eur[i] > 0,
        100-(100*sd(fruit_yield_per_age[1:i] - trees_fruit_quantity_kg[1:i], na.rm = TRUE)
             )/mean(trees_fruit_revenue_Eur[1:i]),
                 0)
  }
  
  # control plots
  # trees_ages
  # plot(trees_dieback_number)
  # plot(trees_fruit_quality_median_percent)
  # plot(trees_fruit_quantity_kg,
  #     #cex.axis = 2,
  #     #cex.lab = 2,
  #     xlab = "year",
  #     #ylab = "risk impacts on yield [%]",
  #     ylim = c(0,1600))
  # lines(fruit_yield_per_age* n_trees, col = "red")
  # plot(tree_fruit_price_Eur_per_kg)


   
  ## Replanting costs & subsidies----
  # Different subsidies, but here: 
  # In planting year, different subs than in the subsequent 4 years
  
  # HeH todo: if (scenario_subsidies_lost = FALSE)
  # function which calculates the 4-year subsidies after planting in a respective vector
  # y1_vec: vector (length timespan) that contains the subsidies in planting year
  # trees_vec: number of trees planted each year
  # rounds: number of years with subsequent subsidy payments
  calculate_subsequent_subs <- function(y1_vec, trees_vec, rounds = 4) {
    vector <- y1_vec != 0 # store when planted as TRUE
    sub_vector <- vector *
      ((tree_subsidies_HALM_2_1_annual_Eur_per_tree +
         tree_subsidies_HALM_2_2_y2_5_Eur_per_tree) * trees_vec) # amount of subs in following years
    shifted_vec <- sub_vector # Placeholder
    result_vector <- rep(0, n_years) # Placeholder

    for (y in 1:rounds) {
      shifted_vec <- c(0, shifted_vec[-length(sub_vector)])  # Shift to the right (following year)
      result_vector <- shifted_vec + result_vector  # "Add" to the previous years
    }
    result_vector <- result_vector+y1_vec # subs from planting and following years together
    return(result_vector)
  }

  
  ### In establishment year----
  # establishment years: per tree, 9 € HALM 2.1, HALM 2.2 90 €
  trees_subsidies_establishment_Eur[1] <- n_trees *
    (tree_subsidies_HALM_2_1_annual_Eur_per_tree +
       tree_subsidies_HALM_2_2_y1_Eur_per_tree)
  # 4 following years
  trees_planted <- rep(0, n_years) # prepare vector for subsidy calculation
  trees_planted[1] <- n_trees
  trees_subsidies_establishment_Eur <- calculate_subsequent_subs(trees_subsidies_establishment_Eur, trees_planted,
                            rounds = 4)
 # plot(trees_subsidies_establishment_Eur)
  
  
  ### In case of replantings---- 
  trees_subsidies_dieback_Eur <- trees_dieback_number *
    # Establishment year)
    (tree_subsidies_HALM_2_1_annual_Eur_per_tree +
       tree_subsidies_HALM_2_2_y1_Eur_per_tree)
    #  4 following years
  trees_subsidies_dieback_Eur <- calculate_subsequent_subs(trees_subsidies_dieback_Eur,
                              trees_dieback_number,
                              rounds = 4)
  
  # plot(trees_subsidies_dieback_Eur)
  
  ## mainteance labor from replantings: 
  # material costs 
  costs_replanting_material <- tree_establishment_material_costs_per_tree *
    trees_dieback_number
  
  # labor: less than in establishment, depends on number of tree that died back
  labor_fruit_replanting_h <- vv(fruit_labor_establishment_mean_h_per_tree,
                                 fruit_labor_establishment_var_per_tree,
                                 n_years) * trees_dieback_number
  

  costs_replanting_labor <- labor_fruit_replanting_h *
    labor_wage_Eur_per_h_brutto
  
  # total costs: material and labor; depends on invest. subsidy scenario
  costs_replanting_trees_Eur <- ifelse(investment_subsidy_scenario, # if inv. sub.
                                       (costs_replanting_material + 
                                          costs_replanting_labor) * 
                                         0.1, # only to pay 10 % - HeH: discuss
                                       (costs_replanting_material + 
                                          costs_replanting_labor))#if not: whole
 
  
  ## Timber----
  # Just if explicit allowance by UNB is given?
  # Harvest in last year: only trees of declining yield (> 50) are harvested
  aged_trees <- sum(trees_ages[timespan, ] > 50, na.rm = TRUE) # number of old trees
  tree_timber_t <- sum(vv(timber_mean_t_per_tree, 
                                timber_var_t_per_tree, aged_trees)) # per tree
  trees_timber_revenue_Eur[timespan] <- tree_timber_t * timber_price_mean_Eur_t
  
  trees_timber_labor_harvest_h[timespan] <- aged_trees * timber_labor_harvest_mean_h
  
  
  # Costs and labor orchard----
  
  ## Establishment costs----
  # Heh discuss: no vv as would be now and therefore clear prices
  # material costs 
  costs_establishment_material <- tree_establishment_material_costs_per_tree *
                                       n_trees
  
  # labor: high because digging the hole might take longer (due to roots of cherry trees)
  # discuss: this with vv
  labor_fruit_establishment_h[1] <- sum(
    vv(fruit_labor_establishment_mean_h_per_tree,
       fruit_labor_establishment_var_per_tree,
       n_trees))
  
  costs_establishment_labor <- labor_fruit_establishment_h[1] *
                                           labor_wage_Eur_per_h_brutto
  
  # total: material and labor; might be reduced by subsidy
  costs_establishment_trees_Eur[1] <- ifelse(investment_subsidy_scenario, # if inv. sub.
                                             (costs_establishment_material + 
                                              costs_establishment_labor) * 
                                              0.1, # only to pay 10 % - HeH: discuss
                                             (costs_establishment_material + 
                                                costs_establishment_labor))#if not: whole
  
  
  ## Mainteance costs----
  ## mainteance labor from loop: labor_fruit_mainteance_h
  costs_mainteance_trees_Eur <- labor_fruit_mainteance_h * 
                                    labor_wage_Eur_per_h_brutto
  
  
  ## machinery scenario
  # machinery price dependet on scenario
  machinery_price <- ifelse(machinery_scenario == TRUE,
                            fruit_price_machinery_mean_Eur/machinery_joint_participants,
                            fruit_price_machinery_mean_Eur)
  years_buying_machines <- floor(n_years/10) # every 10 years, new machinery must be bought
  costs_mainteance_trees_machinery_Eur[seq(10,
                                           length(costs_mainteance_trees_machinery_Eur),
                                           by = 10)] <- vv(machinery_price,
                                                           fruit_price_machinery_var_Eur,
                                                           n = years_buying_machines)
  # plot(costs_mainteance_trees_machinery_Eur)
  
  ## organic certificate
  costs_mainteance_certificate_Eur <- rep(tree_mainteance_costs_certificate,
                                          n_years)
  
  ## fertiliser (lime, compost, else)
  ## HeH discuss: not tree specific?
  costs_mainteance_fertiliser_Eur <- n_trees * 
    vv(tree_mainteance_costs_fertiliser_mean_per_tree,
       tree_mainteance_costs_fertiliser_var,
       n_years)
                                        
  ## supply chain costs
  costs_mainteance_supply_chain_Eur <- labor_supply_chain_building_h *
    labor_wage_Eur_per_h_brutto
  
  ### timber harvest
  costs_timber_harvest_Eur[timespan] <- trees_timber_labor_harvest_h[timespan] * 
    labor_wage_Eur_per_h_brutto

  ### total labor
  labor_trees_total_h <- labor_fruit_establishment_h +
    labor_fruit_replanting_h + 
    labor_fruit_mainteance_h +
    labor_supply_chain_building_h +
    trees_timber_labor_harvest_h
    
  ## Total costs----
  costs_mainteance_trees_total_Eur <- costs_establishment_trees_Eur +
    costs_replanting_trees_Eur +
    costs_mainteance_trees_Eur +
    costs_mainteance_trees_machinery_Eur +
    costs_mainteance_supply_chain_Eur +
    costs_mainteance_certificate_Eur
    costs_mainteance_fertiliser_Eur +
    costs_timber_harvest_Eur 
  
  # plot(costs_mainteance_trees_total_Eur)
    
  # Orchard_benefits----
  ## Subsidies----
  ## Annual GLOEZ subsidy
  tree_subidies_gloez_Eur <- rep(tree_subsidies_GLOEZ_annual_Eur_per_ha *
                          field_size_ha, n_years)

  tree_subidies_total_Eur <- tree_subidies_gloez_Eur +
    trees_subsidies_establishment_Eur + 
    trees_subsidies_dieback_Eur

  #plot(tree_subidies_total_Eur)
  

    
  ## Fruit and timber----
  benefits_orchard <- (trees_fruit_revenue_Eur +
                         trees_timber_revenue_Eur) -
                      (costs_mainteance_trees_total_Eur +
                         costs_establishment_trees_Eur)
  # plot(benefits_orchard)
  
  # Outcomes----
  ## Drought mitigation
  ## HeH discuss:
  ## idea: in years of drought = orchard benefits - hay benefits
  drought_mitigation <- events_drought * (benefits_orchard - benefits_hay)
  # plot(drought_mitigation)
  
  #NPV
  # HeH: different discount rates for agricultural and "usual" products!
  # Thus, separated NPV calculation?
  NPV_orchard <- discount(benefits_orchard, 
                          discount_rate, 
                          calculate_NPV = TRUE)
  
  NPV_hay <- discount(benefits_hay, 
                      discount_rate, 
                      calculate_NPV = TRUE)
  
  NPV_decision <- NPV_orchard - NPV_hay
  
  ## benefit-labor-ratio HeH
  ## HeH todo: benefit/costs? or benefit/labor?
  #benefit_labor_ratio_hay <- benefit_hay/labor_hay_harvest_h
  #benefit_labor_ratio <- NPV/labor_total_h
  
  
  ## motivation
  # tree_fruit_reliability
  
  return(list(NPV_hay = NPV_hay,
              NPV_orchard = NPV_orchard,
              NPV_decision = NPV_decision,
              #labor_benefit_ratio = labor_benefit_ratio,
              diebacks_per_year = trees_dieback_number,
              drought_mitigation = drought_mitigation,
              walnut_revenue_Eur = trees_fruit_revenue_Eur,
              walnut_yields_kg = trees_fruit_quantity_kg,
              walnut_price_Eur = tree_fruit_price_Eur_per_kg,
              labor_h = labor_trees_total_h,
              yield_reliability_percent = trees_fruit_reliability_percent
              ))
  
}

################################################################################
# Model run----
model_runs <- mcSimulation(estimate = as.estimate(estimate_data),
                           model_function = orchard_revitalization,
                           numberOfModelRuns = 1000,
                           functionSyntax = "plainNames")
# save results
# write.csv(model_runs, "Results/MC_orchard_revitalization_100000.csv")
# saveRDS(model_runs, "Results/MC_orchard_revitalization_100000.rds")


################################################################################
# Model results----

## NPV distributions----
plot_distributions(mcSimulation_object = model_runs,
                   "hist_simple_overlay",
                   vars = c("NPV_orchard", "NPV_hay"),
                   method = "smooth_simple_overlay",
                   #method = "boxplot_density",
                   #old_names = c("NPV_orchard", "NPV_hay"),
                   new_names = "Outcome distribution for profits") +
                        labs(subtitle = paste(
                          "Invest sub = ", investment_subsidy_scenario,
                          ", ",
                          "Joint mach. = ", scenario_joint_machinery
                        ))




## State vars----
### Yeld quantity----
plot_yields <- plot_cashflow(model_runs,
              cashflow_var_name = "walnut_yields_kg",
              x_axis_name = "Timeline of intervention",
              y_axis_name = "Yield [kg]",
              legend_name = "Quantiles (%)",
              legend_labels = c("5 to 95", "25 to 75", "median"),
              color_25_75 = "grey75",
              color_5_95 = "grey90",
              color_median = "blue",
              facet_labels = "walnut_yields_kg"
              ) +
  labs(title = "Annual walnut yields",
       subtitle = paste(
         "Scenarios: Invest subsidy ", investment_subsidy_scenario,
         ", ",
         "Joint machinery ", scenario_joint_machinery
       ))
plot_yields

### Walnut revenue----
plot_walnut_revenue <- plot_cashflow(model_runs,
                                         cashflow_var_name = "walnut_revenue_Eur",
                                         x_axis_name = "Timeline of intervention",
                                         y_axis_name = "Revenue [€]",
                                         legend_name = "Quantiles (%)",
                                         legend_labels = c("5 to 95", "25 to 75", "median"),
                                         color_25_75 = "grey75",
                                         color_5_95 = "grey90",
                                         color_median = "blue",
                                         facet_labels = "walnut_revenue_Eur") +
              labs(title = "Annual revenue by walnut fruit sellings",
                   subtitle = paste(
                     "Scenarios: Invest subsidy ", investment_subsidy_scenario,
                     ", ",
                     "Joint machinery ", scenario_joint_machinery))

plot_walnut_revenue

### Diebacks----
plot_diebacks <- plot_cashflow(model_runs,
                                     cashflow_var_name = "diebacks_per_year",
                                     x_axis_name = "Timeline of intervention",
                                     y_axis_name = "Number of trees",
                                     legend_name = "Quantiles (%)",
                                     legend_labels = c("5 to 95", "25 to 75", "median"),
                                     color_25_75 = "grey75",
                                     color_5_95 = "grey90",
                                     color_median = "blue",
                                     facet_labels = "diebacks_per_year") +
  labs(title = "Annual number of dead walnut trees",
       subtitle = paste(
         "Scenarios: Invest subsidy ", investment_subsidy_scenario,
         ", ",
         "Joint machinery ", scenario_joint_machinery))
plot_diebacks

### Labor hours----
plot_labor_h <- plot_cashflow(model_runs,
                               cashflow_var_name = "labor_h",
                               x_axis_name = "Timeline of intervention",
                               y_axis_name = "Working hours [h]",
                               legend_name = "Quantiles (%)",
                               legend_labels = c("5 to 95", "25 to 75", "median"),
                               color_25_75 = "grey75",
                               color_5_95 = "grey90",
                               color_median = "blue",
                               facet_labels = "labor_h") +
  labs(title = "Annual number of working hours for trees",
       subtitle = paste(
         "Scenarios: Invest subsidy ", investment_subsidy_scenario,
         ", ",
         "Joint machinery ", scenario_joint_machinery))
plot_labor_h

### drought mitigation----
plot_drought_mitigation <- plot_cashflow(model_runs,
                             cashflow_var_name = "drought_mitigation",
                             x_axis_name = "Timeline of intervention",
                             y_axis_name = "Surplus income by trees in drought years [€]",
                             legend_name = "Quantiles (%)",
                             legend_labels = c("5 to 95", "25 to 75", "median"),
                             color_25_75 = "grey75",
                             color_5_95 = "grey90",
                             color_median = "blue",
                             facet_labels = "drought_mitigation"
) +
  labs(title = "Financial drought risk mitigation",
       subtitle = paste(
         "Scenarios: Invest subsidy ", investment_subsidy_scenario,
         ", ",
         "Joint machinery ", scenario_joint_machinery
       ))
plot_drought_mitigation


## SA using VIP-PLS----
pls_result_AF <- plsr.mcSimulation(
  object = model_runs,
  resultName = names(model_runs$y)[3],
  ncomp = 1
)

# plot VOI
# with cut_off_line 1 and threshold of 0.5 
plot_pls(pls_result_AF,
         input_table = estimate_data,
         cut_off_line = 1,
         threshold = 0.7) + 
  labs(subtitle = paste(
    "Invest sub = ", investment_subsidy_scenario,
    ", ",
    "Joint mach. = ", scenario_joint_machinery
  ))

## VOI using EVPI----
# save as dataframe
df <- data.frame(model_runs$x, model_runs$y[1:3])

# run evpi on the NPVs (the last few in the table, starting with "NPV_decision")
EVPI <- multi_EVPI(mc = df, first_out_var = "NPV_decision")

# plot the EVPI results for the decision
plot_evpi(EVPIresults = EVPI, decision_vars = "NPV_decision")


                   