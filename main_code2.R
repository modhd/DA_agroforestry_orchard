"Second approach: first iteration.

# Tags:
HeH urgent: concrete questions for next meeting with Cory
HeH todo: next steps todo
HeH discuss: to be discussed in term paper
HeH: annotations that add helpful information
"

library(decisionSupport)
library(tidyverse)
library(readxl)
library(svglite)


# Read input----
# The input tables are divided into:
# (1) data_agency: data retrieved out of report from planning agency
# (2) data_estimates: data estimated otherwise
# (3) uncertain_variables: variables that are conceptualized (or "parameters")
input_data_agency <- read_excel("data/data_agency.xlsx", na = "NA") # bio-physical 
input_data <- read_excel("data/data_estimates.xlsx", na = "NA") # bio-physical 
input_uncertainties <- read_excel("Data/uncertain_variables.xlsx", na = "NA")  

# merge
estimate_data <- rbind(input_uncertainties, input_data_agency, input_data)

################################################################################

# Programming utilities----
## only required during programming process
## HeH discuss: implementtion verification
## "make variables" function to create point estimates as global variable
## (because when dec. function runned in mcSimulation, local variables only)
## from Whitney et al. (Lecture material DA)
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

# create global vars for programming process
#make_variables(as.estimate(input_uncertainties))
#make_variables(as.estimate(input_data))
#make_variables(as.estimate(input_data_agency))
make_variables(as.estimate(estimate_data)) # bound data

################################################################################

# Scenario input----
# HeH discuss: number trees in concept : 2 planting periods (yr1, yr 5), 
# HeH discuss: also idea that 90 % abgängig -> replanting with walnuts up to 30 % ?! 
# HeH discuss: preserved habitat: at least 100 trees
# discount_rate <- 1.05 # HeH discuss: differentiate between agriculture and else!!

## Machinery scenario: 
# co-operative investment in expensive machinery; 
# divided equally among participants
machinery_joint_scenario <- FALSE
machinery_joint_participants <- 10

## Investment subsidy scenario: 
# Part of the investment costs are covered, only a proportion is still to be paid
investment_subsidy_scenario <- FALSE #HeH investment
proportion_to_be_paid <- 0.1 # 10 % of costs must be payed

################################################################################

# Decision function----
orchard_revitalization <- function(){
  
  # Variable declarations----
  # time span of simulation
  n_years <- timespan

  # Preparation of empty vectors and lists for calculations
  # NAs whether the vector is filled (as control if each function runs)
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
  

  # tree age dataframe: col = tree ID, row = simulation year
  trees_ages <- data.frame(matrix(0, nrow = n_years, ncol = n_trees)) %>%
    set_names(paste0("tree", 1:n_trees)) %>%
    mutate(across(everything(), 
                  ~ replace(., row_number() == 1, 1))) # 1rst row (year): all to 1
  
  
  # Risk occurrences----
  ## Probabilities of each risk to occur,
  ## their damage (respectively for diebacks, quality and quantity reduction
  ## follows later)
  
  ## drought
  events_drought <- chance_event(risk_yearly_drought,
                                 value_if = 1,
                                 value_if_not = 0,
                                 n = n_years)
  # plot(events_drought)

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
  # plot(events_disease)
  

  
  # Hay benefit----
  # HeH: discuss - too humid also bad, bud not included explicitly 
  # Heh: discuss - machinery costs not included as usually used in farm management

  # potential yield
  hay_yield_t_max <- vv(hay_yield_mean_t_ha * field_size_ha, 
                        hay_yield_var, 
                        n_years) |> 
    pmax(0) 
  # reductions by drought (HeH discussed: other risks not addressed directly)
  hay_damage_t <- events_drought * vv(
                        risk_drought_hay_decrease_mean,
                        risk_drought_hay_decrease_var,
                        n_years) |> 
    pmax(0) |> 
    pmin(1)
  
  # actual hay yield
  hay_yield_t <- hay_yield_t_max * (1 - hay_damage_t) 
  
  # price
  # HeH discuss: price fluctuates with average hay availability (reflected by the
  # hay yielded - much yield, on average much hay on market, low prices)
  # aims to be similar to walnut fruit price concept (market capacity)
  # Threshold: uncert_hay_good_market_capacity

  
  hay_price_Eur_t <- case_when(
    # if a lot hay available on market - low prices (should me most times)
    hay_yield_t > uncert_hay_good_market_capacity ~ 
      vv(hay_price_bad_market_mean_Eur_t, 
         hay_price_var, 
         n_years),
    # if hay rare in a year - high prices
    hay_yield_t <= uncert_hay_good_market_capacity ~ 
      vv(hay_price_good_market_mean_Eur_t, 
         hay_price_var, 
         n_years),
  )|> 
    pmax(0) 
  
  # revenue
  hay_yield_revenue_Eur <- hay_price_Eur_t * hay_yield_t
  
  # hay costs
  # No machinery costs 
  
  # Labor costs: only if hay grows (no drought) - otherwise, no mainteance costs
  # HeH todo: will change if risk damage included!
  labor_mainteance_hay_h <- vv(hay_labor_harvest_mean_h_ha * field_size_ha, 
                               hay_labor_harvest_var, 
                               n_years) |> 
    pmax(0)
  
  costs_mainteance_hay_Eur <- labor_mainteance_hay_h * 
                              labor_wage_Eur_per_h_brutto
  
  # hay benefit
  benefits_hay <- hay_yield_revenue_Eur - 
                              costs_mainteance_hay_Eur
  # plot(benefits_hay)
  
  # Orchard----
  
  ## Fruit quality and quantity----
  # Maximum yield dependent on tree ages in all years
  # modelled as gompertz curve
  # with estimates from agency report
  # HeH discuss: max yield is estimate - only risk-reduced yields are documented!
  fruit_yield_per_tree_and_age <- gompertz_yield(
    max_harvest = fruit_yield_mean_kg_per_tree,
    time_to_first_yield_estimate = fruit_time_to_first_yield_est,
    time_to_second_yield_estimate = fruit_time_to_first_yield_est+1,
    first_yield_estimate_percent = fruit_first_yield_percent,
    second_yield_estimate_percent = fruit_second_yield_percent,
    n_years = n_years,
    var_CV = fruit_yield_var_per_tree,
    no_yield_before_first_estimate = TRUE
  )
  # plot(fruit_yield_per_tree_and_age)

  
  ## Influence of risks----
  
  ### Tree vulnerability----
  # HeH discuss - Tree vulnerability interpreted as factor: how much 
  # will the tree respond to a potential damage by risks?
  # Depends on age and unknown vulnerability factors
  # 90 % vulnerability == 90 % response to risks

  
  #### age influence
  # Assuming it as parameterized quadratic function (curve with parameters p1, p2)
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
  # HeH duscuss: lower variance to clarify the role
  tree_vulnerability_unknown <- vv(uncert_tree_vulnerability_mean,
                                   uncert_tree_vulnerability_var,
                                   n = n_years,
                                   relative_trend = 0.5) |> 
    pmax(0) |> 
    pmin(1)
  
  # plot(tree_vulnerability_unknown)

  ### Final vulnerability: add the age influence and the placeholder risk
  # Maximized to 1 (100 % vulnerability = 100 % response to risks)
  tree_vulnerability_per_age <- pmin(tree_vulnerability_unknown +
                                       tree_vulnerability_age_influence,
                                     1) 
  # plot(tree_vulnerability_per_age) 

  ### Damages----

  ### Risks for dieback
  # Depends on disease, or drought
  # "How high is the risk damage probability that could lead to dieback?"
  # Is processed by chance_event later
  # HeH: discuss - does not respect if previous years have been dry etc!
  risk_sum_causing_diebacks <- {events_disease * vv(risk_disease_dieback_mean, # HeH discuss: highly pest dependent
                                           risk_disease_dieback_var,
                                           n_years) +
    events_drought * vv(risk_drought_dieback_mean, # Discuss: low as we water?! No/yes?!?!
                        risk_drought_dieback_var,
                        n_years) } |> 
    pmax(0) |> 
    pmin(1)
  # plot(risk_sum_causing_diebacks)
  
  
  ## Risks for yield reduction
  ## Depends on frost and disease
  ## damage: "how much yield loss if risk occurs"
  risk_sum_yield_reduction <- { events_disease * vv(risk_disease_yield_red_mean, # different pests
                                                  risk_disease_yield_red_var,
                                                  n_years,
                                                  relative_trend = 0.1) +
    events_frost * vv(risk_frost_yield_red_mean, # HeH dicuss: frost hardiness varieties?!
                      risk_frost_yield_red_var,
                      n_years, 
                      relative_trend = 0.2) } |> 
    pmax(0) |> 
    pmin(1)
    
  # plot(risk_sum_yield_reduction)
  
  ## risks for quality reduction
  # Depends on disease, drought 
  # damage: "how much quality loss when fully affected?"
  risk_sum_quality_reduction <- { events_disease * vv(risk_disease_quali_red_mean,# different diseases
                                                    risk_disease_quali_red_var,
                                                    n_years,
                                                    relative_trend = 0.1) +
    events_drought * vv(risk_drought_quali_red_mean, # HeH dicuss: frost hardiness varieties?!
                        risk_drought_quali_red_var,
                        n_years,
                        relative_trend = 0.2) } |> 
    pmax(0) |> 
    pmin(1)
  
  
  # plot(risk_sum_quality_reduction)
  
  ## Supply chain----
  # Principle: labor into networking to build a local supply chain.
  # If enough effort to build good supply chain
  # (exceeding the labor needed depending on regional conditions),
  # the amount of directly marketable walnuts increases.
  
  labor_supply_chain_building_h <- vv(	
                                supply_chain_invest_mean_h,
                                supply_chain_invest_var,
                                n_years,
                                relative_trend = -0.5) |>  # HeH assume less work once the chain is build
                    pmax(0)
  
    #plot(labor_supply_chain_building_h) 
                              
  labor_needed_for_good_market_h <- vv(uncert_invest_until_good_market_mean_h,
                                     uncert_invest_until_good_market_var,
                                     n_years,
                                     relative_trend = -0.5)# same trend as invested
    pmax(0) 
  
  # calculate the direct marketing capacities
  direct_marketing_maximum_kg <- ifelse(
    # check if in this year, enough labor is invested to build supply chain
    labor_supply_chain_building_h >= labor_needed_for_good_market_h,
    uncert_good_direct_market_maximum_sells_kg, # if yes, good market - higher sells
    uncert_poor_direct_market_maximum_sells_kg) # if not, bad market - lower sells


  ## Yearly loops----
  # On individual, not population level! 
  # (HeH discuss: general effect on ageing pop is viewed on ind. level (discuss that))
  # This loop makes annual steps and first inspects each tree individually.
  # Dependent on the year's risk and the tree's age, trees might die. 
  # This causes replantings; also the tree age is reset and the tree takes time
  # to mature and give yield again.
  # After that, maintenance costs that are different for juvenile, maturing and
  # mature trees are calculated for that year.
  # Subsequently, depending on the tree's age: yield quality and quantity summed
  # up for all trees.
  # In the end, the resulting yield reliability and market 
  # price (depends on fruit quali and quanti) are calculated.
  
  for (i in 1:n_years) {
    ### Diebacks----
    # HeH discuss: include parameter for motivation_influence? i.e. watering to prevent dieback
    
    # This section gives back a vector with 0 when dieback and 1 if survives
    trees_surviving <- trees_ages[i, ] %>% # pick each tree iteratively
      map_dbl(~ {
        age <- .x   # extract the individual tree age
        vulnerability <- tree_vulnerability_per_age[age] # extract respective vulnerability
        
        # Dependent on year's risks and the age-specific vulnerability: 
        # Dieback? -> 1 if not (survive), 0 if no (dieback)
        chance_event(risk_sum_causing_diebacks[i] * vulnerability, 
                     value_if = 0, value_if_not = 1)  
      })
    
    
    # Dieback numbers (caution, 0 == death!)
    ## which is, the initial number of trees minus the ones surviving
    dieback_numbers <- n_trees - sum(trees_surviving)
    
    
    ## store for output
    trees_dieback_number[i] <- dieback_numbers
    
    ## update tree ages for the following year:
    # Trees surviving: "null" were trees die, then all together ageing 1 year
    trees_ages[i+1,] <- (trees_ages[i,] * trees_surviving)+ 1

    ## Tree age influences----
    # On quantity - per tree individually
    trees_fruit_quantity_kg[i] <- 
      trees_ages[i, ] %>% # take age of each individual
      unlist() %>%
      map_dbl(~ {
        tree_vulnerability_percent <- tree_vulnerability_per_age[.x] # Extract age dep. vulnerability 
       risks_yield_reduction_percent <- risk_sum_yield_reduction[i]  # "how much yield % loss if risk occurs"
       
      # Maximum yield reduced by risk damage dependent on vulnerability
      fruit_yield_per_tree_and_age[.x] * ( 1 - (risks_yield_reduction_percent * 
                                         tree_vulnerability_percent))
      }) %>%
      sum() # HeH discuss: not tree-specific stored?
    
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
       median() # HeH discuss: median
      
      
      ## Maintenance----
      
      # Age-specific labor efforts
      # Planting: hours are calculated later (more efficient)
      labor_fruit_mainteance_h[i] <- trees_ages[i, ] %>%
        unlist() %>%
        map_dbl(~ {
          age <- .x
          
          # if juvenile: 
          if (age <= 5) {
            # pruning + management (check for diseases, clearing surrounding area)
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
              ifelse(i %% 4 == 0, # every 4 years (dividable by 4)
              labor_fruit_pruning_mature_h,
              0) # else no pruning required
          }

        }) %>%
        sum()

      
      ## Walnut price (supply chain) ----
      ## Idea:
      ## If too much kg to sell by themselves (maximum capacity of direct market)
      ## or quali too bad: wholesale.
      ## HeH discuss - we assume that a farmer can only to either - sell directly
      # or to wholesail. Selling surpluses form direct market would not work
      # as too low amounts and not reliable enough for wholesale.
      ## HeH discuss: lower quality for animal feedings.
      
      if (trees_fruit_quantity_kg[i] > direct_marketing_maximum_kg[i] ||  
          trees_fruit_quality_median_percent[i] < uncert_minimum_quali_for_direct_percent) {
        
        tree_fruit_price_Eur_per_kg[i] <- walnut_price_wholesale_Eur_per_kg
      
      ##  If within local market capacity and in sufficient quality: direct
      } else if (trees_fruit_quantity_kg[i] <= direct_marketing_maximum_kg[i] && 
          trees_fruit_quality_median_percent[i] >= uncert_minimum_quali_for_direct_percent) {
        
        tree_fruit_price_Eur_per_kg[i] <- walnut_price_direct_Eur_per_kg

      ## Test if no mistake in logic 
      } else { 
        # print("mistake in pricing logic.") # HeH discuss: implement vali
        tree_fruit_price_Eur_per_kg[i] <- walnut_price_direct_Eur_per_kg }
      
      
      ## Fruit revenue----
      trees_fruit_revenue_Eur[i] <- trees_fruit_quantity_kg[i] * 
        tree_fruit_price_Eur_per_kg[i]
      
      ## Yield reliability----
      # Calculates yield reliability - HeH: discuss idea for an indicator:
      # each year in percent (0 if no yield): 
      # (1) calculates the standard deviation of the difference of maximum yield
      #     minus actual yield up to current yr (which is, sd of the yield damage)
      # (2) calculates the share of this sd from the mean revenue
      # of previous harvests = yield_unreliability [kg/Eur]
      # (2) yield_reliability = 100 - yield_unreliability [kg/Eur]
      trees_fruit_reliability_percent[i] <- ifelse(
        trees_fruit_revenue_Eur[i] > 0,
        100-(100*sd(fruit_yield_per_tree_and_age[1:i] - trees_fruit_quantity_kg[1:i], na.rm = TRUE)
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
  #     ylim = c(0,7000))
  # lines(fruit_yield_per_tree_and_age* n_trees, col = "red")
  # plot(tree_fruit_price_Eur_per_kg)


   
  ## Subsidies----
  ## Annual GLOEZ subsidy
  tree_subidies_gloez_Eur <- rep(tree_subsidies_GLOEZ_annual_Eur_per_ha *
                                   field_size_ha, n_years)
  
  ## Other subsidies: start with tree planting; 
  # lower subsidies in the 4 following years
  
  # HeH discuss: Option to insert risk of subsidy declines: if (scenario_subsidies_lost = FALSE)
  # HeH discuss: Not included case - replanted tree dies back as well (sub would be cancelled!)
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
  trees_subsidies_establishment_Eur <- calculate_subsequent_subs(
                              trees_subsidies_establishment_Eur, 
                              trees_planted,
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
  
  
  # Costs and labor orchard----
  
  ## Replanting costs---- 
  # material costs 
  costs_replanting_material <- tree_establishment_material_costs_per_tree *
    trees_dieback_number
  
  # labor: less than in establishment, depends on number of tree that died back
  labor_fruit_replanting_h <- vv(labor_fruit_replanting_mean_h_per_tree,
                                 labor_fruit_replanting_var_per_tree,
                                 n_years) * trees_dieback_number |> 
      pmax(0)

  costs_replanting_labor <- labor_fruit_replanting_h *
    labor_wage_Eur_per_h_brutto
  
  # Total costs: material and labor; depends on invest. subsidy scenario
  if (investment_subsidy_scenario) {
    costs_replanting_trees_Eur <- (costs_replanting_material + costs_replanting_labor) *
      proportion_to_be_paid
  } else {
    costs_replanting_trees_Eur <- costs_replanting_material + costs_replanting_labor
  }
  
  ## Establishment costs----
  # Heh discuss: no vv as would be now and therefore clear prices
  # material costs 
  costs_establishment_material <- tree_establishment_material_costs_per_tree *
                                       n_trees
  
  # labor: high because digging the hole might take longer (due to roots of cherry trees)
  # discuss: this with vv, tree specific
  labor_fruit_establishment_h[1] <- sum(
    vv(tree_labor_establishment_mean_h_per_tree,
       tree_labor_establishment_var_per_tree,
       n_trees)) |> 
    pmax(0)
  
  costs_establishment_labor <- labor_fruit_establishment_h[1] *
                                           labor_wage_Eur_per_h_brutto
  
  # total: material and labor; might be reduced by subsidy
  costs_establishment_trees_Eur[1] <- ifelse(investment_subsidy_scenario, # if inv. sub.
                                             (costs_establishment_material + 
                                              costs_establishment_labor) * 
                                              proportion_to_be_paid, # only to pay 10 % - HeH: discuss
                                             (costs_establishment_material + 
                                                costs_establishment_labor))#if not: whole
  
  
  ## Mainteance costs----
  ## mainteance labor from loop: labor_fruit_mainteance_h
  costs_mainteance_trees_Eur <- labor_fruit_mainteance_h * 
                                    labor_wage_Eur_per_h_brutto
  
  
  ## machinery scenario
  # machinery price dependet on scenario
  machinery_price <- ifelse(machinery_joint_scenario == TRUE,
                            fruit_price_machinery_mean_Eur/machinery_joint_participants,
                            fruit_price_machinery_mean_Eur)
  years_buying_machines <- floor(n_years/10) # every 10 years, new machinery must be bought
  costs_mainteance_trees_machinery_Eur[seq(10,
                                           length(costs_mainteance_trees_machinery_Eur),
                                           by = 10)] <- vv(machinery_price,
                                                           fruit_price_machinery_var_Eur,
                                                           n = years_buying_machines) |> pmax(0)
  
  # plot(costs_mainteance_trees_machinery_Eur)
  
  ## organic certificate
  costs_mainteance_certificate_Eur <- rep(tree_mainteance_costs_certificate,
                                          n_years)
  
  ## fertiliser (lime, compost, else)
  ## HeH discuss: not tree specific?
  costs_mainteance_fertiliser_Eur <- n_trees * 
    vv(tree_mainteance_costs_fertiliser_mean_per_tree,
       tree_mainteance_costs_fertiliser_var,
       n_years) |> pmax(0)
                                        
  ## supply chain costs
  costs_mainteance_supply_chain_Eur <- labor_supply_chain_building_h *
    labor_wage_Eur_per_h_brutto

  
  # plot(costs_mainteance_trees_total_Eur)
  
  ## Timber----
  # Principle: Harvest in last year: only trees of declining yield (> 50) harvested
  # HeH discuss: this means  scenarios with no timber => realistic
  aged_trees <- sum(trees_ages[timespan, ] > 50, na.rm = TRUE) # number of old trees
  tree_timber_harvest_m3 <- vv(timber_yield_mean_m3_per_tree, 
                           timber_yield_var, aged_trees) |> 
    pmax(0) |> 
    sum()
  # HeH discuss: per tree
  trees_timber_revenue_Eur[timespan] <- tree_timber_harvest_m3 * timber_price_Eur_m3
  
  trees_timber_labor_harvest_h[timespan] <- vv(
    timber_labor_harvest_mean_h,
    timber_labor_harvest_var,
    aged_trees) |> 
    pmax(0) |> 
    sum()
  
  ### timber harvest
  costs_timber_harvest_Eur[timespan] <- trees_timber_labor_harvest_h[timespan] * 
    labor_wage_Eur_per_h_brutto
    
  
  ## Total labor and costs----
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
    costs_mainteance_certificate_Eur +
    costs_mainteance_fertiliser_Eur +
    costs_timber_harvest_Eur 
  
  # Benefits----
  
  ## Subsidies----
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
  
  # NPV
  # HeH: different discount rates for agricultural and "usual" products!
  # Thus, separated NPV calculation?
  NPV_orchard <- discount(benefits_orchard, 
                          discount_rate, 
                          calculate_NPV = TRUE)
  
  NPV_hay <- discount(benefits_hay, 
                      discount_rate, 
                      calculate_NPV = TRUE)
  
  NPV_decision <- NPV_orchard - NPV_hay
  
  # Heh discuss: "holistic elements" that shall be included next:
  
  ## benefit-labor-ratio HeH
  ## HeH todo: benefit/costs? or benefit/labor?
  #benefit_labor_ratio_hay <- benefit_hay/labor_hay_harvest_h
  #benefit_labor_ratio <- NPV/labor_total_h

  ## motivation, knowledge --> influence on vulnerbailities of trees?!

  return(list(NPV_hay = NPV_hay,
              NPV_orchard = NPV_orchard,
              NPV_decision = NPV_decision,
              #labor_benefit_ratio = labor_benefit_ratio,
              dieback_percentage_per_year = (trees_dieback_number*100)/n_trees,
              drought_mitigation = drought_mitigation,
              walnut_revenue_Eur = trees_fruit_revenue_Eur,
              walnut_yields_kg = trees_fruit_quantity_kg,
              walnut_price_Eur = tree_fruit_price_Eur_per_kg,
              labor_h = labor_trees_total_h,
              yield_reliability_percent = trees_fruit_reliability_percent,
              hay_yields_t = hay_yield_t,
              hay_prices_Eur = hay_price_Eur_t
              ))
  
}

################################################################################
# Model run----
# Marcov Chain Monte Carlo Simulation
model_runs <- mcSimulation(estimate = as.estimate(estimate_data),
                           model_function = orchard_revitalization,
                           numberOfModelRuns = 10000,
                           functionSyntax = "plainNames")
# save results
# write.csv(model_runs, "Results/MC_orchard_revitalization_100000.csv")
# saveRDS(model_runs, "Results/MC_orchard_revitalization_100000.rds")


################################################################################
# Model results----

## NPV distributions----
plot_NPV <- plot_distributions(mcSimulation_object = model_runs,
                   "hist_simple_overlay",
                   vars = "NPV_decision",
                   #method = "smooth_simple_overlay",
                   method = "boxplot_density",
                   #old_names = c("NPV_orchard", "NPV_hay"),
                   new_names = "Outcome distribution for profits") +
                        labs(
                          title = "Net Present Value of the decision after 55 years",
                          subtitle = paste(
                          "Investition subsidy = ", investment_subsidy_scenario,
                          ", ",
                          "Joint machinery = ", machinery_joint_scenario
                        )) + 
  theme(
    axis.title = element_text(size = 13),      # X and Y axis labels
    axis.text = element_text(size = 10),       # X and Y axis tick labels
    legend.text = element_text(size = 10),     # Legend text
    legend.title = element_text(size = 12),    # Legend title
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )
plot_NPV
# store
#ggsave("Results/plot_NPV.svg", width = 9, plot = plot_NPV)

## Output vars----
### Yield----
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
  labs(title = "Summed annual walnut yields") + 
  theme(
    axis.title = element_text(size = 13),      # X and Y axis labels
    axis.text = element_text(size = 10),       # X and Y axis tick labels
    legend.text = element_text(size = 10),     # Legend text
    legend.title = element_text(size = 12),    # Legend title
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )

plot_yields
#ggsave("Results/plot_walnut_sum_yields.svg", width = 9, plot = plot_yields)

plot_yield_reliabilities <- plot_cashflow(model_runs,
                             cashflow_var_name = "yield_reliability_percent",
                             x_axis_name = "Timeline of intervention",
                             y_axis_name = "Reliability [kg/Eur]",
                             legend_name = "Quantiles (%)",
                             legend_labels = c("5 to 95", "25 to 75", "median"),
                             color_25_75 = "grey75",
                             color_5_95 = "grey90",
                             color_median = "blue",
                             facet_labels = "yield_reliability_percent"
) +
  labs(title = "Walnut yield reliabilities",
       subtitle = "Indicator dependent on annual quantity and revenues") + 
  theme(
    axis.title = element_text(size = 13),      # X and Y axis labels
    axis.text = element_text(size = 10),       # X and Y axis tick labels
    legend.text = element_text(size = 10),     # Legend text
    legend.title = element_text(size = 12),    # Legend title
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )

plot_yield_reliabilities
#ggsave("Results/plot_yield_reliability.svg", width = 9, plot = plot_yield_reliability)


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
              labs(title = "Annual revenues by walnut fruit sellings",
                   subtitle = paste(
                     "Scenarios: Invest subsidy ", investment_subsidy_scenario,
                     ", ",
                     "Joint machinery ", machinery_joint_scenario)) +
  theme(
    axis.title = element_text(size = 13),      # X and Y axis labels
    axis.text = element_text(size = 10),       # X and Y axis tick labels
    legend.text = element_text(size = 10),     # Legend text
    legend.title = element_text(size = 12),    # Legend title
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )

plot_walnut_revenue
#ggsave("Results/plot_walnut_revenue.svg", width = 9, plot = plot_walnut_revenue)


## walnut prices
plot_walnut_prices <- plot_cashflow(model_runs,
                                     cashflow_var_name = "walnut_price_Eur",
                                     x_axis_name = "Timeline of intervention",
                                     y_axis_name = "Price [€/kg]",
                                     legend_name = "Quantiles (%)",
                                     legend_labels = c("5 to 95", "25 to 75", "median"),
                                     color_25_75 = "grey75",
                                     color_5_95 = "grey90",
                                     color_median = "blue",
                                     facet_labels = "walnut_price_Eur") +
  labs(title = "Annual walnut prices") + 
  theme(
    axis.title = element_text(size = 13),      # X and Y axis labels
    axis.text = element_text(size = 10),       # X and Y axis tick labels
    legend.text = element_text(size = 10),     # Legend text
    legend.title = element_text(size = 12),    # Legend title
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )

plot_walnut_prices
#ggsave("Results/plot_walnut_prices.svg", width = 9, plot = plot_yields)


### Diebacks----
plot_diebacks <- plot_cashflow(model_runs,
                                     cashflow_var_name = "dieback_percentage_per_year",
                                     x_axis_name = "Timeline of intervention",
                                     y_axis_name = "Dead trees [%]",
                                     legend_name = "Quantiles (%)",
                                     legend_labels = c("5 to 95", "25 to 75", "median"),
                                     color_25_75 = "grey75",
                                     color_5_95 = "grey90",
                                     color_median = "blue",
                                     facet_labels = "dieback_percentage_per_year") +
  labs(title = "Annual proportions of dying walnut trees") + 
  theme(
    axis.title = element_text(size = 13),      # X and Y axis labels
    axis.text = element_text(size = 10),       # X and Y axis tick labels
    legend.text = element_text(size = 10),     # Legend text
    legend.title = element_text(size = 12),    # Legend title
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )

plot_diebacks
#ggsave("Results/plot_diebacks.svg", width = 9, plot = plot_diebacks)


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
  labs(title = "Annual hours worked on trees") + 
  theme(
    axis.title = element_text(size = 13),      # X and Y axis labels
    axis.text = element_text(size = 10),       # X and Y axis tick labels
    legend.text = element_text(size = 10),     # Legend text
    legend.title = element_text(size = 12),    # Legend title
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )

plot_labor_h
#ggsave("Results/plot_labor_h.svg", width = 9, plot = plot_labor_h)

### Hay----
plot_hay_yields <- plot_cashflow(model_runs,
                              cashflow_var_name = "hay_yields_t",
                              x_axis_name = "Timeline of intervention",
                              y_axis_name = "Yields [t]",
                              legend_name = "Quantiles (%)",
                              legend_labels = c("5 to 95", "25 to 75", "median"),
                              color_25_75 = "grey75",
                              color_5_95 = "grey90",
                              color_median = "blue",
                              facet_labels = "hay_yields_t") +
  labs(title = "Annual hay yields") + 
  theme(
    axis.title = element_text(size = 13),      # X and Y axis labels
    axis.text = element_text(size = 10),       # X and Y axis tick labels
    legend.text = element_text(size = 10),     # Legend text
    legend.title = element_text(size = 12),    # Legend title
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )

plot_hay_yields
#ggsave("Results/plot_hay_yields.svg", width = 9, plot = plot_hay_yields)

# Hay prices 
plot_hay_prices <- plot_cashflow(model_runs,
                                 cashflow_var_name = "hay_prices_Eur",
                                 x_axis_name = "Timeline of intervention",
                                 y_axis_name = "Price [Eur/t]",
                                 legend_name = "Quantiles (%)",
                                 legend_labels = c("5 to 95", "25 to 75", "median"),
                                 color_25_75 = "grey75",
                                 color_5_95 = "grey90",
                                 color_median = "blue",
                                 facet_labels = "hay_prices_Eur") +
  labs(title = "Annual hay selling prices") + 
  theme(
    axis.title = element_text(size = 13),      # X and Y axis labels
    axis.text = element_text(size = 10),       # X and Y axis tick labels
    legend.text = element_text(size = 10),     # Legend text
    legend.title = element_text(size = 12),    # Legend title
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )

plot_hay_prices
#ggsave("Results/plot_hay_prices.svg", width = 9, plot = plot_hay_prices)


### Drought mitigation----
plot_drought_mitigation <- plot_cashflow(model_runs,
                             cashflow_var_name = "drought_mitigation",
                             x_axis_name = "Timeline of intervention",
                             y_axis_name = "Additional income [€]",
                             legend_name = "Quantiles (%)",
                             legend_labels = c("5 to 95", "25 to 75", "median"),
                             color_25_75 = "grey75",
                             color_5_95 = "grey90",
                             color_median = "blue",
                             facet_labels = "drought_mitigation"
) +
  labs(title = "Financial drought mitigation - additional income by trees",
       subtitle = paste(
         "Scenarios: Invest subsidy ", investment_subsidy_scenario,
         ", ",
         "Joint machinery ", machinery_joint_scenario
       )) + 
  theme(
    axis.title = element_text(size = 13),      # X and Y axis labels
    axis.text = element_text(size = 10),       # X and Y axis tick labels
    legend.text = element_text(size = 10),     # Legend text
    legend.title = element_text(size = 12),    # Legend title
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )

plot_drought_mitigation
#ggsave("Results/plot_drough_mitigation.svg", width = 9, plot = plot_drought_mitigation)


## SA using VIP-PLS----
pls_result_AF <- plsr.mcSimulation(
  object = model_runs,
  resultName = names(model_runs$y)[3],
  ncomp = 1
)

# plot VOI
# with cut_off_line 1 and threshold of 0.5 
plot_voi <- plot_pls(pls_result_AF,
         input_table = estimate_data,
         cut_off_line = 1,
         threshold = 1) + 
  labs(subtitle = paste(
    "Invest sub = ", investment_subsidy_scenario,
    ", ",
    "Joint mach. = ", machinery_joint_scenario
  ))
plot_voi
#ggsave("Results/1_plot_voi.svg", width = 9, plot = plot_voi)


## VOI using EVPI----
# prepare data as dataframe, run evpi analysis
df <- data.frame(model_runs$x, model_runs$y[1:3])
EVPI <- multi_EVPI(mc = df, first_out_var = "NPV_hay")

# plot the EVPI results for the decision
plot_evpi(EVPIresults = EVPI, decision_vars = "NPV_decision")

plot_evpi
#ggsave("Results/1_plot_evpi.svg", width = 9, plot = plot_evpi)


                   