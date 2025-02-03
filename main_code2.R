"Second approach: first iteration."

library(decisionSupport)
library(tidyverse)
library(readxl)


# Read input tables----
input_uncertainities <- read_excel("Data/uncertain_variables.xlsx") 
input_data <- read_excel("data/data_estimates.xlsx") # bio-physical & economic data
estimate_data <- rbind(input_uncertainities, input_data)
# Frame_input <- read.csv() # political scenario probabilities


# Model development utilities
## "make variables" function
# gives point estimates (for code testing)
# from Whitney et al. (Lecture material DA)
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_uncertainities))
make_variables(as.estimate(input_data))


# Scenario input
# HeH interactive input?
#scenario_investition_subsidy <- FALSE
#scenario_joint_machinery <- TRUE
timespan <- 55
n_trees <- 15
field_size_ha <- 1.3
discount_rate <- 2 # HeH: differentiate between agriculture and else!!
machinery_scenario <- TRUE
machinery_joint_participants <- 10

################################################################################

# decision function----
orchard_revitalization <- function(){
  
  # variable declarations----
  # time span of simulation
  n_years <- timespan

  # preparation of empty vectors and lists for calculations
  #late_frost_event<-rep(NA, n_years)
  tree_dieback_number <- rep(0, n_years)
  tree_death_yesno <- rep(NA, n_trees)
  fruit_yield_max_kg <- rep(0, n_years)
  tree_fruit_quantity_kg <- rep(NA, n_years)
  tree_fruit_quality_percent <- rep(NA, n_years)
  costs_establishment_trees <- rep(0, n_years)
  costs_mainteance_trees_machinery_Eur <- rep(0, n_years)
  costs_establishment_hay_Eur <- rep(0, n_years)
  walnut_price <- rep(0, n_years)
  tree_subsidies_Eur <- rep(0, n_years)
  tree_timber_revenue_Eur <- rep(0, n_years)
  tree_timber_labor_harvest_h <- rep(0, n_years)
  costs_timber_harvest_Eur <- rep(0, n_years)
  costs_establishment_hay <- rep(0, n_years)
  
  
  tree_ages <- data.frame(matrix(0, nrow = n_years, ncol = n_trees)) %>%
    set_names(paste0("tree", 1:n_trees)) %>%
    mutate(across(everything(), ~ replace(., row_number() == 1, 1))) # first row (year): all to 1
  
  # risk occurences----
  ## drought
  events_drought <- chance_event(risk_yearly_drought,
                                 # value_if = (vv(risk_yearly_drought_decrease_mean,
                                 #                var_CV = risk_yearly_drought_decrease_var,
                                 #                n = n_years,
                                 #                lower_limit = 0.1,
                                 #                relative_trend = 0.2)),
                                 value_if= 1,
                                 value_if_not =0,
                                 n = n_years)
  
  #plot(events_drought)
  #hist(events_drought)
  
  ## spring frost
  events_frost <- chance_event(risk_spring_frost,
                                # value_if = round(vv(risk_spring_frost_decrease_mean,
                                #                     var_CV = risk_spring_frost_decrease_var,
                                #                     n = n_years,
                                #                    lower_limit = 1,
                                #                    relative_trend = 0.05)),
                               value_if = 1,
                               #value_if_not = rep(0, n_years),
                               value_if_not = 0,
                                n = n_years )
  
  ## disease/pests
  events_disease <- chance_event(risk_disease,
                                 # value_if = (vv(risk_disease_decrease_mean,
                                 #                var_CV = risk_disease_dicrease_var,
                                 #                n = n_years,
                                 #                lower_limit = 0.1,
                                 #                relative_trend = 0.4)),
                                 value_if = 1,
                                 #value_if_not = rep(0, n_years),
                                 value_if_not = 0,
                                 n = n_years)
  
  #plot(events_disease)
  

  # summarise risks----
  ## risks for dieback
  # depends on disease, or drought in first and last years
  risk_sum_diebacks <- events_disease * vv(risk_disease_dieback_mean,
                                           risk_disease_dieback_var,
                                               n_years) +
                          events_drought * vv(risk_drought_dieback_mean,
                                              risk_drought_dieback_var,
                                              n_years)
  #plot(risk_sum_diebacks)
  
  
  ## risks for yield reduction
  ## depends on frost and disease
  risk_sum_yield_reduction <- events_disease * vv(risk_disease_yield_red_mean,
                                                  risk_disease_yield_red_var,
                                                  n_years) +
                                  events_frost * vv(risk_frost_yield_red_mean,
                                                    risk_frost_yield_red_var,
                                                    n_years)

  #plot(risk_sum_yield_reduction)
  
  ## risks for quality reduction
  # depends on disease, drought  
  risk_sum_quality_reduction <- events_disease * vv(risk_disease_quali_red_mean,
                                                    risk_disease_quali_red_var,
                                                    n_years) +
                                    events_drought * vv(risk_frost_quali_red_mean,
                                                   risk_frost_quali_red_var,
                                                   n_years)
                                                   
  
  #plot(risk_sum_quality_reduction)
  
  # hay benefit----
  # hay yield
  # HeH: discuss - too humid also bad, but that would not affect trees so leave it out
  hay_yield_t_max <- vv(hay_mean_t, hay_var_t, n_years)
  hay_yield_t <- hay_yield_t_max*events_drought # *events_uncert_risks
  hay_yield_revenue_Eur <- 2*hay_yield_t*(vv(hay_price_mean_Eur_t, 
                                             hay_price_var_Eur_t, 
                                             n_years)) # 2 yields per year, HeH discuss: yield dependent
  
  # hay costs
  costs_establishment_hay_Eur[1] <- hay_costs_establishment_Eur
  labor_mainteance_hay_h <- vv(hay_labor_harvest_mean_h, hay_labor_harvest_var_h,
                               n_years)
  costs_mainteance_hay_Eur <- labor_mainteance_hay_h * labor_wage_Eur_per_h_brutto
  
  # hay benefit
  benefits_hay <- hay_yield_revenue_Eur - 
    costs_establishment_hay - 
    costs_mainteance_hay_Eur
  
  # orchard----
  
  ## fruit quality and quantity----
  # maximum yield dependent on tree ages in all years
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
  #plot(fruit_yield_per_age)
  
  ### influence of risks---- 
  # tree age influence: assuming it as parameterized quadratic function
  # p1 and p2 are very uncertain -> identify whether they need more detail
  year <- 1:timespan
  tree_age_influence <- (uncert_tree_parameter_age_2*0.0001)*
    (year - uncert_tree_parameter_age_1)^2
  #plot(tree_age_influence)
  
  tree_vulnerability_per_age <- uncert_tree_vulnerability * tree_age_influence
  #plot(tree_vulnerability_per_age)
  
  
  ## yearly loops----
  for (i in 1:n_years) {
    
    ### diebacks----
    # HeH include parameter for motivation_influence?
    # This function gives back a vector with 0 when dieback and 1 if not
    tree_deaths_yesno <- tree_ages[i, ] %>%
      map_dbl(~ {
        age <- .x  # Get tree age
        vulnerability <- tree_vulnerability_per_age[age]  # Lookup for the age in tree_ages[i, n]
        chance_event(pmin(risk_sum_diebacks[i] * vulnerability, 1), # pmin to prevent errors
                     value_if = 0, value_if_not = 1)  # Apply chance_event
      })
    
    
    # store dieback numbers for output (caution, 0 == death!)
    tree_dieback_number[i] <- n_trees - sum(tree_deaths_yesno)
    
    # update tree ages for the given year:
    tree_ages[i+1,] <- tree_ages[i,]*tree_deaths_yesno+1
      
    ## tree age influences----
    # on quantity
    tree_fruit_quantity_kg[i] <- tree_ages[i, ] %>% # take age of each individual
      unlist() %>%
      map_dbl(~ {
        tree_vulnerability <- tree_vulnerability_per_age[.x]  # Extract age dep. vulnerability
        risks <- risk_sum_yield_reduction[i]  # Take risks of that year
        
        # Calculate adjusted yield, prevent neg. values
        # HeH multiply or add?
        pmax(0, fruit_yield_max_kg[i] - (risks * tree_vulnerability)) 
        
      }) %>%
      sum()
    
      # on quality
      tree_fruit_quality_percent[i] <- tree_ages[i, ] %>%
        unlist() %>%
        map_dbl(~ {
          tree_vulnerability <- tree_vulnerability_per_age[.x]
          risk_factor <- risk_sum_quality_reduction[i]
          
          # Ensure the value does not go below 0
          pmax(0, 1 - (risk_factor * tree_vulnerability))
        }) %>%
        mean() # HeH mean orrect?
      
  }
  
  # tree_ages
  # plot(tree_dieback_number)
  # plot(tree_fruit_quality_percent)
  # plot(tree_fruit_quantity_kg)
  
  ## replanting costs & subsidies----
  # HeH if (scenario_subsidies_lost = FALSE)
  # function which implements the 5-year subsidies after planting
  # shift_and_accumulate_subs <- function(vec, rounds = 4) {
  #   result_vector <- vec  # Initialize with the input vector
  #   for (y in 1:rounds) {
  #     shifted_vec <- c(0, result_vector[-length(result_vector)])  # Shift right
  #     result_vector <- shifted_vec + result_vector  # Accumulate sum
  #   }
  #   return(result_vector)
  # }
  
  shift_and_accumulate_subs <- function(y1_vec, trees_vec, rounds = 4) {
    vector <- y1_vec != 0 # store when planted
    sub_vector <- vector *
      ((tree_subsidies_HALM_2_1_annual_Eur_per_tree +
         tree_subsidies_HALM_2_2_y2_5_Eur_per_tree) * trees_vec) # amount of subs
    shifted_vec <- sub_vector
    result_vector <- rep(0, n_years)

    for (y in 1:rounds) {
      shifted_vec <- c(0, shifted_vec[-length(sub_vector)])  # Shift right
      #print(shifted_vector)
      result_vector <- shifted_vec + result_vector  # Accumulate sum
      #print(result_vector)
    }
    result_vector <- result_vector+y1_vec
    return(result_vector)
  }

  
  ### in establishment year----
  tree_subsidies_Eur[1] <- n_trees *
    (tree_subsidies_HALM_2_1_annual_Eur_per_tree +
       tree_subsidies_HALM_2_2_y1_Eur_per_tree)
  # 4 following years
  trees_planted <- rep(0, n_years) 
  trees_planted[1] <- n_trees
  tree_subsidies_Eur <- shift_and_accumulate_subs(tree_subsidies_Eur, trees_planted,
                            rounds = 4)
  plot(tree_subsidies_Eur)
  
  
  ### in case of replantings---- 
  tree_subsidies_dieback_Eur <- tree_dieback_number *
    # Establishment year)
    (tree_subsidies_HALM_2_1_annual_Eur_per_tree +
       tree_subsidies_HALM_2_2_y1_Eur_per_tree)
    #  4 following years
  tree_subsidies_dieback_Eur <-  shift_and_accumulate_subs(tree_subsidies_dieback_Eur,
                              tree_dieback_number,
                              rounds = 4)
  
  #plot(tree_subsidies_dieback_Eur)
 
  
  ## labor for dieback replantings
    labor_mainteance_replanting <- tree_dieback_number*
      vv(fruit_labor_replanting_mean_h, fruit_labor_replanting_var,
         1)
    
  
  ## supply chain investment----
  labor_supply_chain_building_h <- vv(supply_chain_invest_mean_h,
                                       supply_chain_invest_var, n_years)
  
  ## fruit selling----
  ## walnut price (optional! HeH may keep out?)
    if (tree_fruit_quantity_kg[i] < uncert_wholesail_threshhold_t) {
        walnut_price[i] <- walnut_price_direct_Eur_per_kg
      } else {
       walnut_price[i] <- walnut_price_wholesale_Eur_per_kg
      }
  
  #plot(walnut_price)
  
  ## walnut price
  tree_fruit_price_Eur_per_kg <- (tree_fruit_quality_percent * uncert_influence_quali) *
    (labor_supply_chain_building_h * uncert_influence_supply_chain_invest) *
    (walnut_price) 

  ## timber----
  # Just if explicit allowance by UNB is given?
  # Harvest of all trees in last year
  tree_timber_t <- sum(vv(timber_mean_t_per_tree, 
                                timber_var_t_per_tree, n_trees))
  tree_timber_revenue_Eur[timespan] <- tree_timber_t * timber_price_mean_Eur_t
  tree_timber_labor_harvest_h[timespan] <- n_trees * timber_labor_harvest_mean_h
  costs_timber_harvest_Eur[timespan] <- tree_timber_labor_harvest_h[timespan] * labor_wage_Eur_per_h_brutto
  
  
  
  # costs and labor orchard----
  ## labor----
  # labor fruit yield
  labor_fruit_basis_h <- fruit_labor_harvest_basis_h # HeH to be refined!
  labor_fruit_quanti_dependend_h <- tree_fruit_quantity_kg * fruit_labor_harvest_h_per_kg # HeH to be refined! vv
  
  labor_fruit_yield_h <- labor_fruit_basis_h +
    labor_fruit_quanti_dependend_h
  
  # labor tree pruning
  labor_fruit_pruning_h <- fruit_labor_pruning_h_per_tree * n_trees # HeH to be refined!
  

  ## establishment costs----
  costs_establishment_trees[1] <- tree_establishment_costs
  
  ## mainteance costs----
  # HeH machinery scenario
  # machinery and labor for yield
  #machinery price depenend on scenaerio
  machinery_price <- ifelse(machinery_scenario == TRUE, 
                            fruit_price_machinery_mean_Eur/machinery_joint_participants,
                            fruit_price_machinery_mean_Eur)
  years_buying_machines <- floor(n_years/10) # HeH every 10 years, new machinery must be bought
  costs_mainteance_trees_machinery_Eur[seq(10, 
                                           length(costs_mainteance_trees_machinery_Eur), 
                                           by = 10)] <- vv(machinery_price, 
                                                           fruit_price_machinery_var_Eur, 
                                                           n = years_buying_machines)
  #plot(costs_mainteance_trees_machinery_Eur)
  
  ## supply chain costs
  costs_mainteance_supply_chain_Eur <- labor_supply_chain_building_h * 
    labor_wage_Eur_per_h_brutto
  
  ### replanting
  costs_mainteance_replanting_Eur <- labor_mainteance_replanting *
    labor_wage_Eur_per_h_brutto
  
  ### pruningwarnings()
  costs_mainteance_trees_pruning_Eur <- labor_fruit_pruning_h *
    labor_wage_Eur_per_h_brutto
  
  ### yield
  costs_mainteance_trees_yield_Eur <- labor_fruit_yield_h *
    labor_wage_Eur_per_h_brutto

  ## total costs----
  costs_mainteance_trees_total_Eur <- costs_timber_harvest_Eur + 
    costs_mainteance_trees_yield_Eur +
    costs_mainteance_supply_chain_Eur +
    costs_mainteance_trees_machinery_Eur + 
    labor_wage_Eur_per_h_brutto
    
  # orchard_benefits----
  ## subsidies----
  tree_subidies_gloez_Eur <- (tree_subsidies_GLOEZ_annual_Eur_per_ha *
                          field_size_ha * n_years)

  tree_subidies_total_Eur <- tree_subidies_gloez_Eur +
    tree_subsidies_Eur + 
    tree_subsidies_dieback_Eur

  
  ## fruit revenue----
  tree_fruit_revenue_Eur <- tree_fruit_quantity_kg * tree_fruit_price_Eur_per_kg
    
  # fruit and timber
  benefits_orchard <- (tree_fruit_revenue_Eur +
                         tree_timber_revenue_Eur) -
                      (costs_mainteance_trees_total_Eur +
                         costs_establishment_trees)
    
  
  ## yield reliability----
  #tree_fruit_reliability <- var(tree_fruit_revenue_Eur)/tree_fruit_revenue_Eur
  #plot(tree_fruit_reliability)
  
  # outcomes----
  ## drought mitigation
  ## HeH: ???? not sure how to implement that!
  ## idea: in years of drought = orchard - hay benefits
  drought_mitigation <- events_drought * (benefits_orchard - benefits_hay)
  
  #NPV
  # HeH: different discount rates for agricultural and "usual" products!
  # Thus, separated NPV calculation?
  NPV_orchard <- discount(benefits_orchard, 
                          discount_rate, 
                          calculate_NPV = TRUE)
  
  NPV_hay <- discount(benefits_hay, 
                      discount_rate, 
                      calculate_NPV = TRUE)
  NPV_decision <- NPV_hay - NPV_orchard
  
  ## benefit-labor-ratio
  #benefit_labor_ratio_hay <- benefit_hay/labor_hay_harvest_h
  #benefit_labor_ratio <- NPV/labor_total_h
  
  
  ## motivation
  # tree_fruit_reliability
  
  return(list(NPV_hay = NPV_hay,
              NPV_orchard = NPV_orchard,
              NPV_decision = NPV_decision,
              #labor_benefit_ratio = labor_benefit_ratio,
              diebacks_mean_per_year = mean(tree_dieback_number),
              drought_mitigation = drought_mitigation
              ))
  
}

################################################################################
# Model run
model_runs <- mcSimulation(estimate = as.estimate(estimate_data),
                           model_function = orchard_revitalization,
                           numberOfModelRuns = 100,
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
                   #method = "smooth_simple_overlay",
                   #method = "boxplot_density",
                   #old_names = c("NPV_orchard", "NPV_hay"),
                   new_names = "Outcome distribution for profits")


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
         threshold = 0.7)

## VOI using EVPI----
# save as dataframe
df <- data.frame(model_runs$x, model_runs$y[1:3])

# run evpi on the NPVs (the last few in the table, starting with "NPV_decision")
EVPI <- multi_EVPI(mc = df, first_out_var = "NPV_decision")

# plot the EVPI results for the decision
plot_evpi(EVPIresults = EVPI, decision_vars = "NPV_decision")


                   