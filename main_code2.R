"Second approach: first iteration."

library(decisionSupport)
library(tidyverse)
library(readxl)

# Model development utilities ----
## "make variables" function
# gives point estimates (for code testing)
# takes table, takes random point sample based on given distribution
# form Whitney et al. (Lecture material DA)
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

## read input tables
input_uncertainities <- read_excel("Data/uncertain_variables.xlsx") 
input_data <- read_excel("data/data_estimates.xlsx") # bio-physical & economic data
estimate_data <- rbind(input_uncertainities, input_data)
# Frame_input <- read.csv() # political scenario probabilities
make_variables(as.estimate(input_uncertainities))
make_variables(as.estimate(input_data))

# Scenario input
# HeH interactive input?
#scenario_investition_subsidy <- FALSE
#scenario_joint_machinery <- TRUE
timespan <- 55
n_trees <- 15
discount_rate <- 2

################################################################################

# decision function----
orchard_revitalization <- function(){
  
  # variable declarations----
  # time span of simulation
  n_years <- timespan
  costs_establishment_hay <- rep(0, n_years)
  
  # preparation of empty vectors and lists for calculations
  #late_frost_event<-rep(NA, n_years)
  costs_establishment_trees <- rep(0, n_years)
  costs_mainteance_trees_machinery_Eur <- rep(0, n_years)
  costs_establishment_hay_Eur <- rep(0, n_years)
  walnut_price <- rep(0, n_years)

  
  # risk occurences----
  ## drought
  events_drought <- chance_event(risk_yearly_drought,
                                 value_if = (vv(risk_yearly_drought_decrease_mean,
                                                var_CV = risk_yearly_drought_decrease_var,
                                                n = n_years,
                                                lower_limit = 0.1,
                                                relative_trend = 0.2)),
                                 value_if_not = rep(0, n_years),
                                 n = n_years)
  
  plot(events_drought)
  
  ## spring frost
  events_frost <- chance_event(risk_spring_frost,
                                value_if = round(vv(risk_spring_frost_decrease_mean,
                                                    var_CV = risk_spring_frost_decrease_var,
                                                    n = n_years,
                                                    lower_limit = 1,
                                                    relative_trend = 0.05)),
                                value_if_not = rep(0, n_years),
                                n = n_years )
  
  ## disease/pests
  events_disease <- chance_event(risk_disease,
                                 value_if = (vv(risk_disease_decrease_mean,
                                                var_CV = risk_disease_dicrease_var,
                                                n = n_years,
                                                lower_limit = 0.1,
                                                relative_trend = 0.4)),
                                 value_if_not = rep(0, n_years),
                                 n = n_years)
  
  plot(events_disease)
  
  
  ## unknown-events
  events_uncert_risks <- chance_event(risk_unknown,
                                  value_if = (vv(uncert_risk_decrease_mean,
                                                 var_CV = uncert_risk_decrease_var,
                                                 n = n_years,
                                                 lower_limit = 0.1)),
                                  value_if_not = rep(0, n_years),
                                  n = n_years)
  plot(events_uncert_risks)

  
  # fruit quality and quantity----
  # max fruit yield
  fruit_yield_max_kg <- gompertz_yield(
    max_harvest = fruit_mean_kg_per_tree * n_trees,
    time_to_first_yield_estimate = fruit_time_to_first_yield_est,
    time_to_second_yield_estimate = fruit_time_to_first_yield_est+1,
    first_yield_estimate_percent = fruit_first_yield_percent,
    second_yield_estimate_percent = fruit_second_yield_percent,
    n_years = n_years,
    var_CV = fruit_yield_var_per_tree,
    no_yield_before_first_estimate = TRUE
  )
  
  plot(fruit_yield_max_kg)
  
  ## influence of risks---- 
  # tree age influence: assuming it as parameterized quadratic function
  # p1 and p2 are very uncertain -> identify whether they need more detail
  year <- 1:55
  tree_age_influence <- uncert_tree_parameter_age_2*
    (year - uncert_tree_parameter_age_1)^2
  plot(tree_age_influence) 
  
  tree_vulnerability <- uncert_tree_vulnerability * tree_age_influence
  
  # on quantity
  tree_fruit_quantity_kg <- fruit_yield_max_kg -
    ((events_drought + events_disease + events_frost + events_uncert_risks) 
     * tree_vulnerability)
  # check for negatives values- become 0!
  tree_fruit_quantity_kg[tree_fruit_quantity_kg < 0] <- 0
  plot(tree_fruit_quantity_kg)
  
  # on quality
  tree_fruit_quality_percent <- 1 - 
    ((events_drought + events_disease + events_uncert_risks) 
     * tree_vulnerability)

  
  # hay benefit----
  # hay yield
  # HeH: discuss - too humid also bad, but that would not affect trees so leave it out
  hay_yield_t_max <- vv(hay_mean_t, hay_var_t, n_years)
  hay_yield_t <- hay_yield_t_max*events_drought*events_uncert_risks
  hay_yield_revenue_Eur <- 2*hay_yield_t*(vv(hay_price_mean_Eur_t, 
                                             hay_price_var_Eur_t, 
                                             n_years)) # 2 yields per year, HeH discuss: yield dependent
  # hay costs
  costs_establishment_hay_Eur[1] <- hay_costs_establishment_Eur
  labor_mainteance_hay_h <- vv(hay_labor_harvest_mean_h, hay_labor_harvest_var_h,
                             n_years)
  costs_mainteance_hay_Eur <- labor_mainteance_hay_h * labor_wage_Eur_per_h_brutto
  
  # hay benefit
  benefit_hay <- hay_yield_revenue_Eur - 
    costs_establishment_hay - 
    costs_mainteance_hay_Eur
  

  # orchard----
  
  ## supply chain investment----
  labor_supply_chain_building_h <- vv(supply_chain_invest_mean_h,
                                       supply_chain_invest_mean_h, n_years)
  
  ## fruits----
  ## walnut price (optional! HeH may keep out?)
  for (i in 1:n_years) {
    if (tree_fruit_quantity_kg[i] < uncert_wholesail_threshhold_t) {
        walnut_price[i] <- walnut_price_direct_Eur_per_kg
      } else {
       walnut_price[i] <- walnut_price_wholesale_Eur_per_kg
      }
  }
  #plot(walnut_price)
  
  ## walnut price----
  tree_fruit_price_Eur_per_kg <- (tree_fruit_quality_percent * uncert_influence_quali) *
    (labor_supply_chain_building_h * uncert_influence_supply_chain_invest) *
    (walnut_price) 

  
  ### yield costs----
  # HeH machinery scenario
  # machinery and labor for yield
  years_buying_machines <- floor(n_years/10) # HeH every 10 years, new machinery must be bought
  costs_mainteance_trees_machinery_Eur[seq(10, 
     length(costs_mainteance_trees_machinery_Eur), 
     by = 10)] <- vv(fruit_price_machinery_mean_Eur, 
                     fruit_price_machinery_var_Eur, 
                     n = years_buying_machines)
  plot(costs_mainteance_trees_machinery_Eur)
  
  # labor fruit yield
  labor_fruit_basis_h <- fruit_labor_harvest_basis_h # HeH to be refined!
  labor_fruit_quanti_dependend_h <- tree_fruit_quantity_kg * fruit_labor_harvest_h_per_kg # HeH to be refined! vv
    
  labor_fruit_yield_h <- labor_fruit_basis_h +
    labor_fruit_quanti_dependend_h
  
  # tree pruning
  labor_fruit_pruning_h <- fruit_labor_pruning_h_per_tree * n_trees # HeH to be refined!
  
  ### fruit revenue----
  tree_fruit_revenue_Eur <- tree_fruit_quantity_kg * tree_fruit_price_Eur_per_kg
  
  ## timber----
  # Just if explicit allowance by UNB is given?
  # Harvest of all trees in last year
  tree_timber_t <- vv(timber_mean_t_per_tree, 
                                timber_var_t_per_tree, n_years) * n_trees
  tree_timber_revenue_Eur <- tree_timber_t*(vv(timber_price_mean_Eur_t, 
                                               timber_price_var_Eur_t, 
                                                 n_years)) 
  tree_timber_labor_harvest_h <- vv(n_trees*timber_labor_harvest_mean_h, 
                                          n_trees*timber_labor_harvest_var_h, n_years)
  costs_timber_harvest_Eur <- tree_timber_labor_harvest_h * labor_wage_Eur_per_h_brutto
  
  
  
  # costs and labor orchard----
  ## supply chain costs----
  costs_mainteance_supply_chain_Eur <- labor_supply_chain_building_h * 
    labor_wage_Eur_per_h_brutto
  
  ## establishment costs----
  costs_establishment_trees[1] <- tree_establishment_costs
  
  ## mainteance costs----
  ### pruning etc
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
  # fruit and timber
  benefits_orchard <- (tree_fruit_revenue_Eur +
                         tree_timber_revenue_Eur) -
                      (costs_mainteance_trees_total_Eur +
                         costs_establishment_trees)
    
  
  ### yield reliability----
  #tree_fruit_reliability <- var(tree_fruit_revenue_Eur)/tree_fruit_revenue_Eur
  #plot(tree_fruit_reliability)
  
  # outcomes----
  ## drought mitigation
  ## HeH: ???? not sure how to implement that!
  drought_mitigation <- events_drought*tree_vulnerability
  
  #NPV
  NPV_orchard <- discount(benefits_orchard, 
                          discount_rate, 
                          calculate_NPV = TRUE)
  
  NPV_hay <- discount(benefits_orchard, 
                      discount_rate, 
                      calculate_NPV = TRUE)
  
  ## benefit-labor-ratio
  #benefit_labor_ratio_hay <- benefit_hay/labor_hay_harvest_h
  #benefit_labor_ratio <- NPV/labor_total_h
  
  
  ## motivation
  # tree_fruit_reliability
  
  return(list(NPV_hay = NPV_hay,
              NPV_orchard = NPV_orchard))
  
}

model_runs <- mcSimulation(estimate = as.estimate(estimate_data),
                           model_function = orchard_revitalization,
                           numberOfModelRuns = 100,
                           functionSyntax = "plainNames")
