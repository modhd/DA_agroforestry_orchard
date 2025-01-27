"Second approach: first iteration."

library(decisionSupport)
library(tidyverse)
library(readxl)

# Input data preparation----
## "make variables" function
# gives point estimates (for code testing)
# takes table, takes random point sample based on given distribution
# form Whitney et al. (Lecture material DA)
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

## read input tables----
input_uncertainities <- read_excel("Data/uncertain_variables.xlsx") 
input_data <- read_excel("data/data_estimates.xlsx") # bio-physical & economic data
# Frame_input <- read.csv() # political scenario probabilities
make_variables(as.estimate(input_uncertainities))
make_variables(as.estimate(input_data))

# Scenario input----
# HeH interactive input?
#scenario_investition_subsidy <- FALSE
#scenario_joint_machinery <- TRUE
timespan <- 55
farmer_motivation <- 0.5

################################################################################

# decision function----
orchard_revitalization <- function(){
  
  # variable declarations----
  # time span of simulation
  n_years <- timespan
  costs_establishment_hay <- rep(0, n_years)
  
  # preparation of empty vectors and lists for calculations
  #late_frost_event<-rep(NA, n_years)

  
  # risk occurences----
  ## drought----
  events_drought <- chance_event(risk_yearly_drought,
                                 value_if = (vv(risk_yearly_drought_decrease_mean,
                                                var_CV = risk_yearly_drought_decrease_var,
                                                n = n_years,
                                                lower_limit = 0.1,
                                                relative_trend = 0.2)),
                                 value_if_not = rep(0, n_years),
                                 n = n_years)
  
  plot(events_drought)
  
  # spring frost----
  frost_event <- chance_event(risk_late_frost,
                                value_if = round(vv(risk_spring_frost_decrease_mean,
                                                    var_CV = risk_spring_frost_decrease_var,
                                                    n = n_years,
                                                    lower_limit = 1,
                                                    relative_trend = 0.05)),
                                value_if_not = rep(0, n_years),
                                n = n_years )
  
  
  ## unknown-events----
  events_uncert_risks <- chance_event(risk_unknown,
                                  value_if = (vv(uncert_risk_decrease_mean,
                                                 var_CV = uncert_risk_decrease_var,
                                                 n = n_years,
                                                 lower_limit = 0.1)),
                                  value_if_not = rep(0, n_years),
                                  n = n_years)
  plot(events_uncert_risks)

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
  labor_hay_harvest_h <- vv(hay_labor_harvest_mean_h, hay_labor_harvest_var_h,
                             n_years)
  costs_mainteance_hay_Eur <- labor_mainteance_hay_h * labor_wage_Eur_per_h_brutto
  
  # hay benefit
  benefit_hay <- hay_yield_revenue_Eur - 
    costs_establishment_hay - 
    costs_mainteance_hay
  
  
  # fruit quality and quantity----
  # tree_fruit_quantity_kg <-

  # costs and labor orchard----
  
  ## establishment costs----
  
  ## mainteance costs----
  
  
  ## supply chain investment----
  labor_supply_chain_building_h <- vv(supply_chain_invest_mean_h,
                                       supply_chain_invest_mean_h, n_years)
  
  costs_mainteance_supply_chain_Eur <- labor_supply_chain_building_h * 
    labor_wage_Eur_per_h_brutto
  
  ## walnut price (optional! HeH may keep out?)
  if (tree_fruit_quantity_kg > uncert_wholesail_threshhold_t) {
    walnut_price <- walnut_price_direct_Eur_per_kg } else {
      walnut_price <- walnut_price_wholesale_Eur_per_kg
    }
  
  ## final walnut price
  tree_fruit_price_Eur_per_kg <- (fruit_quality * uncert_influence_quali) *
    (costs_mainteance_supply_chain_Eur * uncert_influence_supply_chain_invest) *
    (walnut_price) 
  
  # fruit yield orchard----
  # tree_fruit_quantity_kg
  # tree_fruit_revenue <- tree_fruit_quantity_kg * tree_fruit_price_Eur_per_kg
  
  ## yield reliability
  tree_fruit_reliability <- var(tree_fruit_revenue)
  
  # benefit orchard----
  tree_fruit_revenue_Eur <- tree_fruit_quantity_kg * tree_fruit_price_Eur_per_kg
  
  
  
  # outcomes----
  ## drought mitigation
  # tree age influence: assuming it as parameterized quadratic function
  # p1 and p2 are very uncertain -> identify whether they need more detail
  year <- 1:55
  tree_age_influence <- uncert_tree_parameter_age_2*
    (year - uncert_tree_parameter_age_1)^2
  plot(tree_age_influence) 
  
  tree_vulnerability <- uncert_tree_vulnerability * tree_age_influence
  #drought_mitigation <- events_drought*tree_vulnerability
  
  ## NPV
  
  
  ## benefit-labor-ratio
  benefit_labor_ratio_hay <- benefit_hay/labor_hay_harvest_h
  benefit_labor_ratio <- NPV/labor_total_h
  
  
  ## motivation
  # tree_fruit_reliability
  
  

  
}
