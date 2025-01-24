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
orchard_input <- read_excel("data/data_orchard.xlsx") # bio-phsyical data
economic_input <- read_excel("data/data_economics.xlsx") # economic data
# Frame_input <- read.csv() # political scenario probabilities
make_variables(as.estimate(orchard_input))
make_variables(as.estimate(economic_input))

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
  
  ## management variables
  # HeH tree management can become "starting", "learning", "well-doing"
  # HeH dependent on knowledge status
  # knowledge_status <- c("starting", "learning", "well-doing")
  
  # preparation of empty vectors and lists for calculations
  late_frost_event<-rep(NA, n_years)
  events_damage <- rep(0, n_years)
  costs_establishment_hay <- rep(0, n_years)
  yield_timber_t <- rep(0, n_years)
  costs_timber_harvest_Eur <- rep(0, n_years)
  #too_late_start_list <- vector(mode = "list", length = n_years_protection)
  dieback_number_trees <- rep(0, n_years)
  costs_mainteance_replanting <- rep(0, n_years)
  

  # common risk_ drought----
  events_drought <- chance_event(risk_yearly_drought,
                                value_if = (vv(mean_yearly_drought_decrease,
                                                    var_CV = var_cv_yearly_drought_decrease,
                                                    n = n_years,
                                                    lower_limit = 0.1,
                                                    relative_trend = 0.2)),
                                value_if_not = rep(0, n_years),
                                n = n_years )
  
  # plot(drought_events)
  
  # common costs----
  costs_establishment_hay[1] <- costs_establishment_hay_Eur
  costs_mainteance_hay <- vv(mean_yield_hay_labor_h, var_cv_yield_hay_labor_h, n_years)*
                                    labor_wage_Eur_per_h_brutto
   
  # hay yield----
  # use vv function for hay yield to account for other factors than drought
  # HeH: discuss - too humid also bad, but that would not affect trees so leave it out
  yield_hay_t_max <- vv(mean_hay_t, var_cv_hay_t, n_years)
  yield_hay_t <- yield_hay_t_max*drought_events # HeH reduction by drought? discuss
  yield_hay_revenue_Eur <- 2*yield_hay_t*(vv(mean_price_hay_Eur_t, 
                                             var_cv_price_hay_Eur_t, 
                                             n_years)) # 2 yields per year, HeH discuss: yield dependent
  
  # orchard-related aspects----
  ## risks----
  ### yield optimum----
  # HeH urgent - if no risks would decrease the yields as baseline (gompertz) and then substract risks?
    
  
  ### frost----
  # frost_event <- chance_event(risk_late_frost,
  #                               value_if = round(vv(mean_yearly_late_frost,
  #                                                   var_CV = var_yearly_late_frost,
  #                                                   n = n_years,
  #                                                   lower_limit = 1,
  #                                                   relative_trend = 0.05)),
  #                               value_if_not = rep(0, n_years), 
  #                               n = n_years )
  
  ### damage----
  # damage caused by animals (lower yield) just in first 5 years
  events_damage[1:5] <- chance_event(risk_damage,
                                value_if = vv(mean_damage_decrease,
                                                    var_CV = var_cv_damage_decrease,
                                                    n = 5,
                                                    lower_limit = 0.1,
                                                    relative_trend = -0.15),
                                value_if_not = rep(0, 5),
                                n = 5)
  #plot(damage_event)
  
  ### summarized yearly risks----
  risk_sum_yield_reductions <- events_drought + events_damage #+ events_drought
  #plot(risk_sum_yield_reductions)
  # HeH include a factor that shows: older trees are more stable and less risk affected
  # risk_age_vulnerability <- rep(0, n_years)
  # risk_age_vulnerability[1:27] <- (vv(mean_risk_affection_age, var_cv_risk_affection_age,
  #                                     relative_trend = -0.5,
  #                                     27))
  # risk_age_vulnerability[28:55] <- rev(risk_age_vulnerability[1:27])
  # plot(risk_age_vulnerability)
  
  
  ### planting allowance----
  # Untere Naturschutzbehörde: allowance to plant 
  events_permission <- chance_event(risk_permission,TRUE,FALSE,n=n_years, one_draw = TRUE)

  if (events_permission == FALSE) {
    # if no permission: stop
    stop("No permission was given by UNB.")
    # HeH: affect motivation?
  } else {
    # adapt orchard management systems
    # farmer_motivation <- 0.5
    # knowledge_status <- "start"
    tree_management <- "start"
    # harvest_machinery <- 
  }
  
  ## yearly steps----
  #for walnut tree care & political scenario
  # where balance, knowledge and yield reliability change with every year,
  # affecting motivation and thus, tree management
  for (i in 1:n_years) {

    ### farmers' motivation----
    # HeH affected by l-b-r, balance, knowledge, yield reliability
    
    ### update tree management----
    # management_trees <- HeH affected by motivation?
    # HeH urgent - how to express motivation as a variable??
    # knowledge --> shall help to reduce diebacks?!
    
    ### dieback----
    # Note: we need to replant so it will stay an orchard
    # Heh: urgent - how to say "when it is too much so the tree will die"?!
    # HeH: idea - take vv function over all trees with risk_chance of sum risks
    # And include parameter for motivation_influence?
    diebacks <- chance_event(risk_sum_yield_reductions[i]*risk_age_vulnerability[i],
                             value_if = 1,
                             value_if_not = 0,
                             n_trees)
    dieback_number_trees[i] <- sum(diebacks)
    costs_mainteance_replanting[i] <- dieback_number_trees[i]*
      vv(mean_replanting_labor_h, var_cv_replanting_labor_h,
         1)
    
    ### fruit yield (quali & quanti)----
    # fruit quality
    # HeH: depends on drought, disease, knowledge
    
    # fruit quantity
    # HeH urgent: depends on risks: max - risks * affection_rate * knowledge (?)
    
    # fruit revenue
    # HeH: depends on quantity + quality (how?)
    
    ### yield outcome ----
    # yield reliability
    # Heh: variance of revenue or yield over the years so far
    
    # knowledge
    # HeH: With higher risks but nevertheless high yields, knowledge rises?
    
    ## total costs & labor hours----
    # HeH costs_establishment_walnut <- (planting_material + plant_protection)*trees + labor_hours + machinery
    # HeH costs_mainteance_walnut <- yield_walnut_labor + machinery[scenario] ....
  
    ## current balance
    
    ## labor-benefit-ratio
    
    ## update motivation
    
  }
  
  ## Timber yield----
  # Just if explicit allowance by UNB is given?
  # Harvest of all trees in last year
  yield_timber_t[n_years] <- vv(mean_timber_t_per_tree, 
                                var_cv_timber_t_per_tree, 1) * n_trees
  # 2 yields per year, HeH discuss: yield dependent
  yield_timber_revenue_Eur <- 2*yield_timber_t*(vv(mean_price_timber_Eur_t, 
                                                   var_cv_price_timber, 
                                                   n_years)) 
  costs_timber_harvest_Eur[n_years] <- vv(n_trees*mean_harvest_timber_labor_h, 
                                          n_trees*var_cv_timber_harvest_labor, 1)*
    labor_wage_Eur_per_h_brutto
  
  ## mainteance costs----
  costs_mainteance_total <- costs_mainteance_hay + cost_mainteance_replanting #+...
  
    
  # calculate benefits----
  benefit_hay <- yield_hay_revenue_Eur - 
    costs_establishment_hay - 
    costs_mainteance_hay
  
  #benefit_timber <- 
  #benfit_walnuts <- 
  
  
  return(list())
}

################################################################################

# monte carlo simulation, VIO and EVPI calculations