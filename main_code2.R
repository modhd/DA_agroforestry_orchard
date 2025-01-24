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
  costs_timber_harvest_Eur <