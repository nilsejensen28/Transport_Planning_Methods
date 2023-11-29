rm(list=ls())
library(apollo)
library(tidyverse)

load(file="Data/mzmv_tripsWAlternatives.Rda") #Mandatory to name the data "database"

dat["time_in_pt"] <- dat["totalTravelTime_pt"] -
  dat["access_time"] -
  dat["wait_time"] -
  dat["transferWaitingTime_pt"] -
  dat["egress_time"]

dat <- dat %>% 
  dplyr::rename(time_in_car = totalTravelTime_car) %>% 
  dplyr::rename(time_in_bike = totalTravelTime_bike) %>% 
  dplyr::rename(time_in_walk = totalTravelTime_walk) %>% 
  dplyr::rename(access_pt = access_time)

database = dat

apollo_initialise()
apollo_control = list(
  modelName = "CHOOSE YOUR NAME",
  modelDescr = "MNL model with time, cost, and pt frequency and transfers",
  indivID = "HHNR",
  outputDirectory = "MNL_model_storage/apollo/ZZ",
  panelData = FALSE,
  nCores = 10
)

#Define your parameters and their starting values. ASC are a must, but define a reference one in apollo_fixed

apollo_beta= c(ASC_car=0, 
               ASC_pt=0, 
               ASC_bike=0, 
               ASC_walk=0, 
               b_cost=0, 
               b_tt_car=0,
               b_tt_pt=0,
               b_tt_bike=0,
               b_tt_walk=0,
               b_tt_access=0
               )
apollo_fixed = c()



apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  V = list()
  V[["car"]] =   ASC_car  + b_cost * cost_car  +  b_tt_car   * time_in_car
  V[["pt"]] =    ASC_pt   + b_cost * cost_pt   +  b_tt_pt    * time_in_pt    + b_tt_access * access_pt 
  V[["bike"]] =  ASC_bike +                    +  b_tt_bike  * time_in_bike
  V[["walk"]] =  ASC_walk +                    +  b_tt_walk  * time_in_walk
  
  mnl_settings = list(
    alternatives = c(car=1, pt=2, bike=3, walk=4), #Here the choice values of the choiceVar is associated to each utility function
    avail = list(car=avail_car, pt=avail_pt, bike=avail_bike, walk=avail_walk), #Here we define the variables that represent the availability of each alternative. 
    choiceVar = CHOICE,
    utilities = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_inputs=apollo_validateInputs()
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


apollo_modelOutput(model)


#Compute VTT: 
deltaMethod_settings=list(expression=c(VTT_car_min="b_tt_car/b_cost",
                                       VTT_car_hour="60*b_tt_car/b_cost"
))
apollo_deltaMethod(model, deltaMethod_settings)
