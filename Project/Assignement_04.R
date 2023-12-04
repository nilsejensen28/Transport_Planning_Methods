rm(list=ls())
library(apollo)
library(tidyverse)
library(sf)
library(wesanderson) #Nice color palette

################################################################################
#Load and preprocess the data
################################################################################

load(file="Data/mzmv_tripsWAlternatives.Rda") 

dat["time_in_pt"] <- dat["totalTravelTime_pt"] -
  dat["access_time"] -
  dat["wait_time"] -
  dat["transferWaitingTime_pt"] -
  dat["egress_time"]

dat <- dat %>% 
  dplyr::rename(time_in_car = totalTravelTime_car) %>% 
  dplyr::rename(time_in_bike = totalTravelTime_bike) %>% 
  dplyr::rename(time_in_walk = totalTravelTime_walk) %>% 
  dplyr::rename(access_pt = access_time) %>% 
  dplyr::rename(waiting_pt = wait_time) %>% 
  dplyr::rename(egress_pt = egress_time) 

dat["waiting_pt"] <- dat["waiting_pt"] + dat["transferWaitingTime_pt"]

dat["solarenergy"] <- sapply(dat$solarenergy, as.numeric)
dat["precip"] <- sapply(dat$precip, as.numeric)

dat["id"] <- paste(dat$HHNR, dat$WEGNR, sep="_")

################################################################################
#Initialize the Apollo Framework
################################################################################

database <- dat #Mandatory to name the data "database"

apollo_initialise()
apollo_control = list(
  modelName = "mode_choice_model",
  modelDescr = "MNL model with time, cost, and pt frequency and transfers",
  indivID = "id",
  outputDirectory = "MNL_model_storage/apollo/ZZ",
  panelData = FALSE,
  nCores = 10
)

#Define your parameters and their starting values. ASC are a must, but define a reference one in apollo_fixed

apollo_beta= c(ASC_car=0, 
               ASC_pt=-1.377865, 
               ASC_bike=-1.891062, 
               ASC_walk=0.341877, 
               b_cost=-0.147952, 
               b_tt_car=-0.094219,
               b_tt_pt=-0.006232,
               b_tt_bike=-0.088146,
               b_tt_walk=-0.088666,
               b_tt_access=-0.042881,
               b_tt_waiting=-0.042344,
               b_transfers=-0.170433,
               b_tt_egress=-0.058349,
               b_pt_precip=0.003822,
               b_pt_sun=-0.005141,
               b_bike_precip=-0.005946,
               b_bike_sun=0.014546,
               b_walk_precip=0,
               b_walk_sun=-0.003108
               )
apollo_fixed = c("ASC_car")

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  V = list()
  V[["car"]] =   ASC_car  + b_cost * cost_car  +  b_tt_car    * time_in_car 
  V[["pt"]] =    ASC_pt   + b_cost * cost_pt   +  b_tt_pt     * time_in_pt +  b_tt_access * access_pt + b_tt_waiting * waiting_pt + b_transfers * transfers_pt + b_tt_egress * egress_pt + 
                                                  b_pt_precip * precip       + b_pt_sun   * solarenergy
  V[["bike"]] =  ASC_bike +                    +  b_tt_bike   * time_in_bike +
                                                  b_bike_precip * precip       + b_bike_sun   * solarenergy
  V[["walk"]] =  ASC_walk +                    +  b_tt_walk  * time_in_walk +
                                                  b_walk_precip * precip       + b_walk_sun   * solarenergy

  mnl_settings = list(
    alternatives = c(car=1, pt=2, bike=3, walk=4), #Here the choice values of the choiceVar is associated to each utility function
    avail = list(car=avail_car, pt=avail_pt, bike=avail_bike, walk=avail_walk), #Here we define the variables that represent the availability of each alternative. 
    choiceVar = CHOICE,
    utilities = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
apollo_inputs=apollo_validateInputs()
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model)

################################################################################
#Compute the VTT
################################################################################
deltaMethod_settings=list(expression=c(VTT_car_min="b_tt_car/b_cost",
                                       VTT_car_hour="60*b_tt_car/b_cost",
                                       VTT_pt_min="b_tt_pt/b_cost",
                                       VTT_pt_min="60*b_tt_pt/b_cost"
))
apollo_deltaMethod(model, deltaMethod_settings)
apollo_inputs <- apollo_validateInputs()

################################################################################
#Compute the elasticities
################################################################################

predictions_base <- apollo_prediction(model, apollo_probabilities, apollo_inputs)
travel_type = c("pt", "car", "bike", "walk")
parameters = c("cost_pt", "cost_car", "time_in_pt", "time_in_car", "time_in_bike", "time_in_walk", "precip", "solarenergy")
elasticities <- data.frame(row.names = parameters)
df_elasticities <- data.frame()
for (param in parameters){
  database[param] <- database[param]*1.01 #Increase by 1%
  apollo_inputs <- apollo_validateInputs(silent=TRUE)
  predictions_new <- apollo_prediction(model, apollo_probabilities, apollo_inputs)
  for (type in travel_type){
    elasticity <- log(sum(predictions_new[type])/sum(predictions_base[type]))/log(1.01)
    elasticities[param, type] <- elasticity
    df_new <- data.frame(parameter=c(param),
                         transport_mode=c(type),
                         elasticity=c(elasticity))
    print(df_elasticities)
    if (nrow(df_elasticities)==0){
      df_elasticities <- df_new
    }
    else{
      df_elasticities <- df_elasticities %>% dplyr::add_row(df_new)
    }
  }
  database[param] <- database[param]/1.01 #Revert change
}
elasticities


#Save the model!
apollo_saveOutput(model)

################################################################################
#Plot the elasticities
################################################################################

plot.elasticities <- ggplot(df_elasticities, aes(x = parameter, y = transport_mode, fill = elasticity)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = "#598BF0", mid="white", high = "#E85959") +  #General tweaks to the theme
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 15, title="elasticities")) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
        axis.text=element_text(size=13), #Size of axis numbering
        axis.text.x = element_text(angle = 45, hjust=1), #Tilting of axis numbering
        axis.title=element_text(size=13,face="bold"), #Size of axis title
        legend.text=element_text(size=13),
        legend.title=element_text(size=14, face="bold"),
        legend.background = element_rect(fill="white", color="black", size=0.4, linetype ="solid"),
        legend.position = "right",
        title = element_text(size=15, face="bold")) + #Size of title
  labs(x="Parameter", y="Transport Mode", title="(Cross)-Elasticities of the model")
ggsave(plot.elasticities, filename = "plots/4_plot_elasticities.svg")
ggsave(plot.elasticities, filename = "plots/4_plot_elasticities.png")
plot.elasticities

################################################################################
#Apply to the Data from Assignement 3
################################################################################

load("results_assignement_3.Rda")

df_trips <- database %>% 
  dplyr::mutate(weg_id = row_number()) %>%
  relocate(weg_id)

npvm_zones <- read_sf("NPVM2010MetroZH.shp")

wege_Start <- st_as_sf(df_trips, coords=c("start_x", "start_y"),
                       crs=4326) #WGS 84 coordinates

#st_intersects instead of st_intersection for speed. 
npvm_zones <- st_transform(npvm_zones, crs=4326) #transform from LV95 to WGS 84 coordinates

zone_Start <- wege_Start %>%
  mutate(start_row.id=(st_intersects(wege_Start$geometry, npvm_zones))) %>%
  relocate(start_row.id) %>%
  mutate(start_row.id=as.integer(start_row.id)) %>%
  filter(!is.na(start_row.id)) 

wege_Ziel <- st_as_sf(df_trips, coords=c("end_x", "end_y"),
                      crs=4326)

zone_Ziel <- wege_Ziel %>%
  mutate(ziel_row.id=(st_intersects(wege_Ziel$geometry, npvm_zones))) %>%
  relocate(ziel_row.id) %>%
  mutate(ziel_row.id=as.integer(ziel_row.id)) %>%
  filter(!is.na(ziel_row.id))

wege <- df_trips %>%
  left_join(zone_Start[,c("weg_id", "start_row.id")], by="weg_id") %>%
  left_join(zone_Ziel[,c("weg_id", "ziel_row.id")], by="weg_id") 

#Filter the original wege 
wege <- wege[wege$weg_id %in% zone_Start$weg_id,]
wege <- wege[wege$weg_id %in% zone_Ziel$weg_id,]
number_of_cencus_trips <- n_distinct(wege$weg_id)

wege_Start <- wege_Start[wege_Start$weg_id %in% wege$weg_id, ]
wege_Ziel <- wege_Ziel[wege_Ziel$weg_id %in% wege$weg_id, ]

database <- wege
apollo_inputs <- apollo_validateInputs()
prediction <- apollo_prediction(model, apollo_probabilities, apollo_inputs)
df_trips <- prediction %>% 
  dplyr::rename(id=ID) %>% 
  dplyr::full_join(wege, by="id")
df_trips
n <- length(npvm_zones$row_id)
df_results = data.frame(start_id=numeric(),
                        end_id=numeric(),
                        car=numeric(),
                        pt=numeric(),
                        bike=numeric(),
                        walk=numeric())
for (start in 1:n){
  for (end in 1:n){
    df_filtered <- df_trips %>% 
      dplyr::filter(start_row.id==start) %>% 
      dplyr::filter(ziel_row.id==end) %>% 
      summarise(car = mean(car), pt = mean(pt), bike = mean(bike), walk = mean(walk))
    df_filtered$start_id = start
    df_filtered$end_id = end
    df_results <- rbind(df_results, df_filtered)
  }
}

#This will be used to fill the holes
df_average_total = df_trips %>%
  summarise(car = mean(car), pt = mean(pt), bike = mean(bike), walk = mean(walk))
df_average_samezone = df_trips %>% 
  dplyr::filter(start_row.id == ziel_row.id) %>% 
  summarise(car = mean(car), pt = mean(pt), bike = mean(bike), walk = mean(walk))
df_average_diffzone = df_trips %>% 
  dplyr::filter(start_row.id != ziel_row.id) %>% 
  summarise(car = mean(car), pt = mean(pt), bike = mean(bike), walk = mean(walk))
df_results <- data.frame(sapply(df_results, function(x) ifelse(is.nan(x), NA, x)))

for (i in 1:nrow(df_results)){
  start = df_results[i, "start_id"]
  end = df_results[i, "end_id"]
  if(is.na(df_results[i, "pt"])){
    if (start==end){
      df_results[i, "car"] = df_average_samezone[1, "car"]
      df_results[i, "pt"] = df_average_samezone[1, "pt"]
      df_results[i, "bike"] = df_average_samezone[1, "bike"]
      df_results[i, "walk"] = df_average_samezone[1, "walk"]
    }
    else{
      df_results[i, "car"] = df_average_diffzone[1, "car"]
      df_results[i, "pt"] = df_average_diffzone[1, "pt"]
      df_results[i, "bike"] = df_average_diffzone[1, "bike"]
      df_results[i, "walk"] = df_average_diffzone[1, "walk"]
    }
  }
}

df_furness <- results_assignement_3 %>% 
  dplyr::rename(start_id=start_row.id, end_id=ziel_row.id) %>% 
  full_join(df_results, by=c("start_id", "end_id"))

df_averages_canton_zurich = df_furness %>% 
  dplyr::summarise(car=weighted.mean(car, trips_furness), 
                   pt=weighted.mean(pt, trips_furness), 
                   bike=weighted.mean(bike, trips_furness), 
                   walk=weighted.mean(walk, trips_furness))

zurich_city = c(132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143)
df_averages_to_zurich_city = df_furness %>% 
  dplyr::filter(start_id %in% zurich_city) %>% 
  dplyr::filter(!end_id %in% zurich_city) %>% 
  dplyr::summarise(car=weighted.mean(car, trips_furness), 
                   pt=weighted.mean(pt, trips_furness), 
                   bike=weighted.mean(bike, trips_furness), 
                   walk=weighted.mean(walk, trips_furness))

df_averages_within_zurich = df_furness %>% 
  dplyr::filter(start_id %in% zurich_city) %>% 
  dplyr::filter(end_id %in% zurich_city) %>% 
  dplyr::summarise(car=weighted.mean(car, trips_furness), 
                   pt=weighted.mean(pt, trips_furness), 
                   bike=weighted.mean(bike, trips_furness), 
                   walk=weighted.mean(walk, trips_furness))

df_averages <- rbind(df_averages_within_zurich, df_averages_to_zurich_city, df_averages_canton_zurich)

df_averages <- t(df_averages)

colnames(df_averages) <- c("within Zürich city", "to Zürich city", "within canton Zürich")
df_averages

df_plot <- data.frame()
for (row in c("car", "pt", "bike", "walk")){
  for (col in colnames(df_averages)){
    df_new <- data.frame(
      type = c(row),
      place = c(col),
      percentage = c(df_averages[row, col])
    )
    df_plot <- rbind(df_plot, df_new)
  }
}

################################################################################
#Plot the predicted data
################################################################################

plot_modeled_mode_share <- ggplot(df_plot, 
  aes(fill = type, y = percentage, x = place))+ 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels=function(x) x) +      #Needs to be a better way than dividing by 10
  ylab("Mode share") +
  scale_fill_manual(values=wes_palette("GrandBudapest1", n=4, type = "discrete"), 
                    name="Transport mode") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percentage*100),"%")),
            position=position_stack(vjust=0.5), size = 3, 
            color="white") +
  ggtitle("Mode share canton Zürich") +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white", colour="black", linewidth =0.5, linetype="solid"), #No background and frame
        axis.text=element_text(size=9), #Size of axis numbering
        axis.text.x = element_text(angle = 45, hjust=1), #Tilting of axis numbering
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title=element_text(size=13,face="bold"), #Size of axis title
        legend.text=element_text(size=10),
        legend.title=element_text(size=13, face="bold"),
        legend.background = element_rect(fill="white", color="black", linewidth=0.4, linetype ="solid"),
        legend.position = "right",
        title = element_text(size=15, face="bold")) + #Size of title
  labs(x="Type of trip", y="Mode share", title="Modeled mode share in Zürich")
ggsave(plot_modeled_mode_share, filename = "plots/4_plot_modeled_mode_share.svg")
ggsave(plot_modeled_mode_share, filename = "plots/4_plot_modeled_mode_share.png")
plot_modeled_mode_share

