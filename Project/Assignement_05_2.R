library(igraph)
library(units)
library(sf)
library(tidyverse)
library(gridExtra)
library(openxlsx)
#-----------------------------------------------------------------------------------START_ 

#1-Read in the four shapefiles:
links_pt <- read_sf("Data/Shapefiles/links_PTNet.shp")
links_car <- read_sf("Data/Shapefiles/links_carNet.shp")
nodes_pt <- read_sf("Data/Shapefiles/nodes_PTNet.shp")
nodes_car <- read_sf("Data/Shapefiles/nodes_carNet.shp")
links_pt <- st_transform(links_pt, crs = 2056)
nodes_pt <- st_transform(nodes_pt, crs = 2056)
npvm_zones <- read_sf("NPVM2010MetroZH.shp")

#Calculate speeds in car network links based on demand: 
#____________________________________________________________________________________________________
#DO NOT CHANGE THIS CODE:
df_miv <- sf::st_drop_geometry(links_car)

df_miv$length <- sf::st_length(links_car)/1000

#BPR Function application
df_miv$Bel_GESCHW <- df_miv$GESCHW/(1+0.32*(df_miv$VOL_TOT/df_miv$KAPAZIT)^3.27)*1.1
df_miv$Bel_GESCHW <- ifelse(is.na(df_miv$Bel_GESCHW),40, df_miv$Bel_GESCHW)

df_miv$tt <- df_miv$length/df_miv$Bel_GESCHW*60 #in minutes

#____________________________________________________________________________________________________
#DO NOT CHANGE THIS CODE:
#now for PT (simplistic method with standard speed for Nahverkehr vs. Vollbahn: 
df_pt<- sf::st_drop_geometry(links_pt)
df_pt$length <- as.numeric(substr(df_pt$LENGHT,1,nchar(df_pt$LENGHT)-2))

commercial_speed_bus_tram <- 15
commercial_speed_train <- 35

#Assign speeds based on fastest available PT mode
df_pt <- df_pt %>%
  mutate(speed=if_else(
    grepl("FV", SYSTEMNO, fixed=TRUE) | grepl("RV", SYSTEMNO, fixed=TRUE),
    commercial_speed_train,
    commercial_speed_bus_tram
  ))

df_pt$tt <- df_pt$length/df_pt$speed*60 #in minutes
df_pt <- df_pt %>% 
  arrange(NO)
df_pt = df_pt[!duplicated(df_pt$NO),] #remove duplicates
df_pt <- df_pt %>% 
  dplyr::select(NO, FROMNODE, TONODE, tt, length, ID_Zo)
#____________________________________________________________________________________________________
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nrow(nodes_pt)**2, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "|")   # Character used to create the bar

#Add the possible foot connections between nodes
i=0
count=0
for (left_node in nodes_pt$NO){
  for(right_node in nodes_pt$NO){
    if(left_node != right_node){
      i=i+1
      #Compute the length between the two nodes using nodes_pt
      setTxtProgressBar(pb, i)
      length <- st_distance(nodes_pt[nodes_pt$NO==left_node,], nodes_pt[nodes_pt$NO==right_node,])
      if (drop_units(length) < 300){
        print(drop_units(length))
        count=count+1
        print(count)
        ID_Zo <- nodes_pt[nodes_pt$NO==left_node,]$ID_Zo
        df_pt <- rbind(df_pt , data.frame(NO = paste0(left_node, right_node), FROMNODE = left_node, TONODE = right_node, length = drop_units(length), ID_Zo = ID_Zo, tt = drop_units(length)*12/1000 + 6.0))
      }
    }
  }
}
print(count)


#Add manual links 8572585 - 8503319, 8502880 - 8502956
df_pt <- rbind(df_pt, data.frame(NO = "85725858503319", FROMNODE = "8572585", TONODE = "8503319", length = 0.5, ID_Zo = "4043", tt = 2.0))
df_pt <- rbind(df_pt, data.frame(NO = "85028808502956", FROMNODE = "8502880", TONODE = "8502956", length = 0.5, ID_Zo = "4039", tt = 2.0))
save(df_pt, file = "Data/df_pt.Rda")
load("Data/df_pt.Rda")

#_______________________________________________________________________________________________________
#Now prepare graphs for running the dijkstra algorithm on. 

#Remove units to allow assignment of test value
df_miv$tt <- drop_units(df_miv$tt)
df_miv <- df_miv %>% 
  dplyr::select(FROMNODE, TONODE, tt, ID_Zo, length)

#Add link from 5559 to 5556
df_miv <- rbind(df_miv, data.frame(FROMNODE = "5559", TONODE = "5556", tt = 1.0, ID_Zo = "4043", length = 0.5))

df_miv$FROMNODE <- as.character(df_miv$FROMNODE)
df_miv$TONODE <- as.character(df_miv$TONODE)
df_miv$tt <- as.numeric(df_miv$tt)

#Generate igraph
car_graph <- graph_from_data_frame(df_miv[,c("FROMNODE","TONODE", "tt")], directed = FALSE)
#provide travel times as weight:
E(car_graph)$weight <- as.numeric(df_miv$tt)  


miv_od <- nodes_car[!(duplicated(nodes_car$ID_Zo)),]
miv_od <- miv_od %>%
  expand(NO, NO) 

miv_od <- st_drop_geometry(miv_od)

miv_od$tt <- NA

#Calculate travel times between zones based on NPVM values (shortest-path search with dijkstra algorithm) 

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nrow(miv_od), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "|")   # Character used to create the bar

#Find shortest paths for all OD pairs: 
for(i in 1:nrow(miv_od)){
  setTxtProgressBar(pb, i)  
  source_node <- as.character(miv_od[i,1])
  target_node <- as.character(miv_od[i,2])
  
  path <- shortest_paths(car_graph, from=source_node, to = target_node, weights=E(car_graph)$weight)
  path_weight_sum <- sum(E(car_graph, path=path$vpath[[1]])$weight)
  
  miv_od[i,]$tt <- path_weight_sum
  if (source_node == target_node){
    miv_od[i,]$tt <- 5.0
  }
}
miv_od <- miv_od %>% 
  dplyr::rename(start_node = NO...1, end_node = NO...2, tt_car = tt)
#Get the start zone and end zone from nodes_car
miv_od <- miv_od %>% 
  left_join(nodes_car %>% 
              dplyr::select(NO, ID_Zo) %>% 
              dplyr::rename(start_node = NO), by = "start_node") %>% 
  left_join(nodes_car %>% 
              dplyr::select(NO, ID_Zo) %>% 
              dplyr::rename(end_node = NO), by = "end_node") %>% 
  dplyr::select(start_node, end_node, tt_car, ID_Zo.x, ID_Zo.y) %>% 
  dplyr::rename(start_zone = ID_Zo.x, end_zone = ID_Zo.y)
miv_od <- miv_od %>% 
  dplyr::select(start_zone, end_zone, tt_car)
#_________________________________________________________________________________________
#Now do the same steps above for the PT graph to calculate PT travel times between zone
compute_od_pt <- function(df_pt){
  df_pt$FROMNODE <- as.character(df_pt$FROMNODE)
  df_pt$TONODE <- as.character(df_pt$TONODE)
  df_pt$tt <- as.numeric(df_pt$tt)
    
  #Generate igraph
  pt_graph <- graph_from_data_frame(df_pt[,c("FROMNODE","TONODE", "tt")], directed = FALSE)
  #provide travel times as weight:
  E(pt_graph)$weight <- as.numeric(df_pt$tt)  
    
  nodes_mivStartingpoint <- st_intersection(nodes_pt, npvm_zones)
  pt_od <- nodes_mivStartingpoint[]
  pt_od <- pt_od %>%
  expand(NO, NO) 
  pt_od <- st_drop_geometry(pt_od)
  pt_od$tt <- NA
  
  #Calculate travel times between zones based on NPVM values (shortest-path search with dijkstra algorithm) 
  distMatrix <- shortest.paths(pt_graph, v=V(pt_graph), to=V(pt_graph), weights=E(pt_graph)$weight)
  #Convert this distMatrix into a dataframe with columns start_zone, end_zone and tt_pt
  distMatrix <- as.data.frame(distMatrix)
  distMatrix <- distMatrix %>% 
    rownames_to_column(var = "start_zone") %>% 
    gather(end_zone, tt_pt, -start_zone) %>% 
    dplyr::rename(start_node = start_zone, end_node = end_zone, tt_pt = tt_pt)
  #Replace all 0 values with 7
  distMatrix <- distMatrix %>% 
    mutate(tt_pt = ifelse(tt_pt == 0, 7, tt_pt))
  
  pt_od <- distMatrix 
  pt_od$start_node <- as.numeric(pt_od$start_node)
  pt_od$end_node <- as.numeric(pt_od$end_node)
  #Replace all NA with 120
  pt_od$tt_pt[is.na(pt_od$tt_pt)] <- 120
  #Get the start zone and end zone from nodes_pt
  pt_od <- pt_od %>% 
    dplyr::left_join(nodes_pt, by = c("start_node" = "NO")) %>% 
    dplyr::left_join(nodes_pt, by = c("end_node" = "NO")) %>% 
    dplyr::select(start_node, end_node, tt_pt, ID_Zo.x, ID_Zo.y) %>% 
    dplyr::rename(start_zone = ID_Zo.x, end_zone = ID_Zo.y)
  pt_od <- pt_od %>% 
    dplyr::select(start_zone, end_zone, tt_pt)
  pt_od <- pt_od %>% 
    dplyr::group_by(start_zone, end_zone) %>%
    dplyr::summarise(tt_pt = mean(tt_pt))

  return(pt_od)
}

pt_od <- compute_od_pt(df_pt)

#_________________________________________________________________________________________
#Demand predictions in scenarios: 

# You want to manipulate the df_miv or df_pt dataframes which are representations of the respective network before generating the graph from them. 
#Merge pt_od and miv_od to get all OD pairs in one dataframe
od <- miv_od %>%  full_join(pt_od, by = c("start_zone", "end_zone")) %>% 
  dplyr::rename(tt_car_original = tt_car, tt_pt_original = tt_pt)
od
#Now let us look at adding a link between Altstetten and Affoltern. Straight line distance is 3.595 km -> with curves 4.0km
#Add a link between 8503008 and 8503001
df_pt <- rbind(df_pt, data.frame(NO= "850300885030001", FROMNODE = "8503008", TONODE = "105001", tt = 5.0, length = 4.0, ID_Zo = "26109"))
pt_od_new <- compute_od_pt(df_pt)    
pt_od_new 
od <- od %>% full_join(pt_od_new, by = c("start_zone", "end_zone"))
od$tt_pt_diff <- od$tt_pt_original - od$tt_pt

save(od, file = "od.RData")

#Compute the different variables
load("Assignement_4_mode_share.Rda")
mode_share <- df_furness
df_total <- od %>% 
  merge(mode_share, by=c("start_zone", "end_zone"))

#Export df_total to csv
write.csv(df_total, "df_total.csv", row.names = FALSE)

preprocess <- function(df){
  #Replace all NAs with 120
  df$tt_pt[is.na(df$tt_pt)] <- 120
  df$tt_car[is.na(df$tt_car)] <- 120
  df$tt_pt_original[is.na(df$tt_pt_original)] <- 120
  return(df)
}

compute_total_modal_split <- function(df){
  modal_split <- df %>% 
    dplyr::select(trips_furness, walk, bike, pt, car) %>% 
    dplyr::summarise(walk=weighted.mean(walk, trips_furness),
                     bike=weighted.mean(bike, trips_furness),
                     pt=weighted.mean(pt, trips_furness),
                     car=weighted.mean(car, trips_furness))
  return(modal_split)
}
compute_demand <- function(df){
  df$car_demand <- df$car * df$trips_furness
  df$pt_demand <- df$pt * df$trips_furness
  df$walk_demand <- df$walk * df$trips_furness
  df$bike_demand <- df$bike * df$trips_furness
  return(df)
}

compute_variation <- function(df){
  df$tt_car_variation <- 0.0
  df$tt_pt_variation <- df$tt_pt/df$tt_pt_original - 1
  df$cost_car_variation <- 0.0
  df$cost_pt_variation <- 0.0
  return(df)
}

compute_modal_split_variation_part_1 <- function(df){
  df$car_modal_split_variation <- df$car*(1+df$cost_car_variation*demand_elasticity_cost_car) * (1+df$tt_car_variation*demand_elasticity_tt_car)
  df$pt_modal_split_variation <- df$pt*(1+df$cost_pt_variation*demand_elasticity_cost_pt) * (1+df$tt_pt_variation*demand_elasticity_tt_pt)
  return(df)
}

compute_car_redistribution_demand_change <- function(df){
  df$var_car <- (df$car_modal_split_variation - df$car)*df$trips_furness
  df$car_redistribution_demand_change_car <- df$var_car
  df$car_redistribution_demand_change_pt <- df$pt_demand/(df$trips_furness - df$car_demand)*df$var_car*(-1)
  df$car_redistribution_demand_change_walk <- df$walk_demand/(df$trips_furness - df$car_demand)*df$var_car*(-1)
  df$car_redistribution_demand_change_bike <- df$bike_demand/(df$trips_furness - df$car_demand)*df$var_car*(-1)
  return(df)
}

compute_pt_redistribution_demand_change <- function(df){
  df$var_pt <- (df$pt_modal_split_variation - df$pt)*df$trips_furness
  df$pt_redistribution_demand_change_car <- df$car_demand/(df$trips_furness - df$pt_demand)*df$var_pt
  df$pt_redistribution_demand_change_pt <-  df$var_pt
  df$pt_redistribution_demand_change_walk <- df$walk_demand/(df$trips_furness - df$pt_demand)*df$var_pt
  df$pt_redistribution_demand_change_bike <- df$bike_demand/(df$trips_furness - df$pt_demand)*df$var_pt
  return(df)
}

compute_modal_split_variation_part_2 <- function(df){
  df$walk_modal_split_variation <- ifelse(df$car_pt_demand_variation == 0, 0, (2-df$car_pt_demand_variation)*df$walk)
  df$bike_modal_split_variation <- ifelse(df$car_pt_demand_variation == 0, 0, (2-df$car_pt_demand_variation)*df$bike)
  return(df)
}

compute_total_demand_variation <- function(df){
  df$car_pt_demand_variation <- (df$car_modal_split_variation + df$pt_modal_split_variation)/(df$car + df$pt)
  df <- compute_modal_split_variation_part_2(df)
  df$total_demand_variation <- df$car_modal_split_variation + df$pt_modal_split_variation + df$walk_modal_split_variation + df$bike_modal_split_variation
  return(df)
}


compute_relative_modal_split_variation <- function(df){
  df$car_relative_modal_split_variation <- df$car_modal_split_variation/df$total_demand_variation
  df$pt_relative_modal_split_variation <- df$pt_modal_split_variation/df$total_demand_variation
  df$walk_relative_modal_split_variation <- df$walk_modal_split_variation/df$total_demand_variation
  df$bike_relative_modal_split_variation <- df$bike_modal_split_variation/df$total_demand_variation
  return(df)
}

compute_new_demand <- function(df){
  df$car_new_demand <- (df$car_demand + df$car_redistribution_demand_change_car + df$pt_redistribution_demand_change_car)*df$total_demand_variation
  df$pt_new_demand <- (df$pt_demand + df$car_redistribution_demand_change_pt + df$pt_redistribution_demand_change_pt)*df$total_demand_variation
  df$walk_new_demand <- (df$walk_demand + df$car_redistribution_demand_change_walk + df$pt_redistribution_demand_change_walk)*df$total_demand_variation
  df$bike_new_demand <- (df$bike_demand + df$car_redistribution_demand_change_bike + df$pt_redistribution_demand_change_bike)*df$total_demand_variation
  df$car_new_demand <- ifelse(is.na(df$car_new_demand), 0, df$car_new_demand)
  df$pt_new_demand <- ifelse(is.na(df$pt_new_demand), 0, df$pt_new_demand)
  df$walk_new_demand <- ifelse(is.na(df$walk_new_demand), 0, df$walk_new_demand)
  df$bike_new_demand <- ifelse(is.na(df$bike_new_demand), 0, df$bike_new_demand)
  return(df)
}

compute_new_mode_split <- function(df){
  df$car_new_mode_split <- df$car_new_demand/(df$car_new_demand + df$pt_new_demand + df$walk_new_demand + df$bike_new_demand)
  df$pt_new_mode_split <- df$pt_new_demand/(df$car_new_demand + df$pt_new_demand + df$walk_new_demand + df$bike_new_demand)
  df$walk_new_mode_split <- df$walk_new_demand/(df$car_new_demand + df$pt_new_demand + df$walk_new_demand + df$bike_new_demand)
  df$bike_new_mode_split <- df$bike_new_demand/(df$car_new_demand + df$pt_new_demand + df$walk_new_demand + df$bike_new_demand)
  return(df)
}

compute_new_total_modal_split <- function(df){
  modal_split <- df %>% 
    dplyr::select(car_new_demand, pt_new_demand, bike_new_demand, walk_new_demand) %>% 
    dplyr::summarise(walk_new=sum(walk_new_demand),
                     bike_new=sum(bike_new_demand),
                     pt_new=sum(pt_new_demand),
                     car_new=sum(car_new_demand))
  total <- modal_split$walk_new + modal_split$bike_new + modal_split$pt_new + modal_split$car_new
  print(modal_split)
  modal_split <- modal_split/total
  return(modal_split)
}

df_total <- preprocess(df_total)
df_total <- compute_demand(df_total)
demand_elasticity_tt_car <- -0.173
demand_elasticity_cost_car <- -0.022
demand_elasticity_tt_pt <- -0.059
demand_elasticity_cost_pt <- -0.059
df_total <- compute_demand(df_total)
df_total <- compute_variation(df_total)
df_total <- compute_modal_split_variation_part_1(df_total)
df_total <- compute_car_redistribution_demand_change(df_total)
df_total <- compute_pt_redistribution_demand_change(df_total)
df_total <- compute_total_demand_variation(df_total)
df_total <- compute_relative_modal_split_variation(df_total)
df_total <- compute_new_demand(df_total)
df_total <- compute_new_mode_split(df_total)
compute_new_total_modal_split(df_total)
compute_total_modal_split(df_total)
df_total

#df_total to csv
write.csv(df_total, "df_total.csv")
write.xlsx(df_total, "df_total.xlsx")

cities = c("Zurich"=26101, 
           "Alfotern"=26111,
           "Wädenswil"=142,
           "Rafz"=67,
           "Dietikon"=243,
           "Uster"=198,
           "Regensdorf"=96,
           "Birmensdorf"=242
           )
heat_matrix <- matrix(0, nrow = length(cities), ncol = length(cities))
rownames(heat_matrix) <- names(cities)
colnames(heat_matrix) <- names(cities)
for (i in 1:length(cities)){
    for (j in 1:length(cities)){
    heat_matrix[i, j] <- df_total[df_total$start_zone == cities[i][[1]] & df_total$end_zone == cities[j][[1]], ]$tt_pt_original
  }
}
heat_matrix <- as.data.frame(heat_matrix)
heat_matrix
heat_matrix_new <- matrix(0, nrow = length(cities), ncol = length(cities))
rownames(heat_matrix_new) <- names(cities)
colnames(heat_matrix_new) <- names(cities)
for (i in 1:length(cities)){
  for (j in 1:length(cities)){
    heat_matrix_new[i, j] <- df_total[df_total$start_zone == cities[i][[1]] & df_total$end_zone == cities[j][[1]], ]$tt_pt
  }
}
heat_matrix_new <- as.data.frame(heat_matrix_new)
heat_matrix_new


plot.tt_pt_original <- ggplot(df_total[df_total$start_zone %in% cities & df_total$end_zone %in%cities, ], aes(x = start_zone, y = end_zone, fill = tt_pt_original)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient(low = "#598BF0", high = "#E85959") +  #General tweaks to the theme
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 15, title="Travel time\n(min)")) +
  geom_text(aes(label=sprintf("%.0f", tt_pt_original)), colour="white") +
  scale_x_discrete(labels = names(cities)) +
  scale_y_discrete(labels = names(cities)) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
        axis.text=element_text(size=9), #Size of axis numbering
        axis.text.x = element_text(angle = 45, hjust=1), #Tilting of axis numbering
        axis.title=element_text(size=13,face="bold"), #Size of axis title
        legend.text=element_text(size=10),
        legend.title=element_text(size=13, face="bold"),
        legend.background = element_rect(fill="white", color="black", size=0.4, linetype ="solid"),
        legend.position = "right",
        title = element_text(size=15, face="bold"),  #Size of title
        ) +
  labs(x="Start Zone", y="End Zone", title="Original travel times")
plot.tt_pt_new <- ggplot(df_total[df_total$start_zone %in% cities & df_total$end_zone %in%cities, ], aes(x = start_zone, y = end_zone, fill = tt_pt)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient(low = "#598BF0", high = "#E85959") +  #General tweaks to the theme
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 15, title="Travel time\n(min)")) +
  geom_text(aes(label=sprintf("%.0f", tt_pt)), colour="white") +
  #Change the labels to be the city names from cities vector
  scale_x_discrete(labels = names(cities)) +
  scale_y_discrete(labels = names(cities)) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
        axis.text=element_text(size=9), #Size of axis numbering
        axis.text.x = element_text(angle = 45, hjust=1), #Tilting of axis numbering
        axis.title=element_text(size=13,face="bold"), #Size of axis title
        legend.text=element_text(size=10),
        legend.title=element_text(size=13, face="bold"),
        legend.background = element_rect(fill="white", color="black", size=0.4, linetype ="solid"),
        legend.position = "right",
        title = element_text(size=15, face="bold")
        ) + #Size of title
  labs(x="Start Zone", y="End Zone", title="New travel times")

plot.heatmap <- grid.arrange(plot.tt_pt_original,
                             plot.tt_pt_new,
                             nrow=2) 
ggsave(plot.heatmap, filename = "plots/5_plot_heatmap.svg")
ggsave(plot.heatmap, filename = "plots/5_plot_heatmap.png")
