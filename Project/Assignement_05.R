library(tidyverse)
library(sf)
library(lwgeom)

#-----------------------------------------------------------------------------------START_ 

#1-Read in the four shapefiles:
links_pt <- read_sf("Data/Shapefiles/links_PTNet.shp")
links_car <- read_sf("Data/Shapefiles/links_carNet.shp")
nodes_pt <- read_sf("Data/Shapefiles/nodes_PTNet.shp")
nodes_car <- read_sf("Data/Shapefiles/nodes_carNet.shp")
npvm_zones <- read_sf("NPVM2010MetroZH.shp")
npvm_zones <- st_transform(npvm_zones, crs=2056) #transform from LV95 to WGS 84 coordinates
npvm_zones_2 <- st_union(st_buffer(npvm_zones,100000))

links_pt <- st_intersection(links_pt, npvm_zones_2)
links_car <- st_intersection(links_car, npvm_zones_2)
nodes_pt <- st_intersection(nodes_pt, npvm_zones_2)
nodes_car <- st_intersection(nodes_car, npvm_zones_2)

links_car$start_coord <- st_startpoint(links_car$geometry)
links_car$end_coord <- st_endpoint(links_car$geometry)
links_car <- st_as_sf(links_car, coords=c("start_coord", "end_coord"),
                      crs=2056) #WGS 84 coordinates
#Remove geometry column form links_car
links_car <- links_car[,c("NO", "start_coord", "end_coord", "VOL_TOT", "GESCHW", "KAPAZIT")]

links_car <- links_car %>%
  mutate(start_zone=map_dbl(st_intersects(links_car, npvm_zones), first)) %>%
  relocate(start_zone) %>%
  mutate(end_zone=map_dbl(st_intersects(links_car, npvm_zones), last)) %>%
  relocate(end_zone) %>%
  mutate(start_zone=as.integer(start_zone)) %>%
  mutate(end_zone=as.integer(end_zone)) %>%
  filter(!is.na(start_zone)) %>%
  filter(!is.na(end_zone))

#Now we do the same for PT links:
links_pt$start_coord <- st_startpoint(links_pt$geometry)
links_pt$end_coord <- st_endpoint(links_pt$geometry)
links_pt <- st_as_sf(links_pt, coords=c("start_coord", "end_coord"),
                     crs=2056) #WGS 84 coordinates
#Remove geometry column form links_car
links_pt <- links_pt[, c("NO", "start_coord", "end_coord", "SYSTEMNO", "LENGHT")]

#Intersect links car with npvm zones to get start and end zones for each link
links_pt <- links_pt %>%
  mutate(start_zone=map_dbl(st_intersects(links_pt, npvm_zones), first)) %>%
  relocate(start_zone) %>%
  mutate(end_zone=map_dbl(st_intersects(links_pt, npvm_zones), last)) %>%
  relocate(end_zone) %>%
  mutate(start_zone=as.integer(start_zone)) %>%
  mutate(end_zone=as.integer(end_zone)) %>%
  filter(!is.na(start_zone)) %>%
  filter(!is.na(end_zone))

#Calculate speeds in car network links based on demand: 
#____________________________________________________________________________________________________
#DO NOT CHANGE THIS CODE:
df_miv <- sf::st_drop_geometry(links_car)

df_miv$length <- sf::st_length(links_car)/1000

#BPR Function application
df_miv$Bel_GESCHW <- df_miv$GESCHW/(1+0.32*(df_miv$VOL_TOT/df_miv$KAPAZIT)^3.27)*1.1
df_miv$Bel_GESCHW <- ifelse(is.na(df_miv$Bel_GESCHW),40, df_miv$Bel_GESCHW)

df_miv$tt <- df_miv$length/df_miv$Bel_GESCHW*60 #in minutes
df_miv <- df_miv[!duplicated(df_miv$NO),]

#____________________________________________________________________________________________________
#DO NOT CHANGE THIS CODE:
#now for PT (simplistic method with standard speed for Nahverkehr vs. Vollbahn: 
df_pt<- sf::st_drop_geometry(links_pt)
df_pt$length <- as.numeric(substr(df_pt$LENGHT,1,nchar(df_pt$LENGHT)-2))

commercial_speed_bus_tram <- 15
commercial_speed_train <- 35

#THERE IS A MISTAKE HERE
#Assign speeds based on fastest available PT mode
df_pt <- df_pt %>%
  mutate(speed=if_else(grepl("FV", SYSTEMNO, fixed=TRUE) | grepl("RV", SYSTEMNO, fixed=TRUE) ,
                       commercial_speed_train,
                       commercial_speed_bus_tram
  ))
df_pt$tt <- ((df_pt$length)/(df_pt$speed))*60 #in minutes
#____________________________________________________________________________________________________



#_______________________________________________________________________________________________________
#Now prepare graphs for running the dijkstra algorithm on. 

library(igraph)

library(units)
#Remove units to allow assignment of test value
df_miv$tt <- drop_units(df_miv$tt)

df_miv$end_zone <- as.character(df_miv$end_zone)
df_miv$start_zone <- as.character(df_miv$start_zone)
df_miv$tt <- as.numeric(df_miv$tt)

#Generate igraph
#q: This graph should not be disconnected ...

car_graph <- graph_from_data_frame(df_miv, directed = FALSE)
component_distribution(car_graph)

#provide travel times as weight:
E(car_graph)$weight <- as.numeric(df_miv$tt)

#Add all the start and end zone nodes combinations to a list to calculate shortest paths for all OD pairs
miv_od <- expand.grid(start_zone=unique(df_miv$start_zone), end_zone=unique(df_miv$end_zone))
miv_od$tt <- NA
#Calculate travel times between zones based on NPVM values (shortest-path search with dijkstra algorithm) 
miv_od
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
  path <- shortest_paths(car_graph, from=target_node, to = source_node, weights=E(car_graph)$weight)
  path_weight_sum <- min(path_weight_sum, sum(E(car_graph, path=path$vpath[[1]])$weight))
  miv_od[i,]$tt <- path_weight_sum
  
}
miv_od
#_________________________________________________________________________________________
#Now do the same steps above for the PT graph to calculate PT travel times between zones
df_pt$end_zone <- as.character(df_pt$end_zone)
df_pt$start_zone <- as.character(df_pt$start_zone)
df_pt$tt <- as.numeric(df_pt$tt)

#Generate igraph
pt_graph <- graph_from_data_frame(df_pt[,c("start_zone","end_zone", "tt")], directed = FALSE)
component_distribution(pt_graph)

#provide travel times as weight:
E(pt_graph)$weight <- as.numeric(df_pt$tt)  

#Add all the start and end zone nodes combinations to a list to calculate shortest paths for all OD pairs
pt_od <- expand.grid(start_zone=unique(df_pt$start_zone), end_zone=unique(df_pt$end_zone))
pt_od$tt <- NA
#Calculate travel times between zones based on NPVM values (shortest-path search with dijkstra algorithm) 

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nrow(miv_od), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "|")   # Character used to create the bar

#Find shortest paths for all OD pairs: 
for(i in 1:nrow(pt_od)){
  setTxtProgressBar(pb, i)  
  source_node <- as.character(pt_od[i,1])
  target_node <- as.character(pt_od[i,2])
  
  path <- shortest_paths(pt_graph, from=source_node, to = target_node, weights=E(pt_graph)$weight)
  path_weight_sum <- sum(E(pt_graph, path=path$vpath[[1]])$weight)
  
  pt_od[i,]$tt <- path_weight_sum
  
}
pt_od

#Merge the two dataframes to get a combined PT and MIV OD matrix:
od_matrix <- merge(miv_od, pt_od, by=c("start_zone", "end_zone"))
od_matrix %>% dplyr::rename(tt_car = tt.x, tt_pt = tt.y)
#_________________________________________________________________________________________
#Demand predictions in scenarios: 

# You want to manipulate the df_miv or df_pt dataframes which are representations of the respective network before generating the graph from them. 

