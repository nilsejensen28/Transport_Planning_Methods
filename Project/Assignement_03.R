library(data.table)
library(tidyverse) #Make nice plots
library(gt) #Make nice tables
#library(mmtable2) #Make nice tables
library(wesanderson) #Nice color palette
library(svglite) #Allows for SVG
library(MASS)
library(plyr) #Needed for ddply
library(magrittr) #Allows to use %>%
library(dplyr)
library(glmnet)
library(sf)
library(ggplot2)

# options(scipen = 999) #No scientific notation
############################################################################
#load Data
#############################################################################
df_wegeInland <- read.csv("wegeinland.csv", header=TRUE, sep=";")
npvm_zones <- read_sf("NPVM2010MetroZH.shp")

#Prepare wege, create an ID
#__________________________
#Generate an id for trips
df_wegeInland <- df_wegeInland %>%
  dplyr::mutate(weg_id = row_number()) %>%
  relocate(weg_id)

#COnvert to SF to get the trips within the study area
wege_Start <- st_as_sf(df_wegeInland, coords=c("S_X", "S_Y"),
                       crs=4326) #WGS 84 coordinates

#st_intersects instead of st_intersection for speed. 
npvm_zones <- st_transform(npvm_zones, crs=4326) #transform from LV95 to WGS 84 coordinates

zone_Start <- wege_Start %>%
  mutate(start_row.id=(st_intersects(wege_Start$geometry, npvm_zones))) %>%
  relocate(start_row.id) %>%
  mutate(start_row.id=as.integer(start_row.id)) %>%
  filter(!is.na(start_row.id)) 


wege_Ziel <- st_as_sf(df_wegeInland, coords=c("Z_X", "Z_Y"),
                      crs=4326)


zone_Ziel <- wege_Ziel %>%
  mutate(ziel_row.id=(st_intersects(wege_Ziel$geometry, npvm_zones))) %>%
  relocate(ziel_row.id) %>%
  mutate(ziel_row.id=as.integer(ziel_row.id)) %>%
  filter(!is.na(ziel_row.id))


wege <- df_wegeInland %>%
  left_join(zone_Start[,c("weg_id", "start_row.id")], by="weg_id") %>%
  left_join(zone_Ziel[,c("weg_id", "ziel_row.id")], by="weg_id") 


#Filter the original wege 
wege <- wege[wege$weg_id %in% zone_Start$weg_id,]
wege <- wege[wege$weg_id %in% zone_Ziel$weg_id,]

wege_Start <- wege_Start[wege_Start$weg_id %in% wege$weg_id, ]
wege_Ziel <- wege_Ziel[wege_Ziel$weg_id %in% wege$weg_id, ]

s_x <- wege_Start$geometry
z_x <- wege_Ziel$geometry

#create lines showing the trips
create_linestring <- function(point1, point2) {
  line <- st_linestring(rbind(point1, point2))
  return(line)
}

lines_sf <- mapply(create_linestring, s_x, z_x, SIMPLIFY = FALSE)
lines_sf <- st_sf(geometry = st_sfc(lines_sf))
lines_sf <- st_set_crs(lines_sf, 4326)

#__________________________
#1.Plot the zones colored by population
ggplot()+geom_sf(data=npvm_zones, aes(fill=ZonePop))

#2.Plot the the zones with lines of the OD-trips 

ggplot()+geom_sf(data=npvm_zones, aes(fill=ZonePop))+geom_sf(data=lines_sf)
#That's a mess. 


#For our model now, the exact OD's are not relevant anymore, we want to move to a zone-level. 

#Calculate geographical centroids for display: 
npvm_centroids <- npvm_zones %>%
  st_centroid()
#This is a simplification. The actual centroid in a transport model should be the center of activities or inhabitants, not the geographical center of a political boundary. 


#Aggregate wege at zone level :

#Already calculate the travel times but don't include negative travel times: 

wege$dauer2[wege$dauer2 < 0] <- NA    #wege[wege$dauer2<0] <- NA

wegeOD <- wege %>%
  group_by(start_row.id, ziel_row.id) %>% #row.id are the response of the st_intersects function, which gives us as a result the row of the npvm_zones dataframe (eg. the zone) which intersects the starting point of each trip
  dplyr::summarise(trips=n(), travelTime=median(dauer2, na.rm=T)) %>%
  na.omit()


#Now create linestrings for display: 
npvm_cent_start <- npvm_centroids
npvm_cent_end <- npvm_centroids

wegeOD$start <- left_join(wegeOD, npvm_cent_start, by=c("start_row.id"="row_id"))
wegeOD$end <- left_join(wegeOD, npvm_cent_end, by=c("ziel_row.id"="row_id"))

#Now write code to create the linestrings with the OD demand between zones and plot them weighting the lines according to the number of trips. 
od_lines_sf <- mapply(create_linestring, wegeOD$start$geometry, wegeOD$end$geometry,
                      SIMPLIFY = FALSE)
od_lines_sf <- st_sf(geometry = st_sfc(od_lines_sf))
od_lines_sf <- st_set_crs(od_lines_sf, 4326)
od_lines_sf <- od_lines_sf %>%
  mutate(trips = wegeOD$trips)

od_lines_sf <- od_lines_sf %>%
  filter(trips > 5)

ggplot()+
  geom_sf(data=npvm_zones, aes(fill=ZonePop))+
  geom_sf(data=od_lines_sf, aes(linewidth = trips))


#Not to overcrowd the graph you might wanna exclude those with very little observations. 
ggplot() +
  geom_sf(data = npvm_zones, aes(fill = ZonePop)) +
  geom_sf(data = od_lines_sf, aes(linewidth = trips)) +
  scale_linewidth_continuous(range = c(0,10))

#Export od shapefile
write_sf(od_lines_sf, "old_lines", driver = "GeoJSON")
write_sf(lines_sf, "new_lines", driver = "GeoJSON")

#-----NOW TO THE FURNESS METHOD-----

#Now calculate the travel time matrix between the zones from the trips data. 

#Check number of employments

sum(npvm_zones$ZoneVZA)
sum(npvm_zones$ZonePop)

#1-first check consistency of the start and end zones of the trips (all starts gave to have ends): 
factor <- 1 #choose here the scaling factor that estimates how many of the employees actually live in the area. 
scaling <- sum(npvm_zones$ZoneVZA)/sum(npvm_zones$ZonePop)#This scaling factor is used to ensure that all trips that start have an end in our model

npvm_zones$Empl_Furness <- factor * npvm_zones$ZoneVZA / scaling
npvm_zones$Pop_Furness <- factor * npvm_zones$ZonePop

sum(npvm_zones$Empl_Furness)
sum(npvm_zones$Pop_Furness)

#Now calculate average costs between ALL the zones. These are the generalized travel costs (GC) which we here simplify as being only travel time. 

#First prepare a dataframe for that
gc_trips <- npvm_zones %>%
  tidyr::expand(row_id, row_id) 
colnames(gc_trips) <- c("start_row.id", "ziel_row.id")

#Get data from the OD matrix of trips 
gc_trips <- left_join(gc_trips, wegeOD[,1:4], by=c("start_row.id", "ziel_row.id"))

#There are not travel times available for all of the OD pairs (see NA's). We have to fill those.
#We will use dodgr for this, a fast and simple way for calculating shortest paths in R: 

#https://cran.r-project.org/web/packages/dodgr/vignettes/dodgr.html

library(dodgr)
#Filter those that have no travel times.
noTravelTimes <- gc_trips[is.na(gc_trips$travelTime),]
noTravelTimes <- left_join(noTravelTimes, npvm_centroids[,c("row_id", "geometry")], by=c("start_row.id"="row_id"))
noTravelTimes <- left_join(noTravelTimes, npvm_centroids[,c("row_id", "geometry")], by=c("ziel_row.id"="row_id"))

#Get the Dodgr graph
coordsOrigin <- do.call(rbind, noTravelTimes$geometry.x) %>% 
   as.data.frame() %>% setNames(c("lon","lat"))
 
coordsDest <- do.call(rbind, noTravelTimes$geometry.y) %>% 
   as.data.frame() %>% setNames(c("lon","lat"))

library(osmextract)

#ATTENTION: Download the switzerland-latest.osm.pbf OSM extract from geofabrik. 
#You can also directly download OSM highway data using osmextract in R by providing a bounding box, but this is not always very stable. 

#ATTENTION 2: The next steps are very memory intensive. Ideally you want to use a notebook/PC with 16GB of RAM at least. Try the Bauwelt PC's if yours doesn't. If that's not the case let me know and I will provide you the file. 
network <-osmextract::oe_read("switzerland-latest.osm.pbf", layer="lines")
#Anstatt aus einer pbf-Datei das Netz zu erstellen, kann man es auch direkt als Silicate über Overpass API herunterladen (darf nicht zu gross sein): siehe Zeile 68
network <- network[network$highway %in% c("motorway_link", "secondary", "tertiary", "primary", "motorway", "road", "residential", "unclassified"), ]

gc()
#Now prepare for routing: 
library(dodgr)
library(geodist)
network <- weight_streetnet(network, wt_profile = "motorcar")
#----  
#network <- dodgr::dodgr_streetnet_sc(pts = coordsOrigin, quiet=FALSE)
#network_car <- weight_streetnet(network, wt_profile = "motorcar")

noTravelTimes <- noTravelTimes[is.na(noTravelTimes$travelTime),]
RcppParallel::setThreadOptions (numThreads = 10L) # or desired number
tt_noTravelTimes <- (dodgr_times(graph=network, from=coordsOrigin, to=coordsDest)) #this takes a while (hours).
#save(tt_noTravelTimes,file="tt_noTravelTimes.Rda")
load(file="tt_noTravelTimes.Rda")
tt_noTravelTimes2 <- as.data.frame(tt_noTravelTimes)
tt_noTravelTimes2 <- tt_noTravelTimes2[,-1]

tt_noTravelTimes2$row.id <- noTravelTimes$start_row.id
tt_noTravelTimes2

#Fill the gaps. 
for (k in 1:nrow(gc_trips)){
  
  if(is.na(gc_trips[k, "trips"])){
    
    i=as.numeric(gc_trips[k,"start_row.id"])
    j=as.numeric(gc_trips[k,"ziel_row.id"])
    
    gc_trips[k,"travelTime"]=tt_noTravelTimes2[tt_noTravelTimes2$row.id==i,j][1]
    
  }
}


#Transform new travel times into minutes
gc_trips$travelTime <- ifelse(is.na(gc_trips$trips), gc_trips$travelTime/60, gc_trips$travelTime)
gc_trips$travelTime <- ifelse(is.na(gc_trips$trips), gc_trips$travelTime*1.25, gc_trips$travelTime) #Correct for systematic underestimation of travel time from dodgr (based on my experience)

#What are the methodological problems with the dodgr approach? Think about what mode is being considered here. 


#Now write a function that applies the Furness method!
#First: set an arbitrary value for travel times 
zones <- npvm_zones$row_id
n <- length(zones)
gc <- matrix(0, n, n)
sub_vector <- gc_trips[gc_trips$start_row.id==1,]

for (i in 1:n){
  sub_vector <- gc_trips[gc_trips$start_row.id==i,]
  for (j in 1:n){
    gc[i, j] <- sub_vector[sub_vector$ziel_row.id==j, "travelTime"][[1]]
  }
}

# Furness Method 
ipf <- function(zones, outgoing, incoming, gcosts){ #Assumes gc is in matrix format!
  zones <- as.numeric(zones)
  # number of zones
  n <- length(zones)
  
  # balancing factors initialisation ()
  alpha_o_new <- rep(1, n)
  alpha_d_new <- rep(1, n)
  
  # del is the coefficient assessing the changes of beta after each iteration
  del <- 1
  # eps is the boundry for ending the iteration
  error_bound <- 0.0000000000001
  
  # vectors containing sum of rows (origins) and columns (destinations)
  sum_rows <- outgoing
  sum_cols <- incoming
  
  # matrix with flows
  trips <- matrix(1, n+1, n+1)
  
  # iteration
  iteration <- 1L
  
  repeat {
    for (i in 1:n) {
      alpha_o_new[i] <- sum_rows[i]/(sum(alpha_d_new*gcosts[i,]))
    }
    for (i in 1:n) {
      alpha_d_new[i] <- sum_cols[i]/(sum(alpha_o_new*gcosts[,i]))
    }
    # Compute the flows
    for (i in 1:n) {
      for (j in 1:n){
        trips[i, j] = gcosts[i, j]*alpha_o_new[i]*alpha_d_new[j]
      }
    }
    trips[1:n,n+1] <- rowSums(trips[1:n,1:n])
    trips[n+1, 1:n] <- colSums(trips[1:n, 1:n])
    error <- 0.0
    for(i in 1:n){
      error = error + abs(trips[i, n+1] - sum_rows[i])/sum_rows[i]
    }
    if(error < error_bound){
      return(list(iteration=iteration,trips=trips,alpha_o=alpha_o_new,alpha_o_d=alpha_d_new))
    }
  }
}

#Apply the function
distribution <- ipf(zones, as.matrix(npvm_zones$Pop_Furness), as.matrix(npvm_zones$Empl_Furness), gc)

trips_matrix <- distribution["trips"]$trips
trip_df <- data.frame("start_row.id" = numeric(), "ziel_row.id"= numeric(), "trips"= numeric())
trip_df
for (i in 1:n){
  for(j in 1:n){
    trip_df <- trip_df %>% 
      dplyr::add_row(start_row.id=i, ziel_row.id=j, trips=trips_matrix[i, j])
  }
}

wegeOD$start <- left_join(wegeOD, npvm_cent_start, by=c("start_row.id"="row_id"))
wegeOD$end <- left_join(wegeOD, npvm_cent_end, by=c("ziel_row.id"="row_id"))

od_lines_sf <- mapply(create_linestring, trip_df$start$geometry, trip_df$end$geometry,
                      SIMPLIFY = FALSE)
od_lines_sf <- st_sf(geometry = st_sfc(od_lines_sf))
od_lines_sf <- st_set_crs(od_lines_sf, 4326)
od_lines_sf <- od_lines_sf %>%
  mutate(trips = wegeOD$trips)

od_lines_sf <- od_lines_sf %>%
  filter(trips > 5)

ggplot()+
  geom_sf(data=npvm_zones, aes(fill=ZonePop))+
  geom_sf(data=od_lines_sf, aes(linewidth = trips))

#Now check how well the algorithm worked for representing the trips....