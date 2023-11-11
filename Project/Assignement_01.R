library(data.table)
library(magrittr) #Allows to use %>% 
library(dplyr)
library(tidyverse) #Make nice plots
library(gt) #Make nice tables
library(mmtable2) #Make nice tables
library(plyr) #Needed for ddply
library(wesanderson) #Nice color palette
library(svglite) #Allows for SVG
options(scipen = 999) #No scientific notation
################################################################################
#Read in the data
################################################################################
df_personen <- read.csv("zielpersonen.csv", header = TRUE, sep = ";")
df_hh <- read.csv("haushalte.csv", header=TRUE, sep=";")
df_wegeInland_hh <- read.csv("wegeinland.csv", header=TRUE, sep=";") %>%
  full_join(df_hh, by= c("HHNR"))
df_wegeInLand <- read.csv("wegeinland.csv", header=TRUE, sep=";")

################################################################################
#Task 1.1 - Average number of daily trips by urbanization degree
################################################################################
df_trips_byHHDEGURBA <- df_wegeInland_hh %>%
  mutate(w_rdist = coalesce(w_rdist, 0)) %>%
  group_by(W_DEGURBA) %>%
  dplyr::summarise(n_trips = n(), people = n_distinct(HHNR), avg_daily_trips=n_trips/people) %>% 
  mutate(W_DEGURBA = case_when(W_DEGURBA == 1 ~ "Cities",
                               W_DEGURBA == 2 ~ "Smaller Cities",
                               W_DEGURBA == 3 ~ "Countryside")) %>% 
  dplyr::rename(Density = W_DEGURBA, 
                "Number of trips" = n_trips,
                People = people,
                "Average daily trips" = avg_daily_trips)
df_trips_byHHDEGURBA %>% gt() %>% write.csv("plots/table_1_1.csv", row.names=TRUE)

################################################################################
#Task 1.2 - Average daily distance traveled by individual
################################################################################
df_trip_distance <- df_wegeInland_hh %>%
  mutate(w_rdist = coalesce(w_rdist, 0)) %>%
  mutate(WP = coalesce(WP, 0)) %>%
  group_by(HHNR) %>% 
  dplyr::summarise(W_DEGURBA = mean(W_DEGURBA), w_rdist = sum(w_rdist), WP = mean(WP)) %>% 
  group_by(W_DEGURBA) %>%
  dplyr::summarise(avg_distance = weighted.mean(w_rdist, WP)) %>% 
  mutate(W_DEGURBA = case_when(W_DEGURBA == 1 ~ "Cities",
                               W_DEGURBA == 2 ~ "Smaller Cities",
                               W_DEGURBA == 3 ~ "Countryside")) %>% 
  dplyr::rename(Density = W_DEGURBA, 
                "Average distance" = avg_distance)
df_trip_distance %>% gt() %>% write.csv("plots/table_1_2.csv", row.names=TRUE)

################################################################################
#Task 1.3 - Average number of trips reported per weekday
################################################################################
df_all <- df_wegeInland_hh %>% full_join(df_personen, by=c("HHNR", "WP"))
df_weekday <- df_all %>%
  mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  mutate(WP = coalesce(WP, 0)) %>%
  filter(!is.na(Tag)) %>%
  group_by(Tag) %>%
  dplyr::summarise(avg_distance = weighted.mean(w_rdist, WP), n_trips = n()) %>%
  mutate(Tag = case_when(Tag == 1 ~ "Monday",
                         Tag == 2 ~ "Tuesday",
                         Tag == 3 ~ "Wednesday",
                         Tag == 4 ~ "Thursday",
                         Tag == 5 ~ "Friday",
                         Tag == 6 ~ "Saturday",
                         Tag == 7 ~ "Sunday"))
df_weekday$Tag <- factor(df_weekday$Tag, 
    c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday", "Sunday")) 
plot_1_3 <- df_weekday %>% ggplot(aes(y=n_trips, x=Tag)) + 
  geom_col(fill=wes_palette("GrandBudapest1", n=1)) +
  labs(x = "Weekday", y = "Number of trips", title = "Average number of trips reported per weekday")
#Save to file
ggsave(plot_1_3, filename = "plots/plot_1_3.svg")
################################################################################
#Task 1.4 - Traveled distances by individual vs. household income
################################################################################
df_distance_income <- df_personen %>% 
  full_join(df_wegeInLand) %>% 
  mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  group_by(HHNR) %>% 
  dplyr::summarise(distance = sum(w_rdist), WP = mean(WP)) %>% 
  full_join(df_hh) %>% 
  select(c("distance", "f20601", "WP")) %>% 
  filter(f20601 > 0) %>% 
  group_by(f20601) %>% 
  dplyr::summarise(distance = weighted.mean(distance, WP)) %>% 
  mutate(f20601 = case_when(f20601 == 1 ~ "Under CHF 2000",
                            f20601 == 2 ~ "CHF 2000 to 4000",
                            f20601 == 3 ~ "CHF 4001 to 6000",
                            f20601 == 4 ~ "CHF 6001 to 8000",
                            f20601 == 5 ~ "CHF 8001 to 10000",
                            f20601 == 6 ~ "CHF 10001 to 12000",
                            f20601 == 7 ~ "CHF 12001 to 14000",
                            f20601 == 8 ~ "CHF 14001 to 16000",
                            f20601 == 9 ~ "More than to 16000"))
df_distance_income$f20601 <- factor(df_distance_income$f20601, c(
  "Under CHF 2000", "CHF 2000 to 4000", "CHF 4001 to 6000", "CHF 6001 to 8000", 
  "CHF 8001 to 10000", "CHF 10001 to 12000", "CHF 12001 to 14000",
  "CHF 14001 to 16000", "More than to 16000"))
plot_1_4 <- df_distance_income %>% ggplot(aes(x=f20601, y=distance)) + 
  geom_col(fill=wes_palette("GrandBudapest1", n=1)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  labs(x = "Household income", y = "Daily travel distance", title = "Traveled distances by individual vs. household income")
#Save to file
ggsave(plot_1_4, filename = "plots/plot_1_4.svg")
################################################################################
#Task 1.5 - Mode share by public transport-accessibility of household location
################################################################################
df_mode_location <- df_hh %>% 
  full_join(df_wegeInLand) %>% 
  group_by(wmittel2a, W_OEV_KLASSE) %>% 
  mutate(wmittel2a = case_when(wmittel2a == 1 ~ "Active mode", 
                               wmittel2a == 2 ~ "Motorized mode", 
                               wmittel2a == 3 ~ "Public transit", 
                               wmittel2a == 4 ~ "Other")) %>% 
  mutate(W_OEV_KLASSE = case_when(W_OEV_KLASSE == 1	~ "Class A: Very good accessibility",
                                  W_OEV_KLASSE == 2	~ "Class B: Good accessibility",
                                  W_OEV_KLASSE == 3	~ "Class C: Medium development",
                                  W_OEV_KLASSE == 4	~ "Class D: Low development",
                                  W_OEV_KLASSE == 5	~ "No quality class: Marginal or no public transport accessibility")) %>% 
  filter(wmittel2a > 0) %>% 
  filter(!is.na(W_OEV_KLASSE)) 

#Part a: computation with average distance of each trip

#Use to show the totals over one column
df_totals_by_class <- df_mode_location %>% 
  ddply(.(W_OEV_KLASSE), transform, people_total = n_distinct(HHNR)) %>% 
  group_by(W_OEV_KLASSE, wmittel2a) %>% 
  dplyr::summarise(distance = sum(w_rdist), people_total= mean(people_total), .groups = "keep") %>% 
  ddply(.(W_OEV_KLASSE), transform, average_distance = distance/people_total) %>% 
  group_by(W_OEV_KLASSE) %>% 
  dplyr::summarise(sum_average_distance = sum(average_distance)) %>% 
  mutate(sum_average_distance = round(sum_average_distance, 2))
  df_totals_by_class$label_x <- str_wrap(df_totals_by_class$W_OEV_KLASSE, width=20) 

#Summarise the data
df_mode_location_individual <- df_mode_location %>% 
  ddply(.(W_OEV_KLASSE), transform, people_total = n_distinct(HHNR)) %>% 
  group_by(W_OEV_KLASSE, wmittel2a) %>% 
  dplyr::summarise(distance = sum(w_rdist), people_total= mean(people_total), .groups = "keep") %>% 
  ddply(.(W_OEV_KLASSE), transform, average_distance = distance/people_total)
  
#Calculate the percentages
df_mode_location_individual <- ddply(df_mode_location_individual, .(W_OEV_KLASSE), transform, percent = average_distance/sum(average_distance) * 100)
df_mode_location_individual <- ddply(df_mode_location_individual, .(W_OEV_KLASSE), transform, pos = sum(average_distance) - cumsum(average_distance) + 0.5 * average_distance)
df_mode_location_individual$label_percentage <- paste0(sprintf("%.0f", df_mode_location_individual$percent), "%")

#Line break for the labels on the x-axis
df_mode_location_individual$labels_x <- str_wrap(df_mode_location_individual$W_OEV_KLASSE, width=20) 

#Plot for part a
plot_1_5_a <- df_mode_location_individual %>% ggplot(aes(x=labels_x, y=average_distance, fill=wmittel2a)) +
  geom_bar(stat = "identity") +
  #Total of each bar label
  geom_text(aes(label_x, sum_average_distance, label = sum_average_distance, fill = NULL), 
            data = df_totals_by_class, 
            vjust=-0.5, #Adjust it to slightly higher
            size = 3) +
  #Percentage labels
  geom_text(aes(y = pos, label = label_percentage), 
            size = 3, 
            color="white") +
  #Color of the different sections
  scale_fill_manual(values=wes_palette("GrandBudapest1", n=4, type = "discrete"), 
                    name="Transport mode") + #Name of the legend
  #General tweaks to the theme
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
        panel.grid.major.y = element_line(color="gray", size=0.4),
        axis.text=element_text(size=9), #Size of axis numbering
        axis.text.x = element_text(angle = 45, hjust=1), #Tilting of axis numbering
        axis.title=element_text(size=13,face="bold"), #Size of axis title
        legend.text=element_text(size=10), 
        legend.title=element_text(size=13, face="bold"),
        legend.background = element_rect(fill="white", color="black", size=0.4, linetype ="solid"),
        legend.position = "right",
        title = element_text(size=15, face="bold")) + #Size of title
  
  labs(x="Public transport accessibility", y="Average distance per person", title="Mode share vs. public transport accesibility")
#Save to file
ggsave(plot_1_5_a, filename = "plots/plot_1_5_a.svg")

#Part b: computation with pkm

#Use to show the totals over one column
df_totals_by_class <- df_mode_location %>% 
  mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  group_by(W_OEV_KLASSE) %>% 
  filter(wmittel2a > 0) %>%
  filter(!is.na(W_OEV_KLASSE)) %>% 
  dplyr::summarise(distance = sum(w_rdist)) %>%
  mutate(distance = round(distance, 0))

df_totals_by_class$label_x <- str_wrap(df_totals_by_class$W_OEV_KLASSE, width=20) 

#Summarise the data
df_mode_location_pkm <- df_mode_location %>% 
  group_by(W_OEV_KLASSE, wmittel2a) %>% 
  dplyr::summarise(distance = sum(w_rdist), .groups = "keep") 
  
#Calculate the percentages
df_mode_location_pkm <- ddply(df_mode_location_pkm, .(W_OEV_KLASSE), transform, percent = distance/sum(distance) * 100)
df_mode_location_pkm <- ddply(df_mode_location_pkm, .(W_OEV_KLASSE), transform, pos = sum(distance) - cumsum(distance) + 0.5 * distance)
df_mode_location_pkm$label_percentage <- paste0(sprintf("%.0f", df_mode_location_pkm$percent), "%")

#Line break for the labels on the x-axis
df_mode_location_pkm$labels_x <- str_wrap(df_mode_location_pkm$W_OEV_KLASSE, width=20) 

#Plot for part b
plot_1_5_b <- df_mode_location_pkm %>% ggplot(aes(x=labels_x, y=distance, fill=wmittel2a)) +
  geom_bar(stat = "identity") +
  #Total of each bar label
  geom_text(aes(label_x, distance, label = distance, fill = NULL), 
            data = df_totals_by_class, 
            vjust=-0.5, #Adjust it to slightly higher
            size = 3) +
  #Percentage labels
  geom_text(aes(y = pos, label = label_percentage), 
            size = 3, 
            color="white") +
  #Color of the different sections
  scale_fill_manual(values=wes_palette("GrandBudapest1", n=4, type = "discrete"), 
                    name="Transport mode") + #Name of the legend
  #General tweaks to the theme
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
        panel.grid.major.y = element_line(color="gray", size=0.4),
        axis.text=element_text(size=9), #Size of axis numbering
        axis.text.x = element_text(angle = 45, hjust=1), #Tilting of axis numbering
        axis.title=element_text(size=13,face="bold"), #Size of axis title
        legend.text=element_text(size=10), 
        legend.title=element_text(size=13, face="bold"),
        legend.background = element_rect(fill="white", color="black", size=0.4, linetype ="solid"),
        legend.position = "right",
        title = element_text(size=15, face="bold")) + #Size of title
  
  labs(x="Public transport accessibility", y="Person km", title="Mode share vs. public transport accesibility")
#Save to file
ggsave(plot_1_5_b, filename = "plots/plot_1_5_b.svg")
################################################################################
#Task 1.6 - Mode share by trip purpose
################################################################################
df_mode_purpose <- df_hh %>% 
  full_join(df_wegeInLand) %>% 
  group_by(wmittel2a, wzweck3) %>% 
  mutate(wmittel2a = case_when(wmittel2a == 1 ~ "Active mode", 
                               wmittel2a == 2 ~ "Motorized mode", 
                               wmittel2a == 3 ~ "Public transit", 
                               wmittel2a == 4 ~ "Other")) %>% 
  mutate(wzweck3 = case_when(wzweck3 == 1 ~ "Change, change means of transport, park car",
                                  wzweck3 == 2 ~ "Work",
                                  wzweck3 == 3 ~ "Education, School",
                                  wzweck3 == 4 ~ "Shopping",
                                  wzweck3 == 5 ~ "Errands and use of services",
                                  wzweck3 == 6 ~ "Business activity",
                                  wzweck3 == 7 ~ "Service trip",
                                  wzweck3 == 8 ~ "Leisure activity",
                                  wzweck3 == 9 ~ "Accompanying path (children only)",
                                  wzweck3 == 10	~ "Accompanying path/service path",
                                  wzweck3 == 11	~ "Return home or accommodation away from home",
                                  wzweck3 == 12	~ "Other",
                                  wzweck3 == 13	~ "Border crossing")) %>% 
  filter(wmittel2a > 0) %>% 
  filter(wzweck3 > 0) %>% 
  filter(!is.na(wzweck3)) 

#Summarise the data
df_mode_purpose_sum <- df_mode_purpose %>% 
  group_by(wmittel2a, wzweck3) %>% 
  dplyr::summarise(distance = sum(w_rdist), .groups = "keep") 

#Use to show the totals over one column
df_totals_by_class <- df_mode_purpose %>% 
  mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  group_by(wzweck3) %>% 
  filter(wmittel2a > 0) %>%
  filter(!is.na(wzweck3)) %>% 
  dplyr::summarise(distance = sum(w_rdist)) %>%
  mutate(distance = round(distance, 0))

df_totals_by_class$label_x <- str_wrap(df_totals_by_class$wzweck3, width=20) 

#Calculate the percentages
df_mode_purpose_sum <- ddply(df_mode_purpose_sum, .(wzweck3), transform, percent = distance/sum(distance) * 100)
df_mode_purpose_sum <- ddply(df_mode_purpose_sum, .(wzweck3), transform, pos = sum(distance) - cumsum(distance) + 0.5 * distance)
df_mode_purpose_sum$label_percentage <- paste0(sprintf("%.0f", df_mode_purpose_sum$percent), "%") 
#Remove percentages except for motorized mode
df_mode_purpose_sum <- dplyr::mutate(df_mode_purpose_sum, label_percentage = ifelse(wmittel2a %in% c("Motorized mode"), label_percentage, NA)) 

#Line break for the labels on the x-axis
df_mode_purpose_sum$labels_x <- str_wrap(df_mode_purpose_sum$wzweck3, width=20) 

#Plot 
plot_1_6 <- df_mode_purpose_sum %>% ggplot(aes(x=labels_x, y=distance, fill=wmittel2a)) +
  geom_bar(stat = "identity") +
  #Total of each bar label
  geom_text(aes(label_x, distance, label = distance, fill = NULL), 
            data = df_totals_by_class, 
            vjust=-0.5, #Adjust it to slightly higher
            size = 3) +
  #Percentage labels
  geom_text(aes(y = pos, label = label_percentage), 
            size = 3, 
            color="white") +
  #Color of the different sections
  scale_fill_manual(values=wes_palette("GrandBudapest1", n=4, type = "discrete"), 
                    name="Transport mode") + #Name of the legend
  #General tweaks to the theme
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
        panel.grid.major.y = element_line(color="gray", size=0.4),
        axis.text=element_text(size=9), #Size of axis numbering
        axis.text.x = element_text(angle = 45, hjust=1), #Tilting of axis numbering
        axis.title=element_text(size=13,face="bold"), #Size of axis title
        legend.text=element_text(size=10),
        legend.title=element_text(size=13, face="bold"),
        legend.background = element_rect(fill="white", color="black", size=0.4, linetype ="solid"),
        legend.position = "right",
        title = element_text(size=15, face="bold")) + #Size of title
  labs(x="Trip purpose", y="Person km", title="Mode share vs. trip purpose")
#Save to file
ggsave(plot_1_6, filename = "plots/plot_1_6.svg")
plot_1_6
  

