library(data.table)
library(magrittr) #Allows to use %>% 
library(dplyr)
library(tidyverse) #Make nice plots
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

color_fill <- wes_palette("Royal1", n=2)[2] 
colors_4 <- wes_palette("Royal1", n=4)
################################################################################
#Task 1.1 - Average number of daily trips by urbanization degree
################################################################################
df_num_wege <- df_personen %>% 
  dplyr::select(HHNR, WP, T_WEGE) %>%
  full_join(df_hh, by=c("HHNR"))
df_trips_byHHDEGURBA <- df_num_wege %>%
  group_by(W_DEGURBA) %>%
  dplyr::summarise(avg_trips = weighted.mean(T_WEGE, WP)) %>%
  mutate(W_DEGURBA = case_when(W_DEGURBA == 1 ~ "Cities",
                               W_DEGURBA == 2 ~ "Smaller Cities",
                               W_DEGURBA == 3 ~ "Countryside"))
df_trips_byHHDEGURBA$W_DEGURBA <- factor(df_trips_byHHDEGURBA$W_DEGURBA, 
    c("Cities", "Smaller Cities", "Countryside"))
#Count the number of individuals without any trips in each W_DEGURBA
df_no_trips <- df_num_wege %>%
  group_by(W_DEGURBA) %>%
  dplyr::summarise(n_no_trips = sum(T_WEGE == 0)) %>%
  mutate(W_DEGURBA = case_when(W_DEGURBA == 1 ~ "Cities",
                               W_DEGURBA == 2 ~ "Smaller Cities",
                               W_DEGURBA == 3 ~ "Countryside"))
#Count the number of individuals in each W_DEGURBA
df_num_indiv <- df_num_wege %>%
  group_by(W_DEGURBA) %>%
  dplyr::summarise(n_indiv = n()) %>%
  mutate(W_DEGURBA = case_when(W_DEGURBA == 1 ~ "Cities",
                               W_DEGURBA == 2 ~ "Smaller Cities",
                               W_DEGURBA == 3 ~ "Countryside"))
#Merge all these dataframes
df_trips_byHHDEGURBA <- df_trips_byHHDEGURBA %>%
  full_join(df_no_trips, by=c("W_DEGURBA")) %>%
  full_join(df_num_indiv, by=c("W_DEGURBA")) 
df_trips_byHHDEGURBA
#Add a row "overall"
df_trips_byHHDEGURBA <- df_trips_byHHDEGURBA %>% 
  rbind(c("Overall", weighted.mean(df_trips_byHHDEGURBA$avg_trips, df_trips_byHHDEGURBA$n_indiv),
          sum(df_trips_byHHDEGURBA$n_no_trips), sum(df_trips_byHHDEGURBA$n_indiv)))
df_trips_byHHDEGURBA  %>% write.csv("plots/01_Plots/table_1_1.csv", row.names=TRUE)
df_trips_byHHDEGURBA
################################################################################
#Task 1.2 - Average daily distance traveled by individual
################################################################################
df_trip_distance <- df_wegeInLand %>%
  mutate(w_rdist = coalesce(w_rdist, 0)) %>%
  mutate(WP = coalesce(WP, 0)) %>%
  group_by(HHNR) %>% 
  dplyr::summarise(dist = sum(w_rdist))
#Add the HHNR without any trips
df_trip_distance <- df_trip_distance %>%
  right_join(df_personen, by=c("HHNR")) %>%
  mutate(dist = coalesce(dist, 0)) %>%
  mutate(WP = coalesce(WP, 0)) %>%
  mutate(dist = ifelse(is.na(dist), 0, dist))
df_distance_by_hh <- df_trip_distance
df_trip_distance <- df_trip_distance %>%
  full_join(df_hh, by=c("HHNR")) %>%
  dplyr::select(HHNR, WP, dist, W_DEGURBA)
#Calculate the average distance traveled per W_DEGURBA
df_trip_distance <- df_trip_distance %>%
  group_by(W_DEGURBA) %>%
  dplyr::summarise(avg_distance = weighted.mean(dist, WP), num_trips=sum(WP)) %>%
  mutate(W_DEGURBA = case_when(W_DEGURBA == 1 ~ "Cities",
                               W_DEGURBA == 2 ~ "Smaller Cities",
                               W_DEGURBA == 3 ~ "Countryside"))
df_trip_distance <- df_trip_distance %>%
  rbind(c("Overall", weighted.mean(df_trip_distance$avg_distance, df_trip_distance$num_trips), sum(df_trip_distance$num_trips)))
df_trip_distance
df_trip_distance %>% write.csv("plots/01_Plots/table_1_2.csv", row.names=TRUE)

################################################################################
#Task 1.3 - Average number of trips reported per weekday
################################################################################
df_weekday <- df_personen %>% 
  dplyr::group_by(Tag) %>%
  dplyr::summarise(n_trips = weighted.mean(T_WEGE, WP)) %>%
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
  geom_col(fill=color_fill) +
  labs(x = "Weekday", y = "Number of trips", title = "Average number of trips per weekday per person") +
  geom_text(aes(label = sprintf("%0.2f", round(n_trips, digits = 2))), 
            size = 3.5, vjust = -0.5,
            color="black") +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
        panel.grid.major.y = element_line(color="gray", size=0.4),
        axis.text=element_text(size=11), #Size of axis numbering
        axis.text.x = element_text(angle = 0, hjust=0.5), #Tilting of axis numbering
        axis.title=element_text(size=12), #Size of axis title
        title = element_text(size=14, face="bold"), #Size of title
        legend.position="bottom", #Position of legend
        )
#Save to file
ggsave(plot_1_3, filename = "plots/01_Plots/1_plot_trips_per_weekday.svg")
ggsave(plot_1_3, filename = "plots/01_Plots/1_plot_trips_per_weekday.pdf")
ggsave(plot_1_3, filename = "plots/01_Plots/1_plot_trips_per_weekday.png")
plot_1_3
################################################################################
#Task 1.4 - Traveled distances by individual vs. household income
################################################################################
df_distance_income <- df_personen %>% 
  full_join(df_wegeInLand) %>% 
  mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  group_by(HHNR) %>% 
  dplyr::summarise(distance = sum(w_rdist, WP), WP=mean(WP)) %>% 
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
  geom_col(fill=color_fill) +
  labs(x = "Household income", y = "Daily travel distance", title = "Traveled distances by individual vs. household income") +
  geom_text(aes(label = sprintf("%0.2f", round(distance, digits = 2))), 
            size = 3.5, vjust = -0.5,
            color="black") +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
        panel.grid.major.y = element_line(color="gray", size=0.4),
        axis.text=element_text(size=11), #Size of axis numbering
        axis.text.x = element_text(angle = 45, hjust=1.0), #Tilting of axis numbering
        axis.title=element_text(size=12), #Size of axis title
        title = element_text(size=14, face="bold"), #Size of title
        legend.position="bottom", #Position of legend
  )
#Save to file
ggsave(plot_1_4, filename = "plots/01_Plots/1_plot_distance_income.svg")
ggsave(plot_1_4, filename = "plots/01_Plots/1_plot_distance_income.pdf")
ggsave(plot_1_4, filename = "plots/01_Plots/1_plot_distance_income.png")
plot_1_4
################################################################################
#Task 1.5 - Mode share by public transport-accessibility of household location
################################################################################
df_mode_location <- df_hh %>% 
  full_join(df_wegeInLand) %>% 
  group_by(wmittel2a, W_OEV_KLASSE, HHNR) %>% 
  filter(wmittel2a >= 1) %>%
  filter(wmittel2a <= 4) %>%
  mutate(wmittel2a = case_when(wmittel2a == 1 ~ "Active mode", 
                               wmittel2a == 2 ~ "Motorized mode", 
                               wmittel2a == 3 ~ "Public transit", 
                               wmittel2a == 4 ~ "Other")) %>% 
  mutate(W_OEV_KLASSE = case_when(W_OEV_KLASSE == 1	~ "Class A: Very good accessibility",
                                  W_OEV_KLASSE == 2	~ "Class B: Good accessibility",
                                  W_OEV_KLASSE == 3	~ "Class C: Medium development",
                                  W_OEV_KLASSE == 4	~ "Class D: Low development",
                                  W_OEV_KLASSE == 5	~ "No quality class: Marginal or no public transport accessibility"))

#Part a: computation with average distance of each trip
#Compute the average distance per wmittel2a and W_OEV_KLASSE. Be careful with the weighting!
#If a trip is not in the wmittel2a it should be counted as 0 in the average for this mode
df_totals_by_class <- df_mode_location %>% 
  dplyr::mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  dplyr::group_by(W_OEV_KLASSE, wmittel2a, HHNR) %>% 
  dplyr::summarise(distance = sum(w_rdist), .groups = "keep", W_OEV_KLASSE=W_OEV_KLASSE, HHNR=HHNR, WP=WP) %>% 
  dplyr::group_by(W_OEV_KLASSE, wmittel2a) %>%
  dplyr::summarise(count = sum(WP), distance = sum(distance, WP), .groups = "keep") %>% 
  dplyr::group_by(W_OEV_KLASSE) %>%
  dplyr::summarise(count = sum(count), distance = sum(distance)/count, .groups = "keep") %>% 
  dplyr::mutate(W_OEV_KLASSE = factor(W_OEV_KLASSE, levels = c("Class A: Very good accessibility", "Class B: Good accessibility", "Class C: Medium development", "Class D: Low development", "No quality class: Marginal or no public transport accessibility"))) %>% 
  dplyr::filter(!is.na(W_OEV_KLASSE))
df_totals_by_class$label_x <- str_wrap(df_totals_by_class$W_OEV_KLASSE, width=20)
df_totals_by_class


#Summarise the data by mode and class
df_mode_location_individual <- df_mode_location %>% 
  dplyr::mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  dplyr::group_by(W_OEV_KLASSE, wmittel2a, HHNR) %>% 
  dplyr::summarise(distance = sum(w_rdist), .groups = "keep", W_OEV_KLASSE=W_OEV_KLASSE, HHNR=HHNR, WP=WP) %>% 
  dplyr::group_by(W_OEV_KLASSE, wmittel2a) %>%
  dplyr::summarise(avg_distance = sum(distance, WP), .groups = "keep") %>%
  dplyr::full_join(df_totals_by_class, by=c("W_OEV_KLASSE")) %>% 
  dplyr::mutate(avg_distance = avg_distance/count) %>% 
  dplyr::mutate(W_OEV_KLASSE = factor(W_OEV_KLASSE, levels = c("Class A: Very good accessibility", "Class B: Good accessibility", "Class C: Medium development", "Class D: Low development", "No quality class: Marginal or no public transport accessibility"))) %>%
  dplyr::filter(!is.na(W_OEV_KLASSE))
df_mode_location_individual$label_x <- str_wrap(df_mode_location_individual$W_OEV_KLASSE, width=20)
#Calculate the percentages
df_mode_location_individual <- ddply(df_mode_location_individual, .(W_OEV_KLASSE), transform, percent = avg_distance/sum(avg_distance) * 100)
df_mode_location_individual <- ddply(df_mode_location_individual, .(W_OEV_KLASSE), transform, pos = sum(avg_distance) - cumsum(avg_distance) + 0.5 * avg_distance)
df_mode_location_individual$label_percentage <- paste0(sprintf("%.0f", df_mode_location_individual$percent), "%")

#Line break for the labels on the x-axis
df_mode_location_individual$labels_x <- str_wrap(df_mode_location_individual$W_OEV_KLASSE, width=20) 

#Plot for part a
plot_1_5_a <- df_mode_location_individual %>% ggplot(aes(x=labels_x, y=avg_distance, fill=wmittel2a)) +
  geom_bar(stat = "identity") +
  #Total of each bar label
  geom_text(aes(label_x, distance, label = sprintf("%0.2f", round(distance, digits = 2)), fill = NULL), 
            data = df_totals_by_class, 
            vjust=-0.5, #Adjust it to slightly higher
            size = 3.5) +
  #Percentage labels
  geom_text(aes(y = pos, label = label_percentage), 
            size = 3.5, 
            color="white") +
  #Color of the different sections
  scale_fill_manual(values=colors_4, 
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
        legend.position = "bottom", #Position of legend
        title = element_text(size=15, face="bold")) + #Size of title
  
  labs(x="Public transport accessibility", y="Average distance per person", title="Mode share vs. public transport accesibility")
#Save to file
ggsave(plot_1_5_a, filename = "plots/01_Plots/1_plot_mode_share_vs_accessibility.png")
ggsave(plot_1_5_a, filename = "plots/01_Plots/1_plot_mode_share_vs_accessibility.pdf")
ggsave(plot_1_5_a, filename = "plots/01_Plots/1_plot_mode_share_vs_accessibility.svg")

plot_1_5_a
#Part b: computation with pkm

#Compute the average distance per wmittel2a and W_OEV_KLASSE. Be careful with the weighting!
#If a trip is not in the wmittel2a it should be counted as 0 in the average for this mode
df_totals_by_class <- df_mode_location %>% 
  dplyr::mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  dplyr::group_by(W_OEV_KLASSE) %>% 
  dplyr::summarise(distance = sum(w_rdist, WP), .groups="rowwise") %>% 
  dplyr::filter(is.na(W_OEV_KLASSE) == FALSE)
df_totals_by_class$label_x <- str_wrap(df_totals_by_class$W_OEV_KLASSE, width=20)
df_totals_by_class

#Summarise the data by mode and class
df_mode_location_individual <- df_mode_location %>% 
  dplyr::mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  dplyr::group_by(W_OEV_KLASSE, wmittel2a) %>%
  dplyr::summarise(distance = sum(w_rdist, WP), .groups = "keep") %>%
  dplyr::mutate(W_OEV_KLASSE = factor(W_OEV_KLASSE, levels = c("Class A: Very good accessibility", "Class B: Good accessibility", "Class C: Medium development", "Class D: Low development", "No quality class: Marginal or no public transport accessibility"))) %>%
  dplyr::filter(!is.na(W_OEV_KLASSE))
df_mode_location_individual$label_x <- str_wrap(df_mode_location_individual$W_OEV_KLASSE, width=20)
#Calculate the percentages
df_mode_location_individual <- ddply(df_mode_location_individual, .(W_OEV_KLASSE), transform, percent = distance/sum(distance) * 100)
df_mode_location_individual <- ddply(df_mode_location_individual, .(W_OEV_KLASSE), transform, pos = sum(distance) - cumsum(distance) + 0.5 * distance)
df_mode_location_individual$label_percentage <- paste0(sprintf("%.0f", df_mode_location_individual$percent), "%")

#Line break for the labels on the x-axis
df_mode_location_individual$labels_x <- str_wrap(df_mode_location_individual$W_OEV_KLASSE, width=20) 

#Plot for part b
plot_1_5_b <- df_mode_location_individual %>% ggplot(aes(x=labels_x, y=distance, fill=wmittel2a)) +
  geom_bar(stat = "identity") +
  #Total of each bar label
  geom_text(aes(label_x, distance, label = sprintf("%.f", round(distance, digits = 0)), fill = NULL), 
            data = df_totals_by_class, 
            vjust=-0.5, #Adjust it to slightly higher
            size = 3.5) +
  #Percentage labels
  geom_text(aes(y = pos, label = label_percentage), 
            size = 3.5, 
            color="white") +
  #Color of the different sections
  scale_fill_manual(values=colors_4, 
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
        legend.position = "bottom",
        title = element_text(size=15, face="bold")) + #Size of title
  
  labs(x="Public transport accessibility", y="Person km", title="Mode share vs. public transport accesibility")
#Save to file
ggsave(plot_1_5_b, filename = "plots/01_Plots/1_plot_mode_share_vs_accessibility_pkm.png")
ggsave(plot_1_5_b, filename = "plots/01_Plots/1_plot_mode_share_vs_accessibility_pkm.pdf")
ggsave(plot_1_5_b, filename = "plots/01_Plots/1_plot_mode_share_vs_accessibility_pkm.svg")
plot_1_5_b

################################################################################
#Task 1.6 - Mode share by trip purpose
################################################################################
df_mode_purpose <- df_hh %>% 
  full_join(df_wegeInLand) %>% 
  dplyr::mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
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
  dplyr::summarise(distance = sum(w_rdist, WP), .groups = "keep") 

#Use to show the totals over one column
df_totals_by_class <- df_mode_purpose %>% 
  mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  group_by(wzweck3) %>% 
  filter(wmittel2a > 0) %>%
  filter(!is.na(wzweck3)) %>% 
  dplyr::summarise(distance = sum(w_rdist, WP)) %>%
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
            size = 3.5) +
  #Percentage labels
  geom_text(aes(y = pos, label = label_percentage), 
            size = 3.5, 
            color="white") +
  #Color of the different sections
  scale_fill_manual(values=colors_4, 
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
        legend.position = "bottom",
        title = element_text(size=15, face="bold")) + #Size of title
  labs(x="Trip purpose", y="Person km", title="Mode share vs. trip purpose")
#Save to file
ggsave(plot_1_6, filename = "plots/01_Plots/1_plot_mode_share_vs_trip_purpose.svg")
ggsave(plot_1_6, filename = "plots/01_Plots/1_plot_mode_share_vs_trip_purpose.png")
ggsave(plot_1_6, filename = "plots/01_Plots/1_plot_mode_share_vs_trip_purpose.pdf")
plot_1_6

#Average distance per weekday
df_weekday <- df_personen %>% 
  full_join(df_wegeInLand) %>% 
  dplyr::mutate(w_rdist = coalesce(w_rdist, 0)) %>% 
  group_by(Tag, HHNR) %>%
  dplyr::summarise(w_rdist = sum(w_rdist), WP=WP, Tag=Tag) %>%
  group_by(Tag) %>%
  dplyr::summarise(distance = weighted.mean(w_rdist, WP)) %>% 
  mutate(distance = round(distance, 2)) %>% 
  dplyr::filter(Tag > 0) %>%
  dplyr::filter(!is.na(Tag)) %>%
  dplyr::filter(Tag < 8) %>%
  mutate(Tag = case_when(Tag == 1 ~ "Monday", 
                          Tag == 2 ~ "Tuesday", 
                          Tag == 3 ~ "Wednesday", 
                          Tag == 4 ~ "Thursday", 
                          Tag == 5 ~ "Friday", 
                          Tag == 6 ~ "Saturday", 
                          Tag == 7 ~ "Sunday")) %>%
  mutate(Tag = factor(Tag, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

#Plot
plot_1_7 <- df_weekday %>% ggplot(aes(x=Tag, y=distance)) +
  geom_col(fill=color_fill) +
  geom_text(aes(label = distance), vjust=-0.5, size = 3.5) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
        panel.grid.major.y = element_line(color="gray", size=0.4),
        axis.text=element_text(size=9), #Size of axis numbering
        axis.text.x = element_text(angle = 45, hjust=1), #Tilting of axis numbering
        axis.title=element_text(size=13,face="bold"), #Size of axis title
        legend.text=element_text(size=10),
        legend.title=element_text(size=13, face="bold"),
        legend.background = element_rect(fill="white", color="black", size=0.4, linetype ="solid"),
        legend.position = "bottom",
        title = element_text(size=15, face="bold")) + #Size of title
  labs(x="Day of the week", y="km per person", title="Average distance per weekday")

#Save to file
ggsave(plot_1_7, filename = "plots/01_Plots/1_plot_average_distance_per_weekday.svg")
ggsave(plot_1_7, filename = "plots/01_Plots/1_plot_average_distance_per_weekday.png")
ggsave(plot_1_7, filename = "plots/01_Plots/1_plot_average_distance_per_weekday.pdf")
plot_1_7

