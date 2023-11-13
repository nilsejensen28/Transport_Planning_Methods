library(data.table)
library(magrittr) #Allows to use %>% 
library(dplyr)
library(tidyverse) #Make nice plots
library(plyr) #Needed for ddply
library(wesanderson) #Nice color palette
library(MASS)
library(glmnet)
library(ggplot2)
library(tidyr)
library(gridExtra) #Make grid plots
library(svglite) #Allows for SVG Files
options(scipen = 999) #No scientific notation

################################################################################
#Read in the data
################################################################################
df_personen <- read.csv("zielpersonen.csv", header = TRUE, sep = ";")
df_hh <- read.csv("haushalte.csv", header=TRUE, sep=";")
df_wegeInland_hh <- read.csv("wegeinland.csv", header=TRUE, sep=";") %>%
  full_join(df_hh, by= c("HHNR"))
df_wegeInLand <- read.csv("wegeinland.csv", header=TRUE, sep=";")

df_personen_trips <- df_personen %>% 
  full_join(df_wegeInLand, by=c("HHNR")) %>% 
  group_by(HHNR) %>% 
  dplyr::summarise(n_trips=n())

#Add all household without trips
households_with_trips <- unique(df_wegeInland$HHNR)
df_households_without_trips <- df_hh %>% 
  filter(!(HHNR %in% households_with_trips)) %>% 
  dplyr::select(c("HHNR"))
df_households_without_trips$n_trips <- 0

df_personen_trips <- rbind(df_personen_trips, df_households_without_trips)
  
#Adds the number of daily trips to the data
df_personen <- join(df_personen, df_personen_trips, by=c("HHNR"))

df_all <- full_join(df_personen, df_hh, by=c("HHNR")) %>% lapply(., iconv, to = "UTF-8") %>% tibble::as_tibble()
###############################################################################
#Make the data more readable
###############################################################################

df <- full_join(df_personen, df_hh, by=c("HHNR")) %>% 
  dplyr::rename("person.id" = "HHNR",
                "person.age" = "alter",
                "person.sex" = "gesl",
                "person.travel_day" = "Tag",
                "person.employement" = "ERWERB",
                "person.drivers_licence" = "f20400a",
                "person.work_pensum" = "f40920",
                "person.GA" = "f41600_01a",
                "person.halbtax" = "f41600_01b",
                "person.wheelchair" = "f42000",
                "person.homeoffice" = "f81300",
                "person.stellenprozente_home" = "f81400",
                "person.weight" = "f81502",
                "household.oev_availibility" = "W_OEV_KLASSE",
                "household.degree_urbanization" = "W_DEGURBA",
                "household.number_of_cars" = "f30100",
                "household.number_of_bikes" = "f32200a",
                "household.size" = "hhgr",
                "household.income" = "f20601") %>% 
  filter(person.age > 0) %>% 
  filter(household.size > 0) %>% 
  filter(household.number_of_cars >= 0) 

###############################################################################
#Create dummy_variables
###############################################################################
df$person.is_man <- as.factor(ifelse(df$person.sex == 1, "Man", "Not man"))
df$person.has_GA <- as.factor(ifelse(df$person.GA == 1, "GA", "No GA"))
df$person.has_halbtax <- as.factor(ifelse(df$person.halbtax == 1, "Halbtax", "No halbtax"))
df$household.has_car <- as.factor(ifelse(df$household.number_of_cars >= 1, "Has car", "No car"))
df$household.has_bike <- as.factor(ifelse(df$household.number_of_bikes >= 1, "Has bike", "No bike"))
df$person.has_driverlicence <- as.factor(ifelse(df$person.drivers_licence == 1, "Has driving licence", "No driving licence"))
df$person.always_works_from_home <- as.factor(ifelse(df$person.homeoffice == 1, "Always homeoffice", "Other"))
df$person.happens_works_from_home <- as.factor(ifelse(df$person.homeoffice == 2, "Sometimes homeoffice", "Other"))
df$person.uses_wheelchair <- as.factor(ifelse(df$person.wheelchair == 1, "Wheelchair user", "Other"))
df$household.in_city <- as.factor(ifelse(df$household.degree_urbanization == 1, "City", "Other"))
df$household.in_countryside <- as.factor(ifelse(df$household.degree_urbanization == 3, "Countryside", "Other"))
df$person.is_unemployed <- as.factor(ifelse(df$person.employement == 4, "Unemployed", "Employed"))

#Add quadratic variables
df$person.age_squared = df$person.age**2
df$household.income_squared = df$household.income**2
df$person.age_log = log(df$person.age)
df$household.size_log = log(df$household.size)
df$household.size_squared = df$household.size**2

# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

#Remove outliers
#df <- df %>% 
  #filter(n_trips < 10)

###############################################################################
#Visualize the different dummy variables
###############################################################################
plot_styled <- function(plot){
  plot <- plot +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", fill=wes_palette("GrandBudapest1", n=2)[2]) + 
    labs(x="", y="Number of trips") +
    theme(panel.grid = element_blank(), 
          panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
          panel.grid.major.y = element_line(color="gray", size=0.4),
          axis.text=element_text(size=9), #Size of axis numbering
          axis.text.x = element_text(angle = 0, hjust=0.5), #Tilting of axis numbering
          axis.title=element_text(size=9)) #Size of axis title
  return(plot)
}

plot.is_man <- plot_styled(ggplot(df, aes(x=person.is_man, y=n_trips)))
plot.has_GA <- plot_styled(ggplot(df, aes(x=person.has_GA, y=n_trips)))
plot.has_halbtax <- plot_styled(ggplot(df, aes(x=person.has_halbtax, y=n_trips)))
plot.has_car <- plot_styled(ggplot(df, aes(x=household.has_car, y=n_trips)))
plot.has_bike <- plot_styled(ggplot(df, aes(x=household.has_bike, y=n_trips)))
plot.has_driverlicence <- plot_styled(ggplot(df, aes(x=person.has_driverlicence, y=n_trips)))
plot.always_works_from_home <- plot_styled(ggplot(df, aes(x=person.always_works_from_home, y=n_trips)))
plot.happens_wors_from_home <- plot_styled(ggplot(df, aes(x=person.happens_works_from_home, y=n_trips)))
plot.uses_wheelchair <- plot_styled(ggplot(df, aes(x=person.uses_wheelchair, y=n_trips)))
plot.in_city <- plot_styled(ggplot(df, aes(x=household.in_city, y=n_trips)))
plot.in_countryside <- plot_styled(ggplot(df, aes(x=household.in_countryside, y=n_trips)))
plot.is_unemployed <- plot_styled(ggplot(df, aes(x=person.is_unemployed, y=n_trips)))

grid.plot <- grid.arrange(arrangeGrob(plot.is_man,
                           plot.has_GA,
                           plot.has_halbtax,
                           plot.has_car,
                           plot.has_bike,
                           plot.has_driverlicence,
                           plot.always_works_from_home,
                           plot.happens_wors_from_home,
                           plot.uses_wheelchair,
                           plot.in_city,
                           plot.in_countryside,
                           plot.is_unemployed,
                           nrow=4), nrow=1, top = textGrob("Effect of different factor variables on number of trips", gp=gpar(fontsize=15,face="bold")))
ggsave(grid.plot, filename = "plots/2_plot_dummy_variable.svg")
ggsave(grid.plot, filename = "plots/2_plot_dummy_variable.png")

#Create factor variable
df$household.degree_urbanization <- as.factor(df$household.degree_urbanization)

###############################################################################
#Divide into training and test data
###############################################################################
df_train <- df[0:45000,]
df_test <- df[45001:53781,]

###############################################################################
#Train a negative binomial linear model
###############################################################################
lin_model <- glm.nb(n_trips ~  person.has_driverlicence
                 + household.oev_availibility
                 + person.has_GA
                 + household.has_bike
                 + person.is_unemployed
                 + person.uses_wheelchair
                 + person.happens_works_from_home
                 + person.always_works_from_home
                 + person.age
                 + person.age_squared
                 + person.age_log
                 + household.number_of_cars
                 , data=df_train)

anova(lin_model)
summary(lin_model)
par(mfrow = c(2, 2))
plot(lin_model) #Plots the different diagnostic plots

###############################################################################
#Test the model
###############################################################################
df_test$prediction <- exp(predict(lin_model, df_test)) #Predict on the testset

colors <- wes_palette("GrandBudapest1", n=4, type = "discrete")
legend_colors <- c("n_trips" = colors[1],  "prediction" = colors[3])

plot.prediction_on_age <- ggplot(data = df_test) + 
  geom_smooth(aes(x = person.age, y = prediction, color = "prediction"), method="loess") + 
  geom_smooth(aes(x = person.age, y = n_trips, color = "n_trips"), method = "loess") +
  labs(color = "Colors") + 
  scale_color_manual(values = legend_colors, labels=c("true value", "prediction")) + 
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
  labs(x="Age", y="Number of trips", title="Average prediction on test set vs. average value")
plot.prediction_on_age
ggsave(plot.prediction_on_age, filename = "plots/2_plot_age_prediction.svg")
ggsave(plot.prediction_on_age, filename = "plots/2_plot_age_prediction.png")

df_test$error <- df_test$n_trips - df_test$prediction
plot.error_histogram <- ggplot(data=df_test, aes(x=error)) +
  geom_histogram(binwidth = 0.5, fill=colors[1]) +
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
  labs(x="test error", y="count", title="Distribution of the test error")
plot.error_histogram
ggsave(plot.prediction_on_age, filename = "plots/2_plot_test_error.svg")
ggsave(plot.prediction_on_age, filename = "plots/2_plot_test_error.png")

df_plot_1 <- df_test%>% dplyr::select(-c("n_trips"))
df_plot_1["type"] <- "prediction"
df_plot_1 <- df_plot_1 %>%  dplyr::rename("n_trips" = "prediction")
df_plot_2 <- df_test%>% dplyr::select(-c("prediction"))
df_plot_2["type"] <- "test_data"
df_plot <- rbind(df_plot_1, df_plot_2)
df_plot$type <- as.factor(df_plot$type)

plot_styled <- function(plot){
  plot <- plot +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", aes(fill=type), position = position_dodge(width = 1), show.legend = FALSE) + 
    scale_fill_manual(values=wes_palette("GrandBudapest1", n=2, type = "discrete"), name="") + 
    labs(x="", y="Number of trips") +
    theme(panel.grid = element_blank(), 
          panel.background = element_rect(fill="white", colour="black", size=0.5, linetype="solid"), #No background and frame
          panel.grid.major.y = element_line(color="gray", size=0.4),
          axis.text=element_text(size=9), #Size of axis numbering
          axis.text.x = element_text(angle = 0, hjust=0.5), #Tilting of axis numbering
          axis.title=element_text(size=9)) #Size of axis title
  return(plot)
}

plot.is_man <- ggplot(df_plot, aes(x=person.is_man, y=n_trips)) + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", aes(fill=type), position = position_dodge(width = 1)) + scale_fill_manual(values=wes_palette("GrandBudapest1", n=2, type = "discrete"), name="") + labs(x="", y="Number of trips") + theme(legend.position="bottom", legend.text=element_text(size=10), legend.title=element_text(size=11, face="bold"), legend.background = element_rect(fill="white", color="black", size=0.4, linetype ="solid"))
plot.has_GA <- plot_styled(ggplot(df_plot, aes(x=person.has_GA, y=n_trips)))
plot.has_halbtax <- plot_styled(ggplot(df_plot, aes(x=person.has_halbtax, y=n_trips)))
plot.has_car <- plot_styled(ggplot(df_plot, aes(x=household.has_car, y=n_trips)))
plot.has_bike <- plot_styled(ggplot(df_plot, aes(x=household.has_bike, y=n_trips)))
plot.has_driverlicence <- plot_styled(ggplot(df_plot, aes(x=person.has_driverlicence, y=n_trips)))
plot.always_works_from_home <- plot_styled(ggplot(df_plot, aes(x=person.always_works_from_home, y=n_trips)))
plot.happens_works_from_home <- plot_styled(ggplot(df_plot, aes(x=person.happens_works_from_home, y=n_trips)))
plot.uses_wheelchair <- plot_styled(ggplot(df_plot, aes(x=person.uses_wheelchair, y=n_trips)))
plot.in_city <- plot_styled(ggplot(df_plot, aes(x=household.in_city, y=n_trips)))
plot.in_countryside <- plot_styled(ggplot(df_plot, aes(x=household.in_countryside, y=n_trips)))
plot.is_unemployed <- plot_styled(ggplot(df_plot, aes(x=person.is_unemployed, y=n_trips)))

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
legend <-g_legend(plot.is_man)
plot.is_man <- plot_styled(ggplot(df_plot, aes(x=person.is_man, y=n_trips)))

plot.error_on_dummy_data <- grid.arrange(arrangeGrob(plot.is_man,
             plot.has_GA,
             plot.has_halbtax,
             plot.has_car,
             plot.has_bike,
             plot.has_driverlicence,
             plot.always_works_from_home,
             plot.happens_works_from_home,
             plot.uses_wheelchair,
             plot.in_city,
             plot.in_countryside,
             plot.is_unemployed,
             nrow=4), legend, nrow=2, heights=c(13, 1), top = textGrob("Prediction vs. real values on test set",gp=gpar(fontsize=15,face="bold"))) 
ggsave(plot.prediction_on_age, filename = "plots/2_plot_error_on_dummy_variables.svg")
ggsave(plot.prediction_on_age, filename = "plots/2_plot_error_on_dummy_variables.png")
