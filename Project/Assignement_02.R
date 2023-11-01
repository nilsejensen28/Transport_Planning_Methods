library(data.table)
library(magrittr) #Allows to use %>% 
library(dplyr)
library(tidyverse) #Make nice plots
library(gt) #Make nice tables
library(mmtable2) #Make nice tables
library(plyr) #Needed for ddply
library(wesanderson) #Nice color palette
library(svglite) #Allows for SVG
library(MASS)
library(glmnet)
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

#Adds the number of daily trips to the data
df_personen <- join(df_personen, df_personen_trips, by=c("HHNR"))

df_all <- full_join(df_personen, df_hh, by=c("HHNR")) %>% lapply(., iconv, to = "UTF-8") %>% tibble::as_tibble()

#Make the data more readable
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
                "household.car_ownership" = "f30100",
                "household.size" = "hhgr",
                "household.income" = "f20601"
                ) 

#Create dummy_variables
df$person.man <- ifelse(df$person.sex == 1, 1, 0)
df$person.woman <- ifelse(df$person.sex == 2, 1, 0)
df$person.has_GA <- ifelse(df$person.GA == 1, 1, 0)
df$person.has_halbtax <- ifelse(df$person.halbtax == 1, 1, 0)
df$household.has_car <- ifelse(df$household.car_ownership == 1, 1, 0)
df$person.has_driverlicence <- ifelse(df$person.drivers_licence == 1, 1, 0)
df$person.always_works_from_home <- ifelse(df$person.homeoffice == 1, 1, 0)
df$person.happens_works_from_home <- ifelse(df$person.homeoffice == 2, 1, 0)
df$person.uses_wheelchair <- ifelse(df$person.wheelchair == 1, 1, 0)
df$household.in_city <- ifelse(df$household.degree_urbanization == 1, 1, 0)
df$household.in_countryside <- ifelse(df$household.degree_urbanization == 3, 1, 0)
df$person.unemployed <- ifelse(df$person.employement == -99, 1, 0)

lin_model <- lm(n_trips ~ person.stellenprozente_home, df)
summary(lin_model)

pairs(n_trips ~ person.stellenprozente_home, df)

library(MASS)
library(tree)
library(rpart)
data = df[, c("person.age",
              "person.man",
              "person.has_GA",
              "person.has_halbtax",
              "person.always_works_from_home",
              "person.uses_wheelchair",
              "household.in_city",
              "household.in_countryside",
              "household.oev_availibility",
              "person.has_driverlicence",
              "household.income",
              "n_trips",
              "person.unemployed")]
control = rpart.control(minsplit = 10L, cp=0.0002)
tree = rpart(n_trips ~ ., data=data, control=control)
summary(tree)
plot(tree)
text(tree, pretty = 0)

library(rpart.plot)
prp(tree)

set.seed(18)
tree_cv = cv.tree(tree)

