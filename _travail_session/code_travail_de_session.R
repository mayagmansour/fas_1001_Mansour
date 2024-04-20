# --- LIBRAIRIES ---
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(stringr)
library(lmtest)
library(sandwich)
library(MASS)
library(forestplot)
library(DataExplorer)

# ---IMPORTATION DES BASES DE DONNÉES ---

nc_f <- readRDS("C:/Users/mayag/Desktop/fas_1001/fas_1001_Mansour/_travail_session/donnees/nc_dataframe_2013_2015")
nc_subset_14 <- readRDS("C:/Users/mayag/Desktop/fas_1001/fas_1001_Mansour/_travail_session/donnees/nc_subset_14")
nc <- readRDS("C:/Users/mayag/Desktop/fas_1001/fas_1001_Mansour/_travail_session/donnees/yg821jf8611_nc_statewide_2020_04_01.rds")


## Nettoyage et création des bases de données finales ##

nc_f <- nc %>%
  select(-county_name, -type, -contraband_found, -contraband_drugs, -contraband_weapons, -frisk_performed, -search_basis, -search_conducted, -search_person, -search_vehicle, -reason_for_frisk, -reason_for_search, -raw_Ethnicity, -raw_Race, -raw_action_description) %>% 
  filter(!is.na(subject_race) & !is.na(subject_sex) & !is.na(date) & !is.na(time)) %>%
  mutate(arrest_made = ifelse(arrest_made, 1, 0),
         citation_issued = ifelse(citation_issued, 1, 0),
         warning_issued = ifelse(warning_issued, 1, 0)) %>%
  mutate(reason_for_stop = str_replace_all(reason_for_stop, "Vehicle Regulatory Violation", "Vehicle Violation"),
         reason_for_stop = str_replace_all(reason_for_stop, "Vehicle Equipment Violation", "Vehicle Violation")) %>% 
  filter(str_detect(date, "^2013|^2014|^2015")) 
  mutate(department_name_i = as.integer(factor(department_name, levels = unique(department_name))))

nc_subset_14 <- nc_f %>% 
  filter(str_detect(date, "^2014|^2015")) %>% 
  filter(!is.na(time)) %>% 
  filter(subject_race != "unknown") %>% 
  factor(subject_race, levels = c("white", "black", "hispanic", "asian/pacific islander", "other")) %>%
  mutate(timestamp = as.POSIXct(time)) %>% 
  filter(hour(timestamp) >= 7 & hour(timestamp) < 19)

# Sauvegarder la base de données finale
setwd("C:/Users/mayag/Desktop/fas_1001/fas_1001_Mansour/_travail_session/donnees")
saveRDS(nc_subset_14, file = "nc_subset_14")

## Aperçus des données 
nc_f %>% count(subject_race)
nc_f %>% count(subject_sex)
nc_f %>% count(reason_for_stop)
nc_f %>% count(year(date))

nc_subset_14 %>% count(subject_race)
nc_subset_14 %>% count(subject_sex)
nc_subset_14 %>% count(year(date))
nc_subset_14 %>% count(reason_for_stop)
nc_subset_14 %>% count(department_name)

# Données sur la population 

pop <- read.csv("C:/Users/mayag/Desktop/fas_1001/fas_1001_Mansour/_travail_session/donnees/Race and Ethnicity.csv")

Population <- pop %>%
  select(-c(ID.Year, Geography, ID.Geography, Slug.Geography, share, Hispanic.Population.Moe, ID.Race)) %>% 
  filter(str_detect(Year, "^2014|^2015")) %>% 
  mutate(Race = str_replace_all(Race, "White Alone", "white"), 
         Race = str_replace_all(Race, "American Indian & Alaska Native Alone", "other"),
         Race = str_replace_all(Race, "Some Other Race Alone", "other"), 
         Race = str_replace_all(Race, "Native Hawaiian & Other Pacific Islander Alone", "asian/pacific islander"), 
         Race = str_replace_all(Race, "Asian Alone", "asian/pacific islander"), 
         Race = str_replace_all(Race, "Black or African American Alone", "black"), 
         Race = str_replace_all(Race, "Two or More Races", "other")) %>%
  group_by(Ethnicity) %>% 
  group_by(Year) %>% 
  mutate(hisp = sum(Population))

    # Création de la base de données finale de la population 
population <- tibble(
  subject_race = c("asian/pacific islander", "white", "black", "other", "hispanic", "asian/pacific islander", "white", "black", "other", "hispanic"),
  year = c(2014, 2014, 2014, 2014, 2014, 2015, 2015, 2015, 2015, 2015),
  pop = c(237913, 6294528, 2093389, 309278, 815297, 250320, 6324373, 2115338, 319399, 835903)) %>% 
  mutate(combined_id = paste(year, subject_race, sep = "-"))


# --- STATISTIQUES DESCRIPTIVES --- 


nc_subset_14$year <- format(nc_subset_14$date, "%Y")

# Proportion d'arrêts par genre et par race
prop_sex_race_nc <- prop.table(table(nc_subset_14$subject_sex, nc_subset_14$subject_race, nc_subset_14$year))
print(prop_sex_race_nc)

ggplot(data = as.data.frame.table(prop_sex_race_nc), aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Graphique 2: Proportion d'arrêt par genre et par origine", x = "2014", y = "Proportion")+
  guides(fill = guide_legend(title = "Genre"))+
  theme(plot.title = element_text(hjust = 0.5))

# Arrêts par race (2014-2015)
prop_race_nc <- prop.table(table(as.character(nc_subset_14$subject_race), nc_subset_14$year))
print(prop_race_nc)

# Répartition de l'âge 
ggplot(data = nc_subset_14, aes(x = subject_age))+
  geom_bar(fill = "skyblue")+
  labs(title = "Graphique 1: Nombre d'arrêts par âge (2014-2015)", x = "Âge", y = "Nombre d'observations")+
  theme(plot.title = element_text(hjust = 0.5))

# Arrêts par race et par an (2000-2015)
nc %>% count(year = year(date), subject_race) %>% 
  ggplot(aes(x = year, y = n, color = subject_race))+
  geom_point()+
  geom_line()+
  labs(title = "Nombre d'arrêts selon l'origine ethnique", x = "Année", y = "Arrêts")+
  guides(color = guide_legend(title = "Race"))+
  theme(plot.title = element_text(hjust = 0.5))

# Taux d'arrêts par race par an (pondéré)
nc_subset_14 %>% count(year = year(date), subject_race) %>% 
  mutate(combined_id = paste(year, subject_race, sep = "-")) %>% 
  left_join(population, by = "combined_id") %>% 
  mutate(stop_rate = n / pop) %>% 
  select(-c(subject_race.y, year.y, combined_id)) %>%
  ggplot(aes(x = as.factor(year.x), y = stop_rate, fill = subject_race.x))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Graphique 3: Taux d'arrêt par race", x = "Année", y = "Taux d'arrêt")+ 
  guides(fill = guide_legend(title = "Race"))+
  theme(plot.title = element_text(hjust = 0.5))


# --- OPÉRATIONNALISATION DES DONNÉES --- 

# Fixer les catégories de référence 
nc_subset_14$subject_race <- relevel(nc_subset_14$subject_race, ref = "white")
nc_subset_14$subject_sex <- relevel(nc_subset_14$subject_sex, ref = "male")

# Effet sur arrest_made

mod2 <- glm(arrest_made ~ subject_race + subject_sex + subject_age + date + time, data = nc_subset_14, family = binomial)
summary(mod2)

coef_mod2 <- coef(mod2)
var_names <- names(coef_mod2)
ci_mod2 <- confint(mod2)

forest_data_mod2 <- data.frame(
  Variable = var_names,
  Coefficient = coef_mod2, 
  Lower_CI = ci_mod2[, 1],
  Upper_CI = ci_mod2[, 2]
)

forest_data_mod2_filtered <- subset(forest_data_mod2, Variable != "(Intercept)" & Variable != "time" & Variable != "date")

ggplot(data = forest_data_mod2_filtered, aes(x = Variable, y = Coefficient)) +
  geom_pointrange(aes(ymin = Lower_CI, ymax = Upper_CI), color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add zero line
  labs(x = "Variables sociodémographiques", y = "Logarithme du \nrapport des cotes") +
  ggtitle("Probabilité de subir une arrestation \nselon l'âge, le genre et la race")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels = c("subject_age" = "age",
                              "subject_raceasian/pacific islander" = "asian / pacific islander",
                              "subject_raceblack" = "black",
                              "subject_racehispanic" = "hispanic", 
                              "subject_raceother" = "other", 
                              "subject_sexfemale" = "female"))


# Effet sur citation_issued 

mod3 <- glm(citation_issued ~ subject_race + subject_sex + subject_age + date + time, data = nc_subset_14, family = binomial)
summary(mod3)

coef_mod3 <- coef(mod3)
var_names <- names(coef_mod3)
ci_mod3 <- confint(mod3)

forest_data_mod3 <- data.frame(
  Variable = var_names,
  Coefficient = coef_mod3, 
  Lower_CI = ci_mod3[, 1],
  Upper_CI = ci_mod3[, 2]
)

forest_data_mod3_filtered <- subset(forest_data_mod3, Variable != "(Intercept)" & Variable != "time" & Variable != "date")

ggplot(data = forest_data_mod3_filtered, aes(x = Variable, y = Coefficient)) +
  geom_pointrange(aes(ymin = Lower_CI, ymax = Upper_CI), color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add zero line
  labs(x = "Variables sociodémographiques", y = "Logarithme du \nrapport des cotes") +
  ggtitle("Probabilité de recevoir une citation \nselon l'âge, le genre et la race")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels = c("subject_age" = "age",
                              "subject_raceasian/pacific islander" = "asian / pacific islander",
                              "subject_raceblack" = "black",
                              "subject_racehispanic" = "hispanic", 
                              "subject_raceother" = "other", 
                              "subject_sexfemale" = "female"))

# Effet sur warning_issued 

mod4 <- glm(warning_issued ~ subject_race + subject_sex + subject_age + date + time, data = nc_subset_14, family = binomial)
summary(mod4)

coef_mod4 <- coef(mod4)
var_names <- names(coef_mod4)
ci_mod4 <- confint(mod4)

forest_data_mod4 <- data.frame(
  Variable = var_names,
  Coefficient = coef_mod4, 
  Lower_CI = ci_mod4[, 1],
  Upper_CI = ci_mod4[, 2]
)

forest_data_mod4_filtered <- subset(forest_data_mod4, Variable != "(Intercept)" & Variable != "time" & Variable != "date")

ggplot(data = forest_data_mod4_filtered, aes(x = Variable, y = Coefficient)) +
  geom_pointrange(aes(ymin = Lower_CI, ymax = Upper_CI), color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add zero line
  labs(x = "Variables sociodémographiques", y = "Logarithme du \nrapport des cotes") +
  ggtitle("Probabilité de recevoir un avertissement \nselon l'âge, le genre et la race")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels = c("subject_age" = "age",
                              "subject_raceasian/pacific islander" = "asian / pacific islander",
                              "subject_raceblack" = "black",
                              "subject_racehispanic" = "hispanic", 
                              "subject_raceother" = "other", 
                              "subject_sexfemale" = "female"))


