# .1- Installation des packages ------------------------------------------------

install.packages("tidyverse")
install.packages("ggpubr")
install.packages("smplot2")
install.packages("devtools")
install_github("kassambara/ggpubr")
library(ggpubr)
library(lubridate)
library(dplyr)
library(ggplot2)



# .2- Importer et définir la base de données -----------------------------------

setwd("C:/Users/mayag/Desktop/FAS1001/fas_1001_Mansour/_tp/_tp2")
  #American election study 
anes <- read.csv("anes_timeseries_cdf_csv_20220916.csv")
  #US real government expenditure
exp <- read.csv("gov_current_expenditure.csv")



# .3- Nettoyage de variables ---------------------------------------------------

#   .3.1- ANES Time Series Data -----

anes_clean <- anes %>%
  select(VCF0004, VCF0231) %>% 
  rename(year = VCF0004,
         gov_sup = VCF0231) %>%
  filter(year >= 1950) %>% 
  na.omit() %>% 
  group_by(year) %>%
  mutate(gov_sup = mean(gov_sup, na.rm = TRUE)) %>% 
  distinct()

#   .3.2- US Expenditure Data -----

exp_clean <- exp %>% 
  select(DATE, G160751A027NBEA) %>% 
  mutate(Date = ymd(DATE),
    year = format_ISO8601(Date, precision = "y")) %>% 
  group_by(year) %>% 
  mutate(avg_exp = mean(G160751A027NBEA, na.rm = TRUE)) %>% 
  select(-c(DATE, G160751A027NBEA, Date)) %>% 
  distinct() %>% 
  filter(year %in% c(1980, 1988, 1992, 1996, 2000, 2002, 2004, 2008, 2012))
exp_clean$year <- as.numeric(exp_clean$year)



# .4- Fusion des données -------------------------------------------------------

data_clean <- left_join(anes_clean, exp_clean, by = NULL) %>% 
  na.omit()



# .5- Analyse graphique --------------------------------------------------------

ggplot(data_clean, aes(x = avg_exp, y = gov_sup))+
  geom_point(color = "red")+
  geom_smooth(color = "skyblue")+ 
  geom_smooth(method="lm", color = "blue")+
  labs(x = "Average Expenditure (Billions of Chained 2017 Dollars)",
       y = "Average support for federal government",
       title = "How does government expenditure affect\nthe population's opinion of federal government?\n(1980 - 2012)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  stat_cor(method = "pearson", label.x = 50, label.y = 70)





  
  


  
  
  



