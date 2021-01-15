library(tidyverse)
library(readxl)
library(stringi)
library(assertthat)

# read in data ------------------------------------------------------------------
# https://www.census.gov/data/datasets/time-series/demo/popest/2010s-national-detail.html?#


df <- read_csv("data/raw/ACSDP05Y2018.csv", col_names = FALSE) 
df_t <- transpose(df) 
df_raw <- data.frame("col_name" = unlist(df_t[1]), 
                     "var" = unlist(df_t[2]), 
                     "value" = unlist(df_t[3]))

df_sex <- df_raw %>%
  filter(var == "Estimate!!SEX AND AGE!!Total population!!Male" |
           var == "Estimate!!SEX AND AGE!!Total population!!Female") %>%
  select(var, value) %>%
  mutate(var = gsub("Estimate!!SEX AND AGE!!Total population!!", "", var)) %>%
  mutate(value = as.numeric(levels(value))[value], 
         percent = value/sum(value)) %>%
  rename("sex" = var)

df_race_ethnicity <- df_raw %>%
  filter(var == "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Hispanic or Latino (of any race)" |
           var == "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino!!White alone" |
           var == "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino!!Black or African American alone" |
           var == "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino!!American Indian and Alaska Native alone" |
           var == "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino!!Asian alone" |
           var == "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino!!Native Hawaiian and Other Pacific Islander alone" |
           var == "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino!!Some other race alone" |
           var == "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino!!Two or more races") %>%
  select(var, value) %>%
  mutate(var = gsub("Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!", "", var), 
         var = gsub("Not Hispanic or Latino!!", "", var), 
         var = gsub(" alone", "", var), 
         var = gsub(" \\(of any race\\)", "", var), 
         var = gsub("Some other race", "Other", var)) %>% 
  mutate(value = as.numeric(levels(value))[value], 
         percent = value/sum(value))%>%
  rename("race" = var)

write_csv(df_sex, "data/processed/us_sex.csv")
write_csv(df_race_ethnicity, "data/processed/us_race_ethnicity.csv")
