suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(tidyverse)
  library(jsonlite)
  library(r2d3)
  library(here)
})

# sYYYY -- faculty race/gender
# efYYYYa -- number of students enrolled in the fall, by race/ethnicity, gender, 
# attendance (full- or part-time) status and level of student
# efYYYYcp --  number of students enrolled in the fall, by race/ethnicity, 
# gender, attendance (full- or part-time) status and level of student for 
# selected major fields of study
fac <- read_csv("data/processed/s_long.csv")
stu_enroll <- read_csv("data/processed/efa_long.csv")
stu_dc <- read_csv("data/processed/c_long.csv")
json <- jsonlite::read_json("data/processed/us.json")

# get all uni_id that have data for both 2012 and 2018 in all 3 datasets
get_unique_ids <- function(df) {
  df %>% 
    filter(year == 2012 | year == 2018) %>%
    group_by(uni_id) %>%
    mutate(n_years = length(unique(year))) %>% 
    filter(n_years == 2) %>% pull(uni_id) %>% unique()
}

fac_uni_ids <- get_unique_ids(fac)
stu_enroll_uni_ids <- get_unique_ids(stu_enroll)
stu_dc_uni_ids <- get_unique_ids(stu_dc)
uni_ids <- intersect(intersect(fac_uni_ids, stu_enroll_uni_ids), stu_dc_uni_ids)
fac <- fac %>% filter(uni_id %in% uni_ids) 
stu_enroll <- stu_enroll %>% filter(uni_id %in% uni_ids)
stu_dc <- stu_dc %>% filter(uni_id %in% uni_ids)

# make dataset
us <- read_csv("data/processed/census_race_sex_2018.csv") %>% 
  rename("gender" = sex) %>%
  mutate(year = 2018)

df_long <- bind_rows(list("Faculty" = fac, 
                          "Students enrolled" = stu_enroll, 
                          "Degrees completed" = stu_dc, 
                          "U.S. population" = us), 
                          .id = "group")

clean_long_data <- function(df) {
  df %>% 
    mutate(
      race = case_when(
        race == "asian" ~ "Asian", 
        race == "black" ~ "Black or African American",
        race == "hispanic" ~ "Hispanic or Latino",
        race == "mixed" ~ "Two or more races",
        race == "native" ~ "American Indian and Alaska Native",
        race == "nonresident" ~ "Non-resident",
        race == "pacificnative" ~ "Native Hawaiian and Other Pacific Islander",
        race == "unknown" ~ "Other/Unknown",
        race == "Other"  ~ "Other/Unknown",
        race == "white" ~ "White",
        race == "Two or More Races" ~ "Two or more races",
        race == "Hispanic" ~ "Hispanic or Latino",
        TRUE ~ race
      ), 
      race = factor(race, ordered = TRUE, 
                    levels = c("White", 
                                "Hispanic or Latino", 
                                "Black or African American", 
                                "Asian",
                                "American Indian and Alaska Native",  
                                "Native Hawaiian and Other Pacific Islander", 
                                "Two or more races",
                                "Non-resident",
                                "Other/Unknown", 
                                "Total")),
      race = replace_na(race, "Total"),
      sex = case_when(
        gender == "total" ~ "Total", 
        TRUE ~ stringr::str_to_sentence(gender)
      ), 
      sex = replace_na(sex, "Total"), 
      group = factor(group, ordered = TRUE, 
                     levels = c("U.S. population", 
                                "Students enrolled", 
                                "Degrees completed", 
                                "Faculty")),
      status_level = if_else(is.na(status_level), "Total", stringr::str_to_sentence(status_level)), 
      instructional_staff_category  = if_else(is.na(instructional_staff_category ), 
                                              "Total", stringr::str_to_sentence(instructional_staff_category))) %>% 
    select(-gender) 
}

df_long <- clean_long_data(df_long) %>% filter(year == 2012 | year == 2018)
colSums(is.na(df_long))

# 2018 race by uni
df_long_2018 <- df_long %>% 
  filter(year == 2018) %>%
  filter(race != "Total", 
         sex == "Total", 
         instructional_staff_category == "Total", 
         status_level == "Total") %>%
  group_by(group, race, uni_id) %>%
  summarize(n = sum(n, na.rm = T)) %>%
  group_by(group, uni_id) %>%
  mutate(percent = n/sum(n)) 

write_csv(df_long_2018, "data/processed/all_race_2018.csv")

df_long_2012_2018 <- df_long %>% 
  filter(year == 2018 | year == 2012) %>%
  filter(race != "Total", 
         sex == "Total", 
         instructional_staff_category == "Total", 
         status_level == "Total") %>%
  group_by(group, race, uni_id, year) %>%
  summarize(n = sum(n, na.rm = T)) %>%
  group_by(group, uni_id, year) %>%
  mutate(percent = n/sum(n)) 

write_csv(df_long_2012_2018, "data/processed/all_race_2012_2018.csv")


# summarize race/group level as check 
overall_race <- df_long %>% 
  filter(race != "Total", 
         sex == "Total", 
         instructional_staff_category == "Total", 
         status_level == "Total") %>%
  group_by(group, year, race) %>%
  summarize(n = sum(n, na.rm = T)) %>%
  group_by(group, year) %>%
  mutate(percent = n/sum(n)) 

write_csv(overall_race, "data/processed/overall_race.csv")

# get uni lat/long together for map
unis <- fac %>% 
  group_by(uni_id) %>%
  select(uni_id, longitude, latitude, uni) %>%
  slice(1)

write_csv(unis, "data/processed/uni_locs.csv")

