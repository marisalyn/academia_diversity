library(tidyverse)
library(readxl)
library(stringi)
library(assertthat)

# read in data files and data dictionaries -------------------------------------
data_files <- list.files("data/raw")
data_files <- data_files[data_files != "ACSDP05Y2018.csv"] # clean census data separately!
data <- map(data_files, ~read_csv(file.path("data/raw", .)))
names(data) <- data_files %>% str_extract(., "[^_|.]+") %>% tolower()

data_dicts <- list.files("docs")
data_dicts <- data_dicts[!grepl("~", data_dicts)]
dicts <- map(data_dicts, ~readxl::read_xlsx(file.path("docs", .), sheet = "varlist"))
names(dicts) <- data_dicts %>% str_extract(., "[^_|.]+") 

assert_that(all(names(data) %in% names(dicts)))

# functions for general data cleaning ------------------------------------------
clean_vars <- function(data_name) {
  xwalk <- dicts[[data_name]] %>% 
    select(varname, 
           vartitle = varTitle, 
           type = DataType) %>%
    mutate(vartitle = gsub(" ", "_", tolower(vartitle)), 
           vartitle = gsub("american_indian_or_alaska_native", "native", vartitle), 
           vartitle = gsub("black_or_african_american", "black", vartitle), 
           vartitle = gsub("hispanic_or_latino", "hispanic", vartitle), 
           vartitle = gsub("two_or_more_races", "mixed", vartitle), 
           vartitle = gsub("race/ethnicity_unknown", "unknown", vartitle), 
           vartitle = gsub("nonresident_alien", "nonresident", vartitle), 
           vartitle = gsub("grand_total", "total", vartitle), 
           vartitle = gsub("native_hawaiian_or_other_pacific_islander", "pacificnative", vartitle)) %>%
    filter(varname != "LSTUDY")
  
  # rename variables
  varnames <- xwalk %>% pull(varname)
  names(varnames) <- xwalk %>% pull(vartitle)
  data[[data_name]] <- select(data[[data_name]], varnames)
  
  # clean up data types
  num_cols <- xwalk %>% filter(type == "N") %>% pull(vartitle) 
  char_cols <- xwalk %>% filter(type == "A") %>% pull(vartitle) 
  
  data[[data_name]] <- data[[data_name]] %>%
    mutate_at(char_cols, as.character) %>%
    mutate_at(num_cols, as.numeric) %>%
    rename("uni_id" = "unique_identification_number_of_the_institution")

  return(data[[data_name]])
}

add_uni_info <- function(data_name) { 
  year <- gsub("[a-z]", "", tolower(data_name))
  
  uni_name_xwalk <- data[[paste0("hd", year)]] %>% 
    select("uni_id", 
           "uni" = "institution_(entity)_name", 
           "longitude" = longitude_location_of_institution, 
           "latitude" =latitude_location_of_institution) %>%
    mutate(uni = stringi::stri_trans_general(uni, id = "Latin-ASCII"))
  
  data[[data_name]] <- left_join(data[[data_name]], uni_name_xwalk, by= "uni_id")
  return(data[[data_name]])
}

pivot_long <- function(data_name){
  id_cols_name <- gsub("[[:digit:]]", "", data_name)
  data_long <- data_w_uni[[data_name]] %>%
    select(-contains("cip_code_for_major_field_of_study"), 
           -contains("original_line_number_on_survey_form"), 
           -contains("faculty_and_tenure_status"), 
           -contains("academic_rank"), 
           -contains("attendance"), 
           -contains("level_of_student_2")) %>% 
    pivot_longer(cols = -c(id_cols[[id_cols_name]]), 
                 names_to = c("race", "gender"),
                 names_sep = "_", 
                 values_to = "n") %>% 
    mutate(gender = replace_na(gender, "total"))
  
  return(data_long)
}


clean_vars_2 <- function(data, data_name, pivot){
  # cYYYYb --  number of students who completed any degree or certificate 
  # by race/ethnicity and gender
  # sYYYY -- faculty race/gender
  # efYYYYa -- number of students enrolled in the fall, by race/ethnicity, gender, 
  # attendance (full- or part-time) status and level of student
  # efYYYYcp --  number of students enrolled in the fall, by race/ethnicity, 
  # gender, attendance (full- or part-time) status and level of student for 
  # selected major fields of study
  
  year <- gsub("[a-z]", "", tolower(data_name))
  data_key <- gsub("[[:digit:]]", "", tolower(data_name))
  
  if (data_key == "cb") {
    df <- data 
  } else if (data_key == "s") {
    df <- data %>% 
      mutate(
        instructional_staff_category = case_when(
          instructional_staff_category == 1	  ~ "total",
          instructional_staff_category == 101 ~ "Professors",
          instructional_staff_category == 102 ~ "Associate professors",
          instructional_staff_category == 103 ~ "Assistant professors",
          instructional_staff_category == 104 ~ "Instructors",
          instructional_staff_category == 105 ~ "Lecturers",
          instructional_staff_category == 106 ~ "No academic rank",
          instructional_staff_category == 200 ~ "Tenured",
          instructional_staff_category == 201 ~ "Tenured, professors",
          instructional_staff_category == 202 ~ "Tenured, associate professors",
          instructional_staff_category == 203 ~ "Tenured, assistant professors",
          instructional_staff_category == 204 ~ "Tenured, instructors",
          instructional_staff_category == 205 ~ "Tenured, lecturers",
          instructional_staff_category == 206 ~ "Tenured, no academic rank",
          instructional_staff_category == 300 ~ "On-Tenure track",
          instructional_staff_category == 301 ~ "On-tenure track, professors",
          instructional_staff_category == 302 ~ "On-tenure track, associate professors",
          instructional_staff_category == 303 ~ "On-tenure track, assistant professors",
          instructional_staff_category == 304 ~ "On-tenure track, instructors",
          instructional_staff_category == 305 ~ "On-tenure track, lecturers",
          instructional_staff_category == 306 ~ "On-tenure track, no academic rank",
          instructional_staff_category == 400 ~ "Not on tenure track/No tenure system",
          instructional_staff_category == 401 ~ "Not on tenure/no tenure system, professors",
          instructional_staff_category == 402 ~ "Not on tenure/no tenure system, associate professors",
          instructional_staff_category == 403 ~ "Not on tenure/no tenure system, assistant professors",
          instructional_staff_category == 404 ~ "Not on tenure/no tenure system, instructors",
          instructional_staff_category == 405 ~ "Not on tenure/no tenure system, lecturers",
          instructional_staff_category == 406 ~ "Not on tenure/no tenure system, no academic rank",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(instructional_staff_category))  
    
    if (pivot){
      df <- df %>%
        #separate(instructional_staff_category, into = c("tenure", "rank"), sep = ", ") %>%
        rename_at(vars(-contains("uni")), ~paste0("fac_", .)) %>% 
        pivot_wider(names_from = fac_instructional_staff_category, values_from = contains("fac")) %>%
        rename_all(~gsub(", ", "_", .)) %>%
        rename_all(~gsub("-", "", .))
    }

    } else if( data_key == "efa") {
    df <- data %>%
      mutate(
        level_of_student = case_when(
          level_of_student == 1	  ~ "Total",       
          level_of_student == 2	  ~ "Undergraduate",         
          level_of_student == 12	~ "Graduate",       
          level_of_student == 16	~ "First professional",       
          level_of_student == 21	~ "Full-time",       
          level_of_student == 22	~ "Full-time, Undergraduate",            
          level_of_student == 32	~ "Full-time, Graduate",       
          level_of_student == 36	~ "Full-time, First professional",       
          level_of_student == 41	~ "Part-time",       
          level_of_student == 42	~ "Part-time, Undergraduate",           
          level_of_student == 52	~ "Part-time, Graduate",       
          level_of_student == 56	~ "Part-time, First professional", 
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(level_of_student))
    
    if (pivot) {
      df <- df  %>%
        #separate(level_of_student, into = c("status", "level"), sep = ", ")%>%
        rename_at(vars(-contains("uni")), ~paste0("stu_", .)) %>% 
        pivot_wider(names_from = stu_level_of_student, values_from = contains("stu")) %>%
        rename_all(~gsub(", ", "_", .)) %>%
        rename_all(~gsub("-", "", .))
    }

    } else if (data_key == "efcp"){
    df <- data %>%
      mutate(
        major_field_of_study = case_when(
          # subject, full/part time, grad/undergrad, degree seeking
          major_field_of_study == 101 ~	"Education",
          major_field_of_study == 102 ~	"Education, Undergraduate",
          major_field_of_study == 112 ~	"Education, Graduate",
          major_field_of_study == 121 ~	"Education, Full-time",
          major_field_of_study == 122 ~	"Education, Full-time, Undergraduate",
          major_field_of_study == 132 ~	"Education, Full-time, Graduate",
          major_field_of_study == 141 ~	"Education, Part-time",
          major_field_of_study == 142 ~	"Education, Part-time, Undergraduate",
          major_field_of_study == 152 ~	"Education, Part-time, Graduate",
          major_field_of_study == 201 ~	"Engineering",
          major_field_of_study == 202 ~	"Engineering, Undergraduate",
          major_field_of_study == 212 ~	"Engineering, Graduate",
          major_field_of_study == 221 ~	"Engineering, Full-time",
          major_field_of_study == 222 ~	"Engineering, Full-time, Undergraduate",
          major_field_of_study == 232 ~	"Engineering, Full-time, Graduate",
          major_field_of_study == 241 ~	"Engineering, Part-time",
          major_field_of_study == 242 ~	"Engineering, Part-time, Undergraduate",
          major_field_of_study == 252 ~	"Engineering, Part-time, Graduate",
          major_field_of_study == 301 ~	"Biological Sciences/Life Sciences",
          major_field_of_study == 302 ~	"Biological Sciences/Life Sciences, Undergraduate",
          major_field_of_study == 312 ~	"Biological Sciences/Life Sciences, Graduate",
          major_field_of_study == 321 ~	"Biological Sciences/Life Sciences, Full-time",
          major_field_of_study == 322 ~	"Biological Sciences/Life Sciences, Full-time, Undergraduate",
          major_field_of_study == 332 ~	"Biological Sciences/Life Sciences, Full-time, Graduate",
          major_field_of_study == 341 ~	"Biological Sciences/Life Sciences, Part-time",
          major_field_of_study == 342 ~	"Biological Sciences/Life Sciences, Part-time, Undergraduate",
          major_field_of_study == 352 ~	"Biological Sciences/Life Sciences, Part-time, Graduate",
          major_field_of_study == 401 ~	"Mathematics",
          major_field_of_study == 402 ~	"Mathematics, Undergraduate",
          major_field_of_study == 412 ~	"Mathematics, Graduate",
          major_field_of_study == 421 ~	"Mathematics, Full-time",
          major_field_of_study == 422 ~	"Mathematics, Full-time, Undergraduate",
          major_field_of_study == 432 ~	"Mathematics, Full-time, Graduate",
          major_field_of_study == 441 ~	"Mathematics, Part-time",
          major_field_of_study == 442 ~	"Mathematics, Part-time, Undergraduate",
          major_field_of_study == 452 ~	"Mathematics, Part-time, Graduate",
          major_field_of_study == 501 ~	"Physical Sciences",
          major_field_of_study == 502 ~	"Physical Sciences, Undergraduate",
          major_field_of_study == 512 ~	"Physical Sciences, Graduate",
          major_field_of_study == 521 ~	"Physical Sciences, Full-time",
          major_field_of_study == 522 ~	"Physical Sciences, Full-time, Undergraduate",
          major_field_of_study == 532 ~	"Physical Sciences, Full-time, Graduate",
          major_field_of_study == 541 ~	"Physical Sciences, Part-time",
          major_field_of_study == 542 ~	"Physical Sciences, Part-time, Undergraduate",
          major_field_of_study == 552 ~	"Physical Sciences, Part-time, Graduate",
          major_field_of_study == 601 ~	"Business Management and Administrative Services",
          major_field_of_study == 602 ~	"Business Management and Administrative Services, Undergraduate",
          major_field_of_study == 612 ~	"Business Management and Administrative Services, Graduate",
          major_field_of_study == 621 ~	"Business Management and Administrative Services, Full-time",
          major_field_of_study == 622 ~	"Business Management and Administrative Services, Full-time, Undergraduate",
          major_field_of_study == 632 ~	"Business Management and Administrative Services, Full-time, Graduate",
          major_field_of_study == 641 ~	"Business Management and Administrative Services, Part-time",
          major_field_of_study == 642 ~	"Business Management and Administrative Services, Part-time, Undergraduate",
          major_field_of_study == 652 ~	"Business Management and Administrative Services, Part-time, Graduate",
          major_field_of_study == 716 ~	"Law",
          major_field_of_study == 736 ~	"Law, Full time",
          major_field_of_study == 756 ~	"Law, Part time",
          major_field_of_study == 816 ~	"Dentistry",
          major_field_of_study == 836 ~	"Dentistry, Full time",
          major_field_of_study == 856 ~	"Dentistry, Part time",
          major_field_of_study == 916 ~	"Medicine",
          major_field_of_study == 936 ~	"Medicine, Full time",
          major_field_of_study == 956 ~	"Medicine, Part time",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(major_field_of_study))
    
    if (pivot) {
      df <- df  %>%
        # separate(major_field_of_study, into = c("subject", "status", "level", "degree_seeking"), sep = ", ") %>%
        rename_at(vars(-contains("uni")), ~paste0("stu_field_", .)) %>% 
        pivot_wider(names_from = stu_field_major_field_of_study, values_from = contains("stu_field"))%>%
        rename_all(~gsub(", ", "_", .)) %>%
        rename_all(~gsub("-", "", .))
    }
    
  }

  df <- df %>% mutate(year = as.numeric(year))
  
  return(df)  
}

# id_cols ----------------------------------------------------------------------
id_cols <- list(
  "s" = c(
    "uni_id", 
    "uni", 
    "latitude", 
    "longitude",
    "instructional_staff_category"
  ), 
  "efcp" = c(
    "uni_id", 
    "uni", 
    "latitude", 
    "longitude",
    "major_field_of_study"
  ), 
  "efa" = c(
    "uni_id", 
    "uni", 
    "latitude", 
    "longitude",
    "level_of_student"
  ),
  "cb" = c(
    "uni_id", 
    "uni",
    "latitude", 
    "longitude"
  )
)

# data cleaning  -------------------------------------------------------
data <- map(names(data), ~clean_vars(.))
names(data) <- data_files %>% str_extract(., "[^_|.]+") %>% tolower(.)

data_w_uni <- map(names(data)[!grepl("hd", names(data))], ~add_uni_info(.))
names(data_w_uni) <- names(data)[!grepl("hd", names(data))]

data_long <- map(names(data_w_uni), ~pivot_long(.))
data_long <- map2(data_long, names(data_w_uni), ~clean_vars_2(.x, .y, pivot = FALSE))
names(data_long) <- names(data_w_uni)

# combine into long datasets ----------------------------------------------------
c <- bind_rows(data_long["c2012b"], 
               data_long["c2014b"], 
               data_long["c2016b"], 
               data_long["c2018b"])

efa <- bind_rows(data_long["ef2012a"], 
                 data_long["ef2014a"], 
                 data_long["ef2016a"],
                 data_long["ef2018a"]) %>% 
  rename(status_level = level_of_student)

efcp <- bind_rows(data_long["ef2012cp"], 
                  data_long["ef2014cp"], 
                  data_long["ef2016cp"],
                  data_long["ef2018cp"]) %>% 
  rename(field_status_level = major_field_of_study)

s <- bind_rows(data_long["s2012"], 
               data_long["s2014"], 
               data_long["s2016"], 
               data_long["s2018"])

write_csv(c, paste0("data/processed/","c_long.csv"))
write_csv(efa, paste0("data/processed/","efa_long.csv"))
write_csv(efcp, paste0("data/processed/","efcp_long.csv"))
write_csv(s, paste0("data/processed/","s_long.csv"))


# rm(data_long)
# rm(data)

