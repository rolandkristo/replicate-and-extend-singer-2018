
library(tidyverse)
library(haven)
library(readxl)

##################

dir <- getwd()

setwd(dir)

##Data cleaning

lits_ii <- read.csv("lits_ii.csv")
#Sample

lits_ii <- lits_ii %>%
  mutate(country_ = trimws(country_),
         country_ = as.character(country_))

lits_ii$country <- as.character(lits_ii$country_)

countries <- c("Albania", "Armenia", "Azerbaijan", "Croatia", "Estonia", 
               "France", "Georgia", "Germany", "Italy", "Kosovo", 
               "Lithuania", "Macedonia", "Mongolia", "Montenegro", 
               "Poland", "Romania", "Russia", "Serbia", "Slovenia", 
               "Sweden", "Turkey")

lits_ii_full <- lits_ii

lits_ii <- lits_ii %>%
  filter(as.character(country_) %in% countries)

##################

country_reference <- data.frame(
  country = c("Albania", "Armenia", "Azerbaijan", "Croatia", "Estonia", 
              "France", "Georgia", "Germany", "Italy", "Kosovo", 
              "Lithuania", "Macedonia", "Mongolia", "Montenegro", 
              "Poland", "Romania", "Russia", "Serbia", "Slovenia", 
              "Sweden", "Turkey"),
  gdp = c(3.5, 2.1, 5.0, -1.2, 3.1, 1.4, 6.4, 3.6, 1.3, 4.0,
          1.3, 1.8, 6.4, 1.1, 3.8, -1.3, 4.0, 1.0, 1.2, 5.7, 8.9),
  polity_score_2010 = c(9, 5, 8, 9, 9, 9, 6, 10, 10, 8, 10, 9, 10, 9, 10, 9, 4, 8, 10, 10, 7)
)


# Then merge with your analysis data
lits_ii <- lits_ii %>%
  left_join(country_reference, by = "country")

##################


###Recode Variables

#Following instructions from appendix 
#Government Approval 1 to 5. Are don't knows dropped? I assume so.

lits_ii$gov_approval <- recode(lits_ii$q620c,
                               "VERY BAD"   = 1,
                               "BAD"        = 2,
                               "NEITHER"    = 3,
                               "GOOD"       = 4,
                               "VERY GOOD"  = 5,
                               "Don't know" = NA_real_,
                               .default = NA_real_   # catch-all for everything else
)

lits_ii$gov_approval = factor(lits_ii$gov_approval, ordered = T)
typeof(lits_ii$gov_approval)

lits_ii$sociotropic_retrospective <- recode(lits_ii$q301a,
                               "Strongly agree"   = 1,
                               "Agree"        = 2,
                               "Neither disagree nor agree"    = 3,
                               "Disagree"       = 4,
                               "Strongly disagree"  = 5,
                               .default = NA_real_   # catch-all for everything else
)

table(lits_ii$sociotropic_retrospective, useNA = "ifany")

lits_ii$sociotropic_current <- recode(lits_ii$q301g,
                                       "Strongly agree"   = 1,
                                       "Agree"        = 2,
                                       "Neither disagree nor agree"    = 3,
                                       "Disagree"       = 4,
                                       "Strongly disagree"  = 5,
                                       .default = NA_real_   # catch-all for everything else
)

table(lits_ii$sociotropic_current, useNA = "ifany")

make_binary <- function(x) {
  case_when(
    x == "-90" ~ NA_real_,
    grepl("^NOT", x, ignore.case = TRUE) ~ 0,
    grepl("^[A-Z]", x) ~ 1,
    TRUE ~ NA_real_
  )
}

make_binary_crisis <- function(x) {
  case_when(
    x == "-90" ~ NA_real_,
    grepl("^NOT", x, ignore.case = TRUE) ~ 0,
    grepl("^[A-Z]", x) ~ 1,
    TRUE ~ NA_real_
  )
}



# Helper function for converting Yes/No/Refused to numeric
make_dummy_char <- function(x) {
  case_when(
    toupper(x) == "YES" ~ 1,
    toupper(x) == "NO"  ~ 0,
    TRUE                 ~ NA_real_
  )
}

not_stated_function <- function(x) {
  case_when(
    x == "NOT Not stated" ~ 0,          # recode to 1
    x == "Not stated"     ~ 1,          # recode to 0
    x == "-1"            ~ NA_real_,    # treat -1 as missing
    TRUE                 ~ NA_real_     # anything unexpected → NA
  )
}


#Control variables

lits_ii <- lits_ii %>%
  mutate(
    # Respondent age (assuming numeric already, otherwise convert)
    age = case_when(
      respondentage >= 18 & respondentage <= 24 ~ 1,
      respondentage >= 25 & respondentage <= 34 ~ 2,
      respondentage >= 35 & respondentage <= 44 ~ 3,
      respondentage >= 45 & respondentage <= 54 ~ 4,
      respondentage >= 55 & respondentage <= 64 ~ 5,
      respondentage >= 65                         ~ 6,
      TRUE                                        ~ NA_real_,),
    age = as.numeric(age),
    
    #"A FAIR AMOUNT" "NOT AT ALL"    "A GREAT DEAL"  "JUST A LITTLE" "Refused"       "Don't know"  
    egotropic = case_when(
      q801 == "A GREAT DEAL" ~ 3,
      q801 == "A FAIR AMOUNT" ~ 2,
      q801 == "JUST A LITTLE" ~ 1,
      q801 == "NOT AT ALL" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Respondent gender: 0 = male, 1 = female
    female = as.numeric((ifelse(respondentgender == -1, NA, respondentgender))), 
    education = case_when(
      q515 == "NO DEGREE / NO EDUCATION"             ~ 1,
      q515 == "PRIMARY EDUCATION"                    ~ 2,
      q515 == "LOWER SECONDARY EDUCATION"            ~ 3,
      q515 == "(UPPER) SECONDARY EDUCATION"          ~ 4,
      q515 == "POST-SECONDARY NON TERTIARY EDUCATION"~ 5,
      q515 == "BACHELOR'S DEGREE OR MORE"            ~ 6,
      q515 == "MASTER'S DEGREE OR PHD"               ~ 7,
      TRUE                                           ~ NA_real_
    ),
    education = as.numeric(education),
    
    class = case_when(
      q227 %in% 1:10 ~ as.numeric(q227),
      TRUE           ~ NA_real_
    ),
    class = as.numeric(class)
  )

lits_ii <- lits_ii %>%
  mutate(
    # Convert raw items to binary first
    dont_say_q804   = not_stated_function(q804t_01),
    dont_know_q804  = make_binary(q804u_97),
    head_job_loss   = make_binary(q802aa),
    other_job_loss  = make_binary(q802ab),
    fam_business    = make_binary(q802ac),
    hrs_reduced     = make_binary(q802ad),
    wage_delay      = make_binary(q802ae),
    wage_reduction  = make_binary(q802af),
    rem_reduces     = make_binary(q802ag),
    fam_returned    = make_binary(q802ah),
    
    second_job      = make_binary(q802ai),
    more_hours      = make_binary(q802aj),
    new_job         = make_binary(q802ak),
    sell_asset      = make_binary(q804r),
    forced_move     = make_binary(q804s),
    further_ed      = make_binary(q804h),
    
    staples_reduced = make_binary(q804a),
    doctor_cut      = make_binary(q804j),
    medication_cut  = make_binary(q804l),
    
    utilities_delayed = make_binary(q804n),
    utilities_cut = make_binary(q804o),
    
    luxury_goods_reduced  = make_binary(q804b),
    alcohol_reduced = make_binary(q804c),
    car_reduced     = make_binary(q804d),
    vacation_cut    = make_binary(q804e),
    tobacco_cut     = make_binary(q804f),
    uni_postponed   = make_binary(q804g),
    tv_phone_cut    = make_binary(q804p),
    
    borrow_money = make_dummy_char(q805),
    welfare_1 = make_dummy_char(q812_1),
    welfare_2 = make_dummy_char(q812_2),  
    welfare_3 = make_dummy_char(q812_3),
    welfare_4 = make_dummy_char(q812_4)
  )

lits_ii <- lits_ii %>%
  mutate(
    across(
      c(head_job_loss, other_job_loss, fam_business,
        hrs_reduced, wage_delay, wage_reduction,
        rem_reduces, fam_returned),
      ~ case_when(
        is.na(.x) & egotropic == 0 ~ 0,
        .x == 1 ~ 1,
        .x == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  )

table(lits_ii$dont_say_q804, useNA = "ifany")

lits_ii <- lits_ii %>%
  mutate(
    across(
      c(second_job, more_hours, new_job, sell_asset, forced_move, further_ed,
        staples_reduced, doctor_cut, medication_cut, utilities_delayed, utilities_cut, luxury_goods_reduced, alcohol_reduced, car_reduced, vacation_cut, tobacco_cut, uni_postponed, tv_phone_cut),
      ~ case_when(
        is.na(.x) & dont_say_q804 == 0 ~ 0,
        .x == 1 ~ 1,
        .x == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  )

##show unique value for each variable
unique(lits_ii$gov_approval)
unique(lits_ii$respondentgender)
unique(lits_ii$respondentage)


unique(lits_ii$age)
unique(lits_ii$female)
unique(lits_ii$education)
unique(lits_ii$class)
##################


##Integrate income_jobs_loss variable


# Rule: 1 if any are 1, 0 if none are 1 (ignoring NAs)
lits_ii <- lits_ii %>%
  mutate(
    
    # Intermediate variables - loose approach
    doctor_med_cut = if_else(
      rowSums(across(c(doctor_cut, medication_cut)) == 1, na.rm = TRUE) >= 1,
      1, 0
    ),
    
    utilities = if_else(
      rowSums(across(c(utilities_cut, utilities_delayed)) == 1, na.rm = TRUE) >= 1, 
      1, 0
    ),
    
    # Welfare applied - loose approach
    welfare_applied = if_else(
      rowSums(across(c(welfare_1, welfare_2, welfare_3, welfare_4)) == 1, na.rm = TRUE) >= 1,
      1, 0
    ),
    
    # Aggregate variables - loose approach
    active_coping = if_else(
      rowSums(across(c(second_job, more_hours, new_job, sell_asset, forced_move, further_ed)) == 1, na.rm = TRUE) >= 1,
      1, 0
    ),
    
    essentials_reduced = if_else(
      rowSums(across(c(staples_reduced, doctor_med_cut, utilities)) == 1, na.rm = TRUE) >= 1,
      1, 0
    ),
    
    luxury_reduced = if_else(
      rowSums(across(c(luxury_goods_reduced, alcohol_reduced, car_reduced, vacation_cut,
                       tobacco_cut, uni_postponed, tv_phone_cut)) == 1, na.rm = TRUE) >= 1,
      1, 0
    ),
    
    # Final composite variables - loose approach
    passive_coping = if_else(
      rowSums(across(c(essentials_reduced, luxury_reduced)) == 1, na.rm = TRUE) >= 1,
      1, 0
    ),
    
    essentials_and_luxury = if_else(
      essentials_reduced == 1 & luxury_reduced == 1,
      1, 0
    ),
    
    labour_shocks = case_when(
      # If at least one value is 1, assign 1
      rowSums(across(c(head_job_loss, other_job_loss, fam_business,
                       hrs_reduced, wage_reduction)) == 1, na.rm = TRUE) >= 1 ~ 1,
      # If all values are 0 OR egotropic is 0, assign 0
      rowSums(across(c(head_job_loss, other_job_loss, fam_business,
                       hrs_reduced, wage_reduction)), na.rm = TRUE) == 0 & 
        rowSums(is.na(across(c(head_job_loss, other_job_loss, fam_business,
                               hrs_reduced, wage_reduction)))) == 0 |
        egotropic == 0 ~ 0,
      # Otherwise assign NA
      TRUE ~ NA_real_
    ),
    
    labour_reactions = if_else(
      rowSums(across(c(second_job, more_hours, new_job, further_ed)) == 1, na.rm = TRUE) >= 1,
      1, 0
    ),
    
    income_job_loss = case_when(
      # If at least one value is 1, assign 1
      rowSums(across(c(head_job_loss, other_job_loss, fam_business,
                       hrs_reduced, wage_delay, wage_reduction,
                       rem_reduces, fam_returned)) == 1, na.rm = TRUE) >= 1 ~ 1,
      # If all values are 0 OR egotropic is 0, assign 0
      rowSums(across(c(head_job_loss, other_job_loss, fam_business,
                       hrs_reduced, wage_delay, wage_reduction,
                       rem_reduces, fam_returned)), na.rm = TRUE) == 0 & 
        rowSums(is.na(across(c(head_job_loss, other_job_loss, fam_business,
                               hrs_reduced, wage_delay, wage_reduction,
                               rem_reduces, fam_returned)))) == 0 |
        egotropic == 0 ~ 0,
      # Otherwise assign NA
      TRUE ~ NA_real_
    )
  )


##################
    

##################

control_vars <- c("age", "female", "education", "class")

aggregate_vars <- c("doctor_med_cut", "utilities", "welfare_applied", "borrow_money",
                    "active_coping", "essentials_reduced", "luxury_reduced",
                    "passive_coping", "essentials_and_luxury", "labour_shocks",
                    "labour_reactions", "income_job_loss")

individual_vars <- c("egotropic", "head_job_loss", "other_job_loss", "fam_business",
                     "hrs_reduced", "wage_delay", "wage_reduction", "rem_reduces",
                     "fam_returned", "second_job", "more_hours", "new_job", "sell_asset",
                     "forced_move", "further_ed", "staples_reduced", "luxury_reduced",
                     "alcohol_reduced", "car_reduced", "vacation_cut", "tobacco_cut",
                     "uni_postponed", "tv_phone_cut", "borrow_money", "welfare_1",
                     "welfare_2", "welfare_3", "welfare_4")


singer_vars <- c("gov_approval", "country", "gdp", "polity_score_2010", "income_job_loss", "active_coping", "essentials_reduced", "luxury_reduced", "essentials_and_luxury", "borrow_money", "welfare_applied", "age", "female", "education", "class")

labour_vars <- c("labour_shocks", "labour_reactions", "head_job_loss", "other_job_loss", "fam_business",
                     "hrs_reduced", "wage_delay", "wage_reduction", "rem_reduces",
                     "fam_returned", "egotropic", "second_job", "more_hours", "new_job", "further_ed")
labour_shock_vars <- c("head_job_loss", "other_job_loss", "fam_business",
                       "hrs_reduced")
labour_behaviour_vars <- c("second_job", "more_hours", "new_job", "further_ed")

income_shock_vars <- c("wage_delay", "wage_reduction", "rem_reduces",
                       "fam_returned")
#control_vars 


########

df_singer <- lits_ii %>%
  select(all_of(singer_vars), labour_shocks, labour_reactions) %>%
  na.omit()

df_micro <- lits_ii %>%
  select(all_of(singer_vars), labour_shocks, labour_reactions, labour_shock_vars, labour_behaviour_vars, income_shock_vars) %>%
  na.omit()

###robustness check when dropping egotropic == 0
df_no_ego <- lits_ii %>%
  select(all_of(singer_vars), egotropic) %>%
  filter(egotropic != 0) %>%
  na.omit()

##what about sociotropic? and egotropic
df_soc_ego <- lits_ii %>%
  select(all_of(singer_vars), labour_shocks, labour_reactions, egotropic, sociotropic_current, sociotropic_retrospective) %>%
  na.omit()

##################

#save datasets as Rdata
save(lits_ii, file = "lits_ii_cleaned.Rdata")
save(df_singer, file = "df_singer.Rdata")
save(df_micro, file = "df_micro.Rdata")
save(df_no_ego, file = "df_no_ego.Rdata")
save(df_soc_ego, file = "df_soc_ego.Rdata")
