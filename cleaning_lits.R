
library(tidyverse)
library(haven)
library(readxl)

##################

##Data cleaning

lits_ii <- read.csv("/Users/roland/Desktop/singer 2018 replicate and extend/main/lits_ii.csv")

#gdp_imf data
imf_data <- read_excel("/Users/roland/Desktop/singer 2018 replicate and extend/main/imf_gdp_imf.xls")
imf_data[183, 1] <- "Turkey"
imf_data[145, 1] <- "Russia"
imf_data[130, 1] <- "Macedonia"

imf <- imf_data[, c("Real gdp_imf growth (Annual percent change)", "2010")]


imf <- imf[-1,]
colnames(imf) <- c("country", "gdp_imf")
imf$gdp_imf <- as.numeric(imf$gdp_imf)
imf$gdp_imf <- round(imf$gdp_imf, 2)

##################




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

imf <- imf %>%
  mutate(country = trimws(country),
         country = as.character(country))

imf <- imf %>%
  filter(as.character(country) %in% countries)

##################



lits_ii <- left_join(lits_ii, imf, by = "country")
unique(lits_ii$country)
length(unique(lits_ii$country))

unique(lits_ii$gdp_imf)

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

lits_ii$gov_approval <- ordered(lits_ii$gov_approval)
typeof(lits_ii$gov_approval)

make_binary <- function(x) {
  case_when(
    x == "-90" ~ NA_real_,
    grepl("^NOT", x, ignore.case = TRUE) ~ 0,
    grepl("^[A-Z]", x) ~ 1,
    TRUE ~ NA_real_
  )
}


#
#make_binary <- function(x) {
#case_when(
#  x == "-90" ~ NA_real_,
# grepl("^NOT", x, ignore.case = TRUE) ~ 0,
# grepl("^[A-Z]", x) ~ 1,
# TRUE ~ NA_real_
#)


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

lits_ii <- lits_ii %>%
  mutate(
    # Convert raw items to binary first
    dont_say_q804 = not_stated_function(q804t_01),
    dont_know_q804 = make_binary(q804u_97),
    head_job_loss   = make_binary(q802aa),
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
    age = factor(age),
    
    #"A FAIR AMOUNT" "NOT AT ALL"    "A GREAT DEAL"  "JUST A LITTLE" "Refused"       "Don't know"  
    egotropic = case_when(
      q801 == "A GREAT DEAL" ~ 3,
      q801 == "A FAIR AMOUNT" ~ 2,
      q801 == "JUST A LITTLE" ~ 1,
      q801 == "NOT AT ALL" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Respondent gender: 0 = male, 1 = female
    female = factor(ifelse(respondentgender == -1, NA, respondentgender)), 
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
    education = factor(education, levels = 1:7, ordered = TRUE),
    
    class = case_when(
      q227 %in% 1:10 ~ as.numeric(q227),
      TRUE           ~ NA_real_
    ),
    class = factor(class, levels = 1:10, ordered = TRUE)
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
    
    labour_shocks_agg = if_else(
      rowSums(across(c(head_job_loss, other_job_loss, fam_business, 
                       hrs_reduced, wage_delay, wage_reduction)) == 1, na.rm = TRUE) >= 1,
      1, 0
    ),
    
    labour_reactions_agg = if_else(
      rowSums(across(c(second_job, more_hours, new_job, further_ed)) == 1, na.rm = TRUE) >= 1,
      1, 0
    ),
    
    income_job_loss = if_else(
      rowSums(across(c(head_job_loss, other_job_loss, fam_business,
                       hrs_reduced, wage_delay, wage_reduction,
                       rem_reduces, fam_returned)) == 1, na.rm = TRUE) >= 1,
      1, 0
    ),
    
    income_job_loss_na_min_1 = if_else(
      rowSums(across(c(head_job_loss, other_job_loss, fam_business,
                       hrs_reduced, wage_delay, wage_reduction,
                       rem_reduces, fam_returned)) == 1, na.rm = TRUE) >= 1,
      1, 0
    )
  )
##################


aggregate_vars <- c(
  "doctor_med_cut",           # from q804j, q804l
  "utilities_cut",            # from q804n, q804o
  "welfare_applied",          # from welfare_1, welfare_2, welfare_3, welfare_4
  "borrow_money",            # from q805
  "active_coping",            # from second_job, more_hours, new_job, sell_asset, forced_move, further_ed
  "essentials_reduced",          # from staples_reduced (original), doctor_med_cut, utilities_cut
  "luxury_reduced",           # from luxury_reduced (original), alcohol_reduced, car_reduced, vacation_cut, tobacco_cut, uni_postponed, tv_phone_cut
  "passive_coping",           # from staples_reduced, luxury_reduced
  "staples_reduced_and_luxury", # from staples_reduced, luxury_reduced
  "labour_shocks_agg",        # from head_job_loss, other_job_loss, fam_business, hrs_reduced, wage_delay, wage_reduction
  "labour_reactions_agg",     # from second_job, more_hours, new_job, further_ed
  "income_job_loss"           # from head_job_loss, other_job_loss, fam_business, hrs_reduced, wage_delay, wage_reduction, rem_reduces, fam_returned
)

# INDIVIDUAL VARIABLES (created from single survey questions):
individual_vars <- c(
  # Job/Labour variables
  "egotropic", #q801
  "head_job_loss",     # q802aa
  "other_job_loss",    # q802ab
  "fam_business",      # q802ac
  "hrs_reduced",       # q802ad
  "wage_delay",        # q802ae
  "wage_reduction",    # q802af
  "rem_reduces",       # q802ag
  "fam_returned",      # q802ah
  "second_job",        # q802ai
  "more_hours",        # q802aj
  "new_job",           # q802ak
  
  # Coping strategies
  "sell_asset",        # q804r
  "forced_move",       # q804s
  "further_ed",        # q804h
  "staples_reduced",   # q804a (original, gets combined into aggregate)
  "luxury_reduced",    # q804b (original, gets combined into aggregate)
  "alcohol_reduced",   # q804c
  "car_reduced",       # q804d
  "vacation_cut",      # q804e
  "tobacco_cut",       # q804f
  "uni_postponed",     # q804g
  "tv_phone_cut",      # q804p
  
  # Financial assistance
  "borrow_money",      # q805
  "welfare_1",         # q812_1
  "welfare_2",         # q812_2
  "welfare_3",         # q812_3
  "welfare_4"          # q812_4
)

#control_vars 

control_vars <- c("age", "female", "education", "class")
##################


#almost perfect part 3
#treat -90 and -1 differently 

df_job_loss <- lits_ii %>%
  select(country, egotropic, gov_approval, head_job_loss, other_job_loss, fam_business,
         hrs_reduced, wage_delay, wage_reduction,
         rem_reduces, fam_returned, all_of(control_vars), dont_say_q804, dont_know_q804, income_job_loss, passive_coping, active_coping, luxury_reduced, essentials_reduced, borrow_money, welfare_applied, gdp, polity_score_2010, labour_reactions_agg, labour_shocks_agg)


df_job_loss <- lits_ii %>%
  select(country, gdp, polity_score_2010, gov_approval, all_of(aggregate_vars), all_of(individual_vars), all_of(control_vars), dont_say_q804, dont_know_q804)


#drop if dep and ind vars are NA
df_analysis <- df_job_loss %>%
  mutate(head_job_loss = ifelse(egotropic == 0, 1, head_job_loss)) %>% #except when respondent states crisis has had no impact, in which case we assume everything is 0
  filter(!is.na(gov_approval),
         !is.na(egotropic),
         !is.na(head_job_loss)) 

#drop if controls are NA
df_analysis <- df_analysis %>%
  filter(!is.na(class), 
         !is.na(female), 
         !is.na(age), 
         !is.na(education))

df_analysis <- df_analysis %>%
  filter((income_job_loss == 0 | egotropic == 0 | dont_know_q804 == 0 | passive_coping == 1 | active_coping == 1 | luxury_reduced == 1 | essentials_reduced == 1 | borrow_money == 1 | welfare_applied == 1))

save(df_analysis, file = "lits_analysis.RData")

##################
