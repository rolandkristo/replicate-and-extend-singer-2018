library(ordinal)

library(tidyverse)

# data -----------------------------------------------------------------

dir <- getwd()
setwd(dir)

load("df_singer.RData")

dat <- df_singer

dat_no_income <- dat[dat$income_job_loss==0,]
dat_income_shock <- dat[dat$income_job_loss==1,]


# replication
# ordinal with all REs


m1 <- clmm(gov_approval ~ income_job_loss + active_coping + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = dat, link = "logit")

m2 <- clmm(gov_approval ~ income_job_loss + active_coping + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = dat, link = "logit")

m3 <- clmm(gov_approval ~ active_coping + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = dat_no_income, link = "logit")
m3_alt <- clmm(gov_approval ~ active_coping + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age| country) + (1 + education| country) + (1 + female| country) + ( 1+ class | country), data = dat_no_income, link = "logit")

m4 <- clmm(gov_approval ~ active_coping + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = dat_income_shock, link = "logit")


m1$coefficients

m2$coefficients

m3$coefficients

m4$coefficients

#extension

#separate model
m1_agg <- clmm(gov_approval ~ labour_shocks + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = dat, link = "logit")

m2_agg <- clmm(gov_approval ~ labour_shocks + labour_reactions + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = dat, link = "logit")

m3_agg <- clmm(gov_approval ~ labour_shocks + labour_reactions + active_coping + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = dat_no_income, link = "logit")

m4_agg <- clmm(gov_approval ~ labour_shocks + labour_reactions + active_coping + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = dat_income_shock, link = "logit")

# labour reaction, holding other shocks constant 

#dissagregate
load("df_micro.RData")

df_micro_no_shock <- df_micro[df_micro$income_job_loss==0,]
df_micro_income_shock <- df_micro[df_micro$income_job_loss==1,]

m1_dis <- clmm(gov_approval ~ head_job_loss + other_job_loss + fam_business + hrs_reduced + wage_delay + wage_reduction + rem_reduces + fam_returned + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = df_micro, link = "logit")

m2_dis <- clmm(gov_approval ~ second_job + more_hours + new_job + further_ed + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = df_micro_no_shock, link = "logit")

m3_dis <- clmm(gov_approval ~ second_job + more_hours + new_job + further_ed + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = df_micro_income_shock, link = "logit")



load("df_soc_ego.Rdata")
m1_socio <- clmm(gov_approval ~ sociotropic_current + egotropic + labour_shocks + labour_reactions + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = df_soc_ego, link = "logit")

m2_socio <- clmm(gov_approval ~ sociotropic_current + egotropic + income_job_loss + active_coping + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = df_soc_ego, link = "logit")

load("df_no_ego.Rdata")


m2_no_ego <- clmm(gov_approval ~ income_job_loss + active_coping + essentials_reduced + luxury_reduced + essentials_and_luxury + borrow_money + welfare_applied + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = df_no_ego, link = "logit")

m1_no_ego <- clmm(gov_approval ~ income_job_loss + age + class + education + female + gdp + polity_score_2010 +(1 +  age + education + female + class | country), data = df_no_ego, link = "logit")

##
save(m1, m2, m3, m4,
     m1_agg, m2_agg, m3_agg, m4_agg,
     m1_dis, m2_dis, m3_dis,
     m1_socio, m2_socio,
     m1_no_ego, m2_no_ego,
     file = "regression_outputs.RData")
