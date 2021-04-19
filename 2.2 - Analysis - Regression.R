library(tidyverse)
library(survival)
library(survminer)
library(gtsummary)
library(broom)

db <- 
  readRDS("unos_ped_data.rds") %>% 
  select(PSTATUS, PTIME, CARDARREST_POSTNEURO_DON,
         AGE,GENDER,BMI_CALC,BMI_MMTCH,WEIGHT_MMTCH,MOST_RCNT_CREAT,
         DISTANCE,LOS,
         AGE_DON,ETHCAT_DON_grp,CREAT_DON,HIST_CIG_DON,
         CLIN_INFECT_DON,ARGININE_DON,PT_DIURETICS_DON,ANTIHYPE_DON
         ) %>% 
  mutate(mort_30 = ifelse(PSTATUS == 1 & PTIME <= 30, 1, 0),
         mort_1yr = ifelse(PSTATUS == 1 & PTIME <= 365, 1, 0),
         mort_3yr = ifelse(PSTATUS == 1 & PTIME <= 1095, 1, 0),
         AGE_grp = ifelse(AGE <= 5, 1, 
                          ifelse(AGE <= 12, 2, 3)))

db %>% 
  group_by(AGE_grp) %>% 
  summarise(min = min(AGE), max = max(AGE))

# complete case analysis - losing 23%
db_complete <-
  db %>% 
  filter(complete.cases(.))
  
# KAPLAN-MEIER #

km <- survfit(Surv(PTIME, PSTATUS) ~ CARDARREST_POSTNEURO_DON, data = db)

ggsurvplot(km, 
           surv.scale = 'percent',
           xscale = 365.25,
           break.time.by = 365.25,
           xlab = 'Time (years)',
           pval = TRUE,
           pval.coord = c(7500, 1),
           legend = 'top',
           legend.title = '',
           legend.labs = c("No cardiac arrest", "Donor cardiac arrest"),
           censor = TRUE,
           conf.int = TRUE,
           data = db)

ggsave("Output/km_plot.png", height=7, width=8)

# KAPLAN-MEIER - STRATIFIED BY AGE #

km2 <- survfit(Surv(PTIME, PSTATUS) ~ CARDARREST_POSTNEURO_DON + strata(AGE_grp), data = db)

# broom::tidy(km2) %>% 
#   group_by(strata) %>% 
#   summarise(n=n()) %>% 
#   pull(strata)

ggsurvplot(km2, 
           surv.scale = 'percent',
           xscale = 365.25,
           break.time.by = 365.25,
           xlab = 'Time (years)',
           pval = TRUE,
           pval.coord = c(7500, 1),
           legend = 'top',
           legend.title = '',
           legend.labs = c("No cardiac arrest - Infant", "No cardiac arrest - School Age", "No cardiac arrest - Teenager",
                           "Donor cardiac arrest - Infant", "Donor cardiac arrest - School Age", "Donor cardiac arrest - Teenager"),
           censor = TRUE,
           data = db)

ggsurvplot_facet(km, data = db, facet.by = "AGE_grp",
                 surv.scale = 'percent',
                 xscale = 365.25,
                 break.time.by = 365.25*2,
                 xlab = 'Time (years)',
                 pval = TRUE,
                 pval.coord = c(7000, 1),
                 legend.title = '',
                 legend.labs = c("No cardiac arrest", "Donor cardiac arrest")
)

ggsave("Output/km_plot_AGE.png", height=7, width=8)

###########################
# LOGISTIC/COX REGRESSION #
###########################

t1 <- 
  db_complete %>% 
  select(-c(PSTATUS, PTIME, mort_1yr, mort_3yr)) %>% 
  glm(mort_30 ~ ., data = .) %>% 
  tbl_regression(label = list(CARDARREST_POSTNEURO_DON ~ "30-day mortality"),
                 exponentiate = TRUE,
                 include = "CARDARREST_POSTNEURO_DON",
                 show_single_row = "CARDARREST_POSTNEURO_DON") %>% 
  modify_header(list(
    label ~"**Donor cardiac arrest prediction of mortality**",
    estimate ~ "**Adjusted odds/hazard ratio**"))

t2 <- 
  db_complete %>% 
  select(-c(PSTATUS, PTIME, mort_30, mort_3yr)) %>% 
  glm(mort_1yr ~ ., data = .) %>% 
  tbl_regression(label = list(CARDARREST_POSTNEURO_DON ~ "1-year mortality"),
                 exponentiate = TRUE,
                 include = "CARDARREST_POSTNEURO_DON",
                 show_single_row = "CARDARREST_POSTNEURO_DON") %>% 
  modify_header(list(
    label ~"**Donor cardiac arrest prediction of mortality**",
    estimate ~ "**Adjusted odds/hazard ratio**"))

t3 <- 
  db_complete %>% 
  select(-c(PSTATUS, PTIME, mort_30, mort_1yr)) %>% 
  glm(mort_3yr ~ ., data = .) %>% 
  tbl_regression(label = list(CARDARREST_POSTNEURO_DON ~ "3-year mortality"),
                 exponentiate = TRUE,
                 include = "CARDARREST_POSTNEURO_DON",
                 show_single_row = "CARDARREST_POSTNEURO_DON") %>% 
  modify_header(list(
    label ~"**Donor cardiac arrest prediction of mortality**",
    estimate ~ "**Adjusted odds/hazard ratio**"))

t4 <-
  db_complete %>% 
  select(-c(mort_30, mort_1yr, mort_3yr)) %>% 
  coxph(Surv(PTIME, PSTATUS) ~ ., data = .) %>% 
  tbl_regression(label = list(CARDARREST_POSTNEURO_DON ~ "Overall mortality"),
                 exponentiate = TRUE,
                 include = "CARDARREST_POSTNEURO_DON",
                 show_single_row = "CARDARREST_POSTNEURO_DON") %>% 
  modify_header(list(
    label ~"**Donor cardiac arrest prediction of mortality**",
    estimate ~ "**Adjusted odds/hazard ratio**"))


tab2 <-
  tbl_stack(tbls = list(tbl_stack(tbls = list(t1, t2, t3)), t4),
          group_header = c("Multivariable logistic regression", 
                           "Multivariable Cox proportional hazard"))

########################
# SCHOENFELD RESIDUALS #
########################

fit_cox <-
  db_complete %>% 
  select(-c(mort_30, mort_1yr, mort_3yr)) %>% 
  coxph(Surv(PTIME, PSTATUS) ~ ., data = .)

sch <- cox.zph(fit_cox)
vars <- which(sch$table[,3] < 0.05) 





