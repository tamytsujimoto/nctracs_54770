library(tidyverse)
library(survival)
library(gtsummary)

# LOADING DATASET

db <- 
  readRDS("unos_ped_data.rds") %>% 
  mutate(PSTATUS = factor(PSTATUS, labels = c("Alive", "Dead")),
         CARDARREST_POSTNEURO_DON = ifelse(CARDARREST_POSTNEURO_DON == "N", "No arrest", "Donor arrest"))

#################################################
# TABLE 1: BASELINE RECIPICIENT CHARACTERISTICS #
#################################################

tab1_rec <-
  db %>% 
  select(CARDARREST_POSTNEURO_DON, AGE, GENDER, GENDER_MMTCH, ETHCAT_grp, 
         ETHCAT_MMTCH, WGT_KG_CALC, WEIGHT_MMTCH, HGT_CM_CALC, HGT_MMTCH, 
         BMI_CALC, BMI_MMTCH, DIAG_grp2, STERNOTOMY_TCR, ICU, ABN_CONGEN_DON, 
         VAD_DEVICE_TY_TRR_grp, IMPL_DEFIBRIL, MOST_RCNT_CREAT, INIT_O2, MALIG, 
         FUNC_STAT_TCR, INOTROPES_TRR, ECMO_TRR, ONVENT, HEMO_PA_MN_TRR, HEMO_CO_TRR, 
         ECMO_72HOURS, PRAPK_CL1, PRAPK_CL2, ABO, ABO_MAT, CMV_STATUS, DAYSWAIT_CHRON, 
         DAYS_STAT1A, DAYS_STAT1B, DAYS_STAT2, DISTANCE, ISCHTIME, LOS, PST_DIAL, 
         PST_PACEMAKER, PST_STROKE, PSTATUS, TRTREJ1Y) %>% 
  tbl_summary(
    by = CARDARREST_POSTNEURO_DON,
    label = list(AGE~"Age (years)",
                 GENDER~"Gender",
                 WGT_KG_CALC~"Weight (kg)",
                 BMI_CALC~"BMI",
                 HGT_CM_CALC~"Height (cm)",
                 ETHCAT_grp~"Race",
                 STERNOTOMY_TCR~"Total prior sternotomies",
                 ICU~"Recipient in ICU",
                 ABN_CONGEN_DON~"Heart failure type Congenital",
                 DIAG_grp2~"Recipient primary diagnosis",
                 VAD_DEVICE_TY_TRR_grp~"Presence of VAD at listing",
                 IMPL_DEFIBRIL~"Implantable defibrillator at registration",
                 MOST_RCNT_CREAT~"Creatine",
                 INIT_O2~"O2 Requirement at Listing",
                 MALIG~"Any previous malignacy",
                 FUNC_STAT_TCR~"Recipient functional status at registration",
                 INOTROPES_TRR~"IV inotropes at transplant",
                 ECMO_TRR~"ECMO at transplant",
                 ONVENT~"Mechanical ventilator support",
                 HEMO_PA_MN_TRR~"Hemodynamics at time of transplant: Mean PA (mm/Hg) pressure",
                 HEMO_CO_TRR~"Hemodynamics at time of transplant: CO (L/min)",
                 ECMO_72HOURS~"Recipient ECMO at 72 Hours",
                 PRAPK_CL1~"Peak PRA class I at transplant",
                 PRAPK_CL2~"Peak PRA class II at transplant",
                 ABO~"Recipient blood type",
                 ABO_MAT~"Donor-recipient ABO match level",
                 CMV_STATUS~"Recipient CMV status at transplant",
                 DAYSWAIT_CHRON~"Total days on waitlist",
                 DAYS_STAT1A~"Days in status: Status 1A",
                 DAYS_STAT1B~"Days in status: Status 1B",
                 DAYS_STAT2~"Days in status: Status 2",
                 DISTANCE~"Distance from donor hospital to transplant center (Nautical Miles)",
                 ISCHTIME~"Ischemic time in hours",
                 LOS~"Length of stay post transplant",
                 PST_DIAL~"Events prior to discharge: Dialysis",
                 PST_PACEMAKER~"Events prior to discharge: Pacemaker",
                 PST_STROKE~"Events prior to discharge: Stroke",
                 PSTATUS~"Most Recent Patient Status",
                 TRTREJ1Y~"Treated for rejection within 1 year",
                 GENDER_MMTCH~"Gender mismatch",
                 ETHCAT_MMTCH~"Race mismatch",
                 WEIGHT_MMTCH~"Weight mismatch",
                 HGT_MMTCH~"Height mismatch",
                 BMI_MMTCH~"BMI mismatch"),
    missing_text = "Missing") %>% 
  modify_header(label = "**Variable**", update = all_stat_cols() ~ "**{level}**, N =  {n} ({style_percent(p)}%)") %>%
  add_overall(col_label = "**Overall** (N = {N})") %>% 
  bold_labels() %>% 
  add_p(test = list(all_continuous() ~ "kruskal.test", all_dichotomous() ~ "chisq.test", all_categorical() ~ "chisq.test")) 

###########################################
# TABLE 1: BASELINE DONOR CHARACTERISTICS #
###########################################

tab1_don <-
  db %>% 
  select(CARDARREST_POSTNEURO_DON,
         AGE_DON,
         ETHCAT_DON_grp,
         GENDER_DON,
         BMI_DON_CALC,
         HGT_CM_DON_CALC,
         WGT_KG_DON_CALC,
         INTRACRANIAL_CANCER_DON,
         DEATH_MECH_DON,
         CREAT_DON,
         HIST_CIG_DON,
         ALCOHOL_HEAVY_DON,
         CONTIN_COCAINE_DON,
         AMIS,
         BMIS,
         DRMIS,
         CMV_NUCLEIC_DON,
         CLIN_INFECT_DON,
         ARGININE_DON,
         INOTROP_SUPPORT_DON,
         PT_DIURETICS_DON,
         LV_EJECT,
         POSTERIOR_WALL,
         WALL_ABN_SEG_DON,
         WALL_ABN_GLOB_DON,
         CORONARY_ANGIO,
         ISCHTIME,
         ANTIHYPE_DON,
         CHEST_TRAUMA,
         REGION,
         GSTATUS,
         GTIME) %>% 
  tbl_summary(
    by = CARDARREST_POSTNEURO_DON,
    label = list(AGE_DON~"Age (years)",
                 ETHCAT_DON_grp~"Race",
                 GENDER_DON~"Gender",
                 BMI_DON_CALC~"BMI",
                 HGT_CM_DON_CALC~"Height (cm)",
                 WGT_KG_DON_CALC~"Weight (cm)",
                 INTRACRANIAL_CANCER_DON~"Intracranial cancer",
                 DEATH_MECH_DON~"Mechanism of death",
                 CREAT_DON~"Terminal creatinine",
                 HIST_CIG_DON~"Cigarette use in past at > 20pack years",
                 ALCOHOL_HEAVY_DON~"Heavy Alcohol Use (2+ drinks/day)",
                 CONTIN_COCAINE_DON~"Cocaine use (recent 6 mo use)",
                 AMIS~"HLA mismatch level: A locus",
                 BMIS~"HLA mismatch level: B locus",
                 DRMIS~"HLA mismatch level: DR locus",
                 CMV_NUCLEIC_DON~"CMV by nucleic acid",
                 CLIN_INFECT_DON~"Clinical infection",
                 ARGININE_DON~"Arginine vesopressin within 24h pre cruss clamp",
                 INOTROP_SUPPORT_DON~"Inotropic medications at procurement",
                 PT_DIURETICS_DON~"Diuretics within 24 hrs procurement",
                 LV_EJECT~"LV ejection fraction (%)",
                 POSTERIOR_WALL~"LV posterior wall thickness (cm)",
                 WALL_ABN_SEG_DON~"Segmental wall abnormality",
                 WALL_ABN_GLOB_DON~"Global wall abnormality",
                 CORONARY_ANGIO~"Coronary angiogram",
                 ISCHTIME~"Ischemic time in hours",
                 ANTIHYPE_DON~"Antihypertensives within 24h pre-cross clamp",
                 CHEST_TRAUMA~"Chest Trauma",
                 REGION~"UNOS Region of recovery",
                 GSTATUS ~ "Graft failure",
                 GTIME ~ "Graft lifespan"),
    missing_text = "Missing") %>% 
  modify_header(label = "**Variable**", update = all_stat_cols() ~ "**{level}**, N =  {n} ({style_percent(p)}%)") %>%
  add_overall(col_label = "**Overall** (N = {N})") %>% 
  bold_labels() %>% 
  add_p(test = list(all_continuous() ~ "kruskal.test", all_dichotomous() ~ "chisq.test", all_categorical() ~ "chisq.test")) 

