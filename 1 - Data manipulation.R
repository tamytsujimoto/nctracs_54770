require(tidyverse)
library(readstata13)
library(gtsummary)

# Loaging datasets

donor = 
  read.dta13("../../1 - From PI/SAS Export to STATA 202009/Deceased Donor/DECEASED_DONOR_DATA.DTA", convert.factors = FALSE) %>% 
  select(DONOR_ID,
         POSTERIOR_WALL,
         CARDARREST_POSTNEURO_DON,
         HR_FINAL_FLUSH_OSTXT,
         CHEST_TRAUMA,
         CARDARREST_DOWNTM,
         CARDARREST_DOWNTM_DURATION,
         CPR_ADMIN,
         CPR_ADMIN_DURATION)

thoracic = 
  read.dta13("../../1 - From PI/SAS Export to STATA 202009/Thoracic/THORACIC_DATA.DTA", convert.factors = FALSE) %>% 
  select(DONOR_ID,
         WL_ID_CODE,
         TRR_ID_CODE,
         ORGAN,
         TX_DATE,
         AGE_GROUP,
         AGE,
         GENDER,
         WGT_KG_CALC,
         BMI_CALC,
         HGT_CM_CALC,
         ETHCAT,
         STERNOTOMY_TCR,
         ICU,
         ABN_CONGEN_DON,
         DIAG,
         DIAG_OSTXT,
         VAD_DEVICE_TY_TRR,
         IMPL_DEFIBRIL,
         MOST_RCNT_CREAT,
         INIT_O2,
         MALIG,
         FUNC_STAT_TCR,
         INOTROPES_TRR,
         ECMO_TRR,
         ONVENT,
         VAD_TAH_TRR,
         HEMO_PA_MN_TRR,
         HEMO_CO_TRR,
         ECMO_72HOURS,
         PRAPK_CL1,
         PRAPK_CL2,
         ABO,
         ABO_MAT,
         CMV_STATUS,
         DAYSWAIT_CHRON,
         DAYS_STAT1A,
         DAYS_STAT1B,
         DAYS_STAT2,
         DISTANCE,
         ISCHTIME,
         LOS,
         PST_DIAL,
         PST_PACEMAKER,
         PST_STROKE,
         PSTATUS,
         PTIME,
         PX_STAT,
         PX_STAT_DATE,
         TRTREJ1Y,
         AGE_GROUP,
         AGE_DON,
         ETHCAT_DON,
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
         WALL_ABN_SEG_DON,
         WALL_ABN_GLOB_DON,
         CORONARY_ANGIO,
         ISCHTIME,
         ANTIHYPE_DON,
         REGION,
         CORONARY_ANGIO,
         GSTATUS,
         GTIME)

# donor = 233354 cases (donors)
# thoracic = 189543 (transplant)

# FILTERS

thoracic %>% 
  mutate(TX_DATE_bin = ifelse(!is.na(TX_DATE), 1, 0)) %>% 
  group_by(AGE_GROUP, TX_DATE_bin, ORGAN) %>% 
  summarise(n=n())


get_label <- function(variable, fmtname, label_data) {
  
  values <- names(table(final_data[variable]))
  
  label_data %>% 
    filter(CODE %in% values, FMTNAME == fmtname) %>% 
    select(LABEL) %>% 
    pull()
  
}

# COMBINING DATASET AND COMPUTING VARIABLES

final_data <-
  thoracic %>% 
  filter(AGE_GROUP == "P", !is.na(TX_DATE), ORGAN == "HR") %>% 
  left_join(donor, by = "DONOR_ID") %>% 
  filter(!is.na(PTIME), !is.na(PSTATUS), CARDARREST_POSTNEURO_DON %in% c("Y", "N")) %>% 
  mutate(ETHCAT = ifelse(ETHCAT == 998, NA, ETHCAT),
         ETHCAT_DON = ifelse(ETHCAT_DON == 998, NA, ETHCAT_DON),
         STERNOTOMY_TCR = ifelse(STERNOTOMY_TCR == 998, NA, STERNOTOMY_TCR),
         VAD_TAH_TRR_grp = ifelse(VAD_TAH_TRR == 999, NA, 
                                  ifelse(VAD_TAH_TRR == 20, 0, 1)),
         VAD_DEVICE_TY_TRR_grp = ifelse(VAD_DEVICE_TY_TRR == 1, 0, 1),
         GENDER_MMTCH = ifelse(GENDER == GENDER_DON, 0, 1),
         WEIGHT_MMTCH = ifelse(abs(WGT_KG_CALC/WGT_KG_DON_CALC - 1) > .2, 1, 0),
         HGT_MMTCH = ifelse(abs(HGT_CM_CALC/HGT_CM_DON_CALC - 1) > .2, 1, 0),
         BMI_MMTCH = ifelse(abs(BMI_CALC/BMI_DON_CALC - 1) > .2, 1, 0),
         ETHCAT_MMTCH = ifelse(ETHCAT == ETHCAT_DON, 0, 1)) %>% 
  mutate_at(vars(ETHCAT,
                 ETHCAT_DON,
                 STERNOTOMY_TCR,
                 DIAG,
                 VAD_DEVICE_TY_TRR,
                 FUNC_STAT_TCR,
                 VAD_TAH_TRR,
                 ABO_MAT,
                 CMV_STATUS,
                 PX_STAT,
                 DEATH_MECH_DON,
                 CMV_NUCLEIC_DON,
                 CORONARY_ANGIO,
                 VAD_TAH_TRR_grp,
                 REGION), ~factor(.)) %>% 
  mutate_at(vars(ICU,
                 ABN_CONGEN_DON,
                 IMPL_DEFIBRIL,
                 MALIG,
                 ONVENT,
                 ECMO_72HOURS,
                 PST_DIAL,
                 PST_PACEMAKER,
                 PST_STROKE,
                 TRTREJ1Y,
                 INTRACRANIAL_CANCER_DON,
                 HIST_CIG_DON,
                 ALCOHOL_HEAVY_DON,
                 CONTIN_COCAINE_DON,
                 CLIN_INFECT_DON,
                 ARGININE_DON,
                 INOTROP_SUPPORT_DON,
                 PT_DIURETICS_DON,
                 WALL_ABN_SEG_DON,
                 WALL_ABN_GLOB_DON,
                 ANTIHYPE_DON,
                 CHEST_TRAUMA),
            ~ifelse(. %in% c("", "U"), NA, .))

tho_label <- read.csv('thoracic_label.csv')

levels(final_data$ETHCAT) = get_label("ETHCAT", "ETHCAT", tho_label)
levels(final_data$ETHCAT_DON) = get_label("ETHCAT_DON", "ETHCAT", tho_label)
levels(final_data$STERNOTOMY_TCR) = get_label("STERNOTOMY_TCR", "STERNOTO", tho_label)
levels(final_data$DIAG) = get_label("DIAG", "TH_DGN", tho_label)
levels(final_data$VAD_DEVICE_TY_TRR) = get_label("VAD_DEVICE_TY_TRR", "VADDEVTY", tho_label)
levels(final_data$FUNC_STAT_TCR) = get_label("FUNC_STAT_TCR", "FUNCSTAT", tho_label)
levels(final_data$VAD_TAH_TRR) = get_label("VAD_TAH_TRR", "VADTAH", tho_label)
levels(final_data$ABO_MAT) = get_label("ABO_MAT", "ABOMAT", tho_label)
levels(final_data$CMV_STATUS) = get_label("CMV_STATUS", "SERSTAT", tho_label)
levels(final_data$PX_STAT) = get_label("PX_STAT", "PXSTAT", tho_label)
levels(final_data$DEATH_MECH_DON) = get_label("DEATH_MECH_DON", "DTHMECH", tho_label)
levels(final_data$CMV_NUCLEIC_DON) = get_label("CMV_NUCLEIC_DON", "SERSTAT", tho_label)
levels(final_data$CORONARY_ANGIO) = get_label("CORONARY_ANGIO", "ANGIO", tho_label)

levels(final_data$VAD_TAH_TRR_grp) = c("No", "Yes")
levels(final_data$VAD_DEVICE_TY_TRR_grp) = c("No", "Yes")

# Treating DIAG and ETHCAT 

final_data <-
  final_data %>% 
  separate(DIAG, sep = ":", into = c("DIAG_grp", NA), remove = FALSE) %>% 
  separate(DIAG_grp, sep = " - ", into = c("DIAG_grp", NA)) %>% 
  mutate(DIAG_grp2 = ifelse(DIAG_grp %in% c("CONGENITAL HEART DEFECT", 
                                            "DILATED MYOPATHY", 
                                            "HEART RE-TX/GF",
                                            "HYPERTROPHIC CARDIOMYOPATHY",
                                            "RESTRICTIVE MYOPATHY"), DIAG_grp, "OTHER"),
         ETHCAT_grp = factor(ifelse(ETHCAT %in% c("White", "Black", "Hispanic", "Asian"), ETHCAT, "Other")),
         ETHCAT_DON_grp = factor(ifelse(ETHCAT_DON %in% c("White", "Black", "Hispanic", "Asian"), ETHCAT_DON, "Other"))
  )

levels(final_data$ETHCAT_grp) <- c("White", "Black", "Hispanic", "Asian", "Other")
levels(final_data$ETHCAT_DON_grp) <- c("White", "Black", "Hispanic", "Asian", "Other")

# EDA: UNIVARIATE ANALYSIS

# final_data %>% 
#   select(-c(DONOR_ID, WL_ID_CODE, TRR_ID_CODE, TX_DATE, PX_STAT_DATE, DIAG_OSTXT, HR_FINAL_FLUSH_OSTXT)) %>% 
#   tbl_summary()

# SAVING DATASET

saveRDS(final_data, "unos_ped_data.rds")

db %>% 
  group_by(PSTATUS, PX_STAT) %>% 
  summarise(n = n())