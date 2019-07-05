
# Create vars PRIOR to imputation (used in imp model) ------------------------

pdata <- pdata %>%
  mutate_if(is_character, list(~ na_if(., ""))) %>%
  mutate_if(is_character, list(~ na_if(., "Unknown"))) %>%
  mutate(
    ## Any meds
    diur_no = rowSums(select(
      ., MED_DIUT1, MED_DIUT2, MED_DIUL1, MED_DIUL2,
      MED_DIUO1, MED_DIUO2
    )), # also calculate no diuretics due to exckl criteria 9b. no other antihyp drugs have > 2 and also sys bp in intervall
    diur_any = diur_no > 0,
    ace_any = rowSums(select(., MED_ACE1, MED_ACE2, MED_ACE3)) > 0,
    arb_any = rowSums(select(., MED_ARB1, MED_ARB2, MED_ARB3)) > 0,
    mra_any = rowSums(select(., MED_DIUP1, MED_DIUP2)) > 0,
    bbl_any = rowSums(select(., MED_BB1, MED_BB2, MED_BB3)) > 0,
    cc_any = rowSums(select(., MED_CCB1, MED_CCB2, MED_CCB3)) > 0,
    statin_any = rowSums(select(., MED_STA1, MED_STA2)) > 0,
    antiplat_any = rowSums(select(., MED_APA1, MED_APA2)) > 0,
    anticoag_any = rowSums(select(., MED_ACO1, MED_ACO2)) > 0,
    nitrate_any = rowSums(select(., MED_NIT1, MED_NIT2)) > 0,

    ## Any Med history
    liver_any = rowSums(select(
      ., HIS_ASSLIV, HIS2_ASSLIV, HIS3_ASSLIV, HIS_ASSLIVNOT,
      HIS2_ASSLIVNOT, HIS3_ASSLIVNOT
    ) == "Yes") > 0,
    cancer_any = rowSums(select(
      .,
      HIS_ASSBREAST, HIS2_ASSBREAST, HIS3_ASSBREAST,
      HIS_ASSCANOTH, HIS2_ASSCANOTH, HIS3_ASSCANOTH,
      HIS_ASSCOL, HIS2_ASSCOL, HIS3_ASSCOL,
      HIS_ASSLUNG, HIS2_ASSLUNG, HIS3_ASSLUNG,
      HIS_ASSPROST, HIS2_ASSPROST, HIS3_ASSPROST
    ) == "Yes") > 0,

    stroke_cur_any = rowSums(select(., HIS2_ASSSTROKE, HIS3_ASSSTROKE) == "Yes") > 0,
    stroke_any = rowSums(select(., HIS_ASSSTROKE, HIS2_ASSSTROKE, HIS3_ASSSTROKE) == "Yes") > 0,
    cor_cur_any = rowSums(select(., HIS2_COR, HIS3_COR) == "Yes") > 0,
    ihd_any = rowSums(select(., HIS_COR, HIS2_COR, HIS3_COR) == "Yes") > 0,

    pci_cabg_any = rowSums(select(
      ., HIS_SURANG, HIS2_SURANG, HIS3_SURANG,
      HIS_SURCABG, HIS2_SURCABG, HIS3_SURCABG
    ) == "Yes") > 0,

    smoking = case_when(
      is.na(HIS_ASSCURSMO) | is.na(HIS2_ASSCURSMO) | is.na(HIS3_ASSCURSMO) ~ NA_character_,
      HIS3_ASSCURSMO == "Yes" ~ "Current",
      (HIS_ASSCURSMO == "Yes" | HIS2_ASSCURSMO == "Yes") & HIS3_ASSCURSMO == "No" ~ "Former",
      HIS_ASSCURSMO == "No" & HIS2_ASSCURSMO == "No" & HIS3_ASSCURSMO == "No" ~ "Never"
    ),

    diabetes1_any = rowSums(select(., HIS_ASSDIAB1, HIS2_ASSDIAB1, HIS3_ASSDIAB1) == "Yes") > 0,
    diabetes2_any = rowSums(select(., HIS_ASSDIAB2, HIS2_ASSDIAB2, HIS3_ASSDIAB2) == "Yes") > 0,

    heartvalvesurgery_any = rowSums(select(
      .,
      HIS_SURAORREPA, HIS2_SURAORREPA, HIS3_SURAORREPA,
      HIS_SURMITREPA, HIS2_SURMITREPA, HIS3_SURMITREPA,
      HIS_SURPULREPA, HIS2_SURPULREPA, HIS3_SURPULREPA,
      HIS_SURTRIREPA, HIS2_SURTRIREPA, HIS3_SURTRIREPA
    ) == "Yes") > 0,

    hypertension_any = rowSums(select(., HIS_ASSHYP, HIS2_ASSHYP, HIS3_ASSHYP) == "Yes") > 0,

    pvd_any = rowSums(select(., HIS_ASSPERVAS, HIS2_ASSPERVAS, HIS3_ASSPERVAS) == "Yes") > 0,

    valvular_any = rowSums(select(., HIS_VA, HIS2_VA, HIS3_VA) == "Yes") > 0,

    copd_any = rowSums(select(., HIS_ASSCOPD, HIS2_ASSCOPD, HIS3_ASSCOPD) == "Yes") > 0,

    EKG_FIB_FLUT = case_when(
      is.na(EKG_RHYATFIB) | is.na(EKG_RHYATFLUT) ~ NA_character_,
      EKG_RHYATFIB == "Yes" | EKG_RHYATFLUT == "Yes" ~ "Yes",
      TRUE ~ "No"
    )
  )

pdata <- pdata %>%
  mutate(
    # eGFR according to MDRD
    crea = ifelse(is.na(FSI_CREAT_MG), FSI_CREAT_MM / 88.4, FSI_CREAT_MG),
    sex = recode(IND_GEN, "Male" = 1, "Female" = 0),
    ethnicity = recode(IND_ETHNIC, "Caucasion" = 0, "Other" = 0, "Afro-American" = 1),
    MDRD = nephro::MDRD4(crea, sex, IND_YRS, ethnicity),
    MDRD_cat = case_when(
      is.na(MDRD) ~ NA_character_,
      MDRD < 30 ~ "1.<30",
      MDRD < 60 ~ "2.30-<60",
      MDRD >= 60 ~ "3.>=60"
    ),
    logFSI_PROBNP = log10(FSI_PROBNP),
    ## noncv death
    time = CENSDATE6 - dmy(FSI_PREDAT),
    censtime = ymd("2012-09-30") - dmy(FSI_PREDAT),
    nonCVDeath3y = case_when(
      time < 3 * 365.25 & OUTCOME6 == 0 ~ NA_character_,
      censtime < 3 * 365.25 & OUTCOME6 %in% c(1, 2) ~ NA_character_,
      OUTCOME6 == 2 & TIME6 <= 365.25 * 3 ~ "yes",
      OUTCOME5 == 1 & OUTCOME6 == 1 & TIME6 <= 365.25 * 3 ~ "yes", # unknown cause of death is assumed to be non CV
      TRUE ~ "no"
    ),
    ef = case_when(
      is.na(VG10) ~ NA_character_,
      VG10 >= 40 & VG10 < 50 ~ "40-49%",
      VG10 >= 50 ~ ">=50%"
    ),
    FSI_BMI_cat = case_when(
      is.na(FSI_BMI) ~ NA_character_,
      FSI_BMI <= 30 ~ "1.<=30",
      FSI_BMI <= 40 ~ "2.>30-40",
      FSI_BMI > 40 ~ "3.>40"
    ),
    indexYear = year(dmy(FSI_PREDAT))
  ) %>%
  select(-FSI_PROBNP)
