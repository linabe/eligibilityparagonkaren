
# Create vars PRIOR to imputation (used in imp model) ------------------------

comFunc <- function(var) {
  var <- case_when(
    is.na(var) ~ NA_character_,
    var ~ "yes",
    !var ~ "no"
  )
  return(var)
}

pdata <- pdata %>%
  mutate_if(is_character, list(~ na_if(., ""))) %>%
  mutate_if(is_character, list(~ na_if(., "Unknown"))) %>%
  mutate(
    # eGFR according to MDRD
    crea = ifelse(is.na(FSI_CREAT_MG), FSI_CREAT_MM / 88.4, FSI_CREAT_MG),
    sex = recode(IND_GEN, "Male" = 1, "Female" = 0),
    ethnicity = recode(IND_ETHNIC, "Caucasion" = 0, "Other" = 0, "Afro-American" = 1),
    MDRD = nephro::MDRD4(crea, sex, IND_YRS, ethnicity),
    indexYear = year(dmy(FSI_PREDAT)),

    ## Any meds
    diur_no = rowSums(select(
      ., MED_DIUT1, MED_DIUT2, MED_DIUL1, MED_DIUL2,
      MED_DIUO1, MED_DIUO2
    )), # also calculate no diuretics due to exckl criteria 9b. no other antihyp drugs have > 2 and also sys bp in intervall
    diur_any = comFunc(diur_no > 0),
    ace_any = comFunc(rowSums(select(., MED_ACE1, MED_ACE2, MED_ACE3)) > 0),
    arb_any = comFunc(rowSums(select(., MED_ARB1, MED_ARB2, MED_ARB3)) > 0),
    mra_any = comFunc(rowSums(select(., MED_DIUP1, MED_DIUP2)) > 0),
    bbl_any = comFunc(rowSums(select(., MED_BB1, MED_BB2, MED_BB3)) > 0),
    cc_any = comFunc(rowSums(select(., MED_CCB1, MED_CCB2, MED_CCB3)) > 0),
    statin_any = comFunc(rowSums(select(., MED_STA1, MED_STA2)) > 0),
    antiplat_any = comFunc(rowSums(select(., MED_APA1, MED_APA2)) > 0),
    anticoag_any = comFunc(rowSums(select(., MED_ACO1, MED_ACO2)) > 0),
    nitrate_any = comFunc(rowSums(select(., MED_NIT1, MED_NIT2)) > 0),

    ## Any Med history
    liver_any = comFunc(rowSums(select(
      ., HIS_ASSLIV, HIS2_ASSLIV, HIS3_ASSLIV, HIS_ASSLIVNOT,
      HIS2_ASSLIVNOT, HIS3_ASSLIVNOT
    ) == "Yes") > 0),
    cancer_any = comFunc(rowSums(select(
      .,
      HIS_ASSBREAST, HIS2_ASSBREAST, HIS3_ASSBREAST,
      HIS_ASSCANOTH, HIS2_ASSCANOTH, HIS3_ASSCANOTH,
      HIS_ASSCOL, HIS2_ASSCOL, HIS3_ASSCOL,
      HIS_ASSLUNG, HIS2_ASSLUNG, HIS3_ASSLUNG,
      HIS_ASSPROST, HIS2_ASSPROST, HIS3_ASSPROST
    ) == "Yes") > 0),

    stroke_cur_any = comFunc(rowSums(select(., HIS2_ASSSTROKE, HIS3_ASSSTROKE) == "Yes") > 0),
    stroke_any = comFunc(rowSums(select(., HIS_ASSSTROKE, HIS2_ASSSTROKE, HIS3_ASSSTROKE) == "Yes") > 0),
    cor_cur_any = comFunc(rowSums(select(., HIS2_COR, HIS3_COR) == "Yes") > 0),
    ihd_any = comFunc(rowSums(select(., HIS_COR, HIS2_COR, HIS3_COR) == "Yes") > 0),
    syncope_any = comFunc(rowSums(select(., HIS_SYN, HIS2_SYN, HIS3_SYN) == "Yes") > 0),

    pci_cabg_any = comFunc(rowSums(select(
      ., HIS_SURANG, HIS2_SURANG, HIS3_SURANG,
      HIS_SURCABG, HIS2_SURCABG, HIS3_SURCABG
    ) == "Yes") > 0),

    smoking = case_when(
      is.na(HIS3_ASSCURSMO) & is.na(HIS2_ASSCURSMO) & is.na(HIS_ASSCURSMO) &
        is.na(HIS3_ASSPRESMO) & is.na(HIS2_ASSPRESMO) & is.na(HIS_ASSPRESMO) ~ NA_character_,
      HIS3_ASSCURSMO == "Yes" | HIS2_ASSCURSMO == "Yes" | HIS_ASSCURSMO == "Yes" |
        HIS3_ASSPRESMO == "Yes" | HIS2_ASSPRESMO == "Yes" | HIS_ASSPRESMO == "Yes" ~ "Ever",
      TRUE ~ "Never"
    ),

    diabetes_any = comFunc(rowSums(select(., HIS_ASSDIAB1, HIS2_ASSDIAB1, HIS3_ASSDIAB1, HIS_ASSDIAB2, HIS2_ASSDIAB2, HIS3_ASSDIAB2) == "Yes") > 0),

    heartvalvesurgery_any = comFunc(rowSums(select(
      .,
      HIS_SURAORREPA, HIS2_SURAORREPA, HIS3_SURAORREPA,
      HIS_SURMITREPA, HIS2_SURMITREPA, HIS3_SURMITREPA,
      HIS_SURPULREPA, HIS2_SURPULREPA, HIS3_SURPULREPA,
      HIS_SURTRIREPA, HIS2_SURTRIREPA, HIS3_SURTRIREPA
    ) == "Yes") > 0),

    hypertension_any = comFunc(rowSums(select(., HIS_ASSHYP, HIS2_ASSHYP, HIS3_ASSHYP) == "Yes") > 0),

    pvd_any = comFunc(rowSums(select(., HIS_ASSPERVAS, HIS2_ASSPERVAS, HIS3_ASSPERVAS) == "Yes") > 0),

    valvular_any = comFunc(rowSums(select(., HIS_VA, HIS2_VA, HIS3_VA) == "Yes") > 0),

    lung_any = comFunc(rowSums(select(
      ., HIS_ASSCOPD, HIS2_ASSCOPD, HIS3_ASSCOPD,
      HIS_ASSPULOTH, HIS2_ASSPULOTH, HIS3_ASSPULOTH,
      HIS_ASSASTHMA, HIS2_ASSASTHMA, HIS3_ASSASTHMA
    ) == "Yes") > 0),

    EKG_FIB_FLUT = case_when(
      is.na(EKG_RHYATFIB) | is.na(EKG_RHYATFLUT) ~ NA_character_,
      EKG_RHYATFIB == "Yes" | EKG_RHYATFLUT == "Yes" ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  select(
    PatID,
    FSI_PREDAT,
    IND_YRS,
    IND_GEN,
    IND_ETHNIC,
    indexYear,
    VG10,
    FSI_NYHA,
    EKG_FIB_FLUT,
    F21, F22, VG1, VG4, VOG,
    FSI_SBP,
    FSI_DBP,
    EKG_VRATE,
    MDRD,
    FSI_HEM,
    FSI_PROBNP,
    FSI_K,
    ace_any,
    arb_any,
    mra_any,
    diur_any,
    diur_no,
    nitrate_any,
    antiplat_any,
    anticoag_any,
    bbl_any,
    cc_any,
    statin_any,
    smoking,
    FSI_BMI,
    hypertension_any,
    diabetes_any,
    ihd_any,
    cor_cur_any,
    pvd_any,
    stroke_any,
    stroke_cur_any,
    valvular_any,
    pci_cabg_any,
    heartvalvesurgery_any,
    lung_any,
    liver_any,
    cancer_any,
    syncope_any,
    FSI_MIN1,
    FSI_MAJ7,
    FSI_MAJ3,
    FSI_MIN3
  ) %>%
  mutate_if(is_character, factor)
