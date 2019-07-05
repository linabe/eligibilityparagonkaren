

# Impute missing values ---------------------------------------------------

set.seed(45345)

impvars <- pdata %>%
  mutate_if(is_character, factor) %>%
  mutate_at(vars(smoking, FSI_NYHA, FSI_BMI_cat, MDRD_cat), as.ordered) %>%
  select(
    IND_YRS, IND_GEN,
    indexYear,
    ef,
    FSI_NYHA,
    EKG_FIB_FLUT,
    F21, F22, VG1, VG4, VOG,
    FSI_SBP,
    EKG_VRATE,
    MDRD_cat,
    FSI_HEM,
    logFSI_PROBNP,
    FSI_K,
    ace_any, arb_any,
    mra_any,
    diur_any,
    nitrate_any,
    bbl_any,
    cc_any,
    smoking,
    FSI_BMI_cat,
    hypertension_any,
    diabetes1_any,
    diabetes2_any,
    ihd_any,
    cor_cur_any,
    pvd_any,
    stroke_cur_any,
    valvular_any,
    copd_any,
    liver_any,
    cancer_any
  )


imp <- mice(impvars, m = 1)

impdata <- mice::complete(imp, 1)

# IND_ETHNIC not imputed since to few values other than Caucasion

## vars not in imputaion model but needed for eligibilty criteria evaluation
impdata <- cbind(impdata, pdata %>% select(nonCVDeath3y, PatID, diur_no)) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(diur_no = case_when(
    !is.na(diur_no) ~ diur_no,
    is.na(diur_no) & diur_any ~ 1,
    is.na(diur_no) & !diur_any ~ 0
  )) # impute any diuretics, but need no for eligibilty
