
# Create criteria variables POST imputation -------------------------------

## ntProBNP median calc for non imputed values
ntp <- pdata %>%
  summarise(ntp = list(enframe(quantile(FSI_PROBNP, probs = c(0.5), na.rm = TRUE)))) %>%
  unnest(cols = c(ntp)) %>%
  spread(name, value)

# Convert to Long
long <- mice::complete(imp, action = "long", include = TRUE)

fixdata <- function(indata) {
  utdata <- indata %>%
    mutate(
      crit_structural_hd = case_when(
        is.na(F21) | is.na(F22) | is.na(VG1) | is.na(VG4) | is.na(VOG) ~ NA_character_,
        F21 >= 38 | # or LA diameter =38 mm
          F22 >= 55 | # or LA volume =55 ml
          VG1 >= 11 | # IVS thickness =11 mm
          VG4 >= 11 | # posterior wall thickness =11 mm
          VOG >= 29 ~ "yes", # LAVI =29 mL/m2
        TRUE ~ "no"
      ),
      crit_Adrugs = rowSums(select(., ace_any, arb_any, mra_any, bbl_any, cc_any) == "yes"),
      crit_Adrugs = crit_Adrugs + diur_no,
      crit_SBP_Adrugs = case_when(
        is.na(crit_Adrugs) | is.na(FSI_SBP) ~ NA_character_,
        FSI_SBP > 150 & FSI_SBP < 180 & crit_Adrugs < 3 ~ "yes",
        TRUE ~ "no"
      ),
      ## All patients considered to have been HF hosp within prev 9 months
      crit_ntprobnp = case_when(
        is.na(FSI_PROBNP) | is.na(EKG_FIB_FLUT) ~ NA_character_,
        FSI_PROBNP > 200 & EKG_FIB_FLUT == "No" |
          FSI_PROBNP > 600 & (EKG_FIB_FLUT == "Yes") ~ "yes",
        TRUE ~ "no"
      ),
      raas = case_when(
        is.na(ace_any) | is.na(arb_any) ~ NA_character_,
        ace_any == "yes" | arb_any == "yes" ~ "yes",
        TRUE ~ "no"
      ),
      indexYear_cat = case_when(
        indexYear <= 2005 ~ "2000-2005",
        indexYear <= 2011 ~ "2006-2011",
        indexYear <= 2016 ~ "2012-2016"
      ),
      FSI_NYHA_cat = case_when(
        is.na(FSI_NYHA) ~ NA_character_,
        FSI_NYHA %in% c("I", "II") ~ "I-II",
        FSI_NYHA %in% c("III", "IV") ~ "III-IV"
      ),
      ## anemia
      FSI_HEM_cat = case_when(
        is.na(IND_GEN) | is.na(FSI_HEM) ~ NA_character_,
        IND_GEN == "Female" & FSI_HEM < 120 | IND_GEN == "Male" & FSI_HEM < 130 ~ "<120(f)/130(m)",
        TRUE ~ ">=120(f)/130(m)"
      ),
      map = (FSI_SBP + 2 * FSI_DBP) / 3,
      map_cat = case_when(
        is.na(map) ~ NA_character_,
        map < 90 ~ "<90",
        map >= 90 ~ ">=90"
      ),
      EKG_VRATE_cat = case_when(
        is.na(EKG_VRATE) ~ NA_character_,
        EKG_VRATE < 70 ~ "<70",
        EKG_VRATE >= 70 ~ ">=70"
      ),
      FSI_BMI_cat = case_when(
        is.na(FSI_BMI) ~ NA_character_,
        FSI_BMI < 30 ~ "1.<30",
        FSI_BMI <= 40 ~ "2.30-40",
        FSI_BMI > 40 ~ "3.>40"
      ),
      FSI_BMI_cat2 = case_when(
        is.na(FSI_BMI) ~ NA_character_,
        FSI_BMI < 30 ~ "<30",
        FSI_BMI >= 30 ~ ">=30"
      ),
      MDRD_cat = case_when(
        is.na(MDRD) ~ NA_character_,
        MDRD < 30 ~ "1.<30",
        MDRD < 60 ~ "2.30-59",
        MDRD >= 60 ~ "3.>=60"
      ),
      MDRD_cat2 = case_when(
        is.na(MDRD) ~ NA_character_,
        MDRD < 60 ~ "<60",
        MDRD >= 60 ~ ">=60"
      ),
      FSI_PROBNP_cat = case_when(
        is.na(FSI_PROBNP) ~ NA_character_,
        FSI_PROBNP < ntp$`50%` ~ "<median",
        FSI_PROBNP >= ntp$`50%` ~ ">=median"
      ),
      ef = case_when(
        is.na(VG10) ~ NA_character_,
        VG10 >= 40 & VG10 < 50 ~ "40-49%",
        VG10 >= 50 ~ ">=50%"
      ),
      EFgroup57 = case_when(
        VG10 <= 57 ~ "<=57",
        VG10 > 57 ~ ">57"
      )
    )

  utdata <- utdata %>%
    select(!!!elvars) %>%
    transmute(nomissing = rowSums(is.na(.)) == 0) %>%
    bind_cols(utdata, .) %>%
    mutate_if(is_character, factor)

  return(utdata)
}

pdata <- fixdata(pdata)
impdata <- fixdata(impdata)
long <- fixdata(long)

# Convert back to Mids
imput.short <- as.mids(long)
implogreg <- imput.short
