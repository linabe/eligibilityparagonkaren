
# Create criteria variables POST imputation -------------------------------

fixdata <- function(indata) {
  utdata <- indata %>%
    mutate(
      FSI_PROBNP = 10^(logFSI_PROBNP),
      crit_structural_hd = case_when(
        is.na(F21) | is.na(F22) | is.na(VG1) | is.na(VG4) | is.na(VOG) ~ NA_character_,
        F21 >= 38 | # or LA diameter =38 mm
          F22 >= 55 | # or LA volume =55 ml
          VG1 >= 11 | # IVS thickness =11 mm
          VG4 >= 11 | # posterior wall thickness =11 mm
          VOG >= 29 ~ "yes", # LAVI =29 mL/m2
        TRUE ~ "no"
      ),
      crit_Adrugs = rowSums(select(., diur_no, ace_any, arb_any, mra_any, bbl_any, cc_any)),
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
        ace_any == TRUE | arb_any == TRUE ~ "yes",
        TRUE ~ "no"
      ),
      indexYear_cat = case_when(
        indexYear <= 2005 ~ "2000-2005",
        indexYear <= 2011 ~ "2006-2011",
        indexYear <= 2016 ~ "2012-2016"
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
