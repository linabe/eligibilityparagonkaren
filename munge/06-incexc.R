
funcIE(IND_YRS >= 50 | is.na(IND_YRS), 2, inex = "in", pragscenie = TRUE)

## ska MRA vara med diuretia (UP). Nope.
funcIE(diur_any == "yes" | is.na(diur_any), 4, inex = "in")

funcIE(FSI_NYHA %in% c("II", "III", "IV", "") | is.na(FSI_NYHA), 5, pragscenie = TRUE, inex = "in")

funcIE(crit_structural_hd == "yes" | is.na(crit_structural_hd), 6, inex = "in")

funcIE(crit_ntprobnp == "yes" | is.na(crit_ntprobnp), 7, pragscenie = TRUE, inex = "in")

## not HIS_COR since then will include historic MI
funcIE(!cor_cur_any == "yes" | is.na(cor_cur_any), 2)

funcIE(!(ace_any == "yes" & arb_any == "yes") | is.na(ace_any) | is.na(arb_any), 5)

funcIE(!FSI_HEM / 10 < 10 | is.na(FSI_HEM), "8. b")

funcIE(!FSI_BMI > 40 | is.na(FSI_BMI), "8. c")

funcIE(!FSI_SBP >= 180 | is.na(FSI_SBP), "9. a")

funcIE(!crit_SBP_Adrugs == "yes" | is.na(crit_SBP_Adrugs), "9. b")

funcIE(!FSI_SBP < 110 | is.na(FSI_SBP), "9. c", pragscenie = TRUE)

## not HIS_ASSSTROKE since then will include historic stroke
funcIE(stroke_cur_any == "no" | is.na(stroke_cur_any), 16)

# Don't use HIS3_RHYATTACH but below for consitency with SwedeHF
funcIE(!(EKG_VRATE > 110 & EKG_FIB_FLUT == "Yes") | is.na(EKG_VRATE) | is.na(EKG_FIB_FLUT), 18)

funcIE(liver_any == "no" | is.na(liver_any), 23)

funcIE(!MDRD < 30 | is.na(MDRD), "24. a", pragscenie = TRUE)

funcIE(!FSI_K > 5.2 | is.na(FSI_K), "26. a")

funcIE(cancer_any == "no" | is.na(cancer_any), 31)
