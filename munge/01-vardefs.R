
# Names of variables used in analysis -------------------------------------

elvars <- c(
    "IND_YRS", "diur_any",
    "FSI_NYHA",
    "F21", "F22", "VG1", "VG4", "VOG",
    "FSI_PROBNP",
    "EKG_FIB_FLUT",
    "cor_cur_any",
    "ace_any", "arb_any",
    "FSI_HEM", "FSI_BMI_cat",
    "FSI_SBP", "mra_any", "bbl_any", "cc_any",
    "stroke_cur_any",
    "EKG_VRATE",
    "MDRD_cat",
    "FSI_K",
    "cancer_any",
    "liver_any"
)

tabvars <- c(
    "IND_GEN",
    "IND_YRS",
    "IND_ETHNIC",
    "indexYear_cat",
    "ef",
    "FSI_NYHA",
    "EKG_FIB_FLUT",
    "F21", "F22", "VG1", "VG4", "VOG",
    "FSI_SBP", "FSI_DBP",
    "EKG_VRATE",
    "MDRD_cat",
    "FSI_HEM",
    "FSI_PROBNP",
    "FSI_K",
    "raas",
    "mra_any",
    "diur_any",
    "nitrate_any",
    "antiplat_any",
    "anticoag_any",
    "statin_any",
    "bbl_any",
    "cc_any",
    "smoking",
    "FSI_BMI_cat",
    "hypertension_any",
    "diabetes_any",
    "ihd_any",
    "pci_cabg_any",
    "cor_cur_any",
    "pvd_any",
    "stroke_any",
    "stroke_cur_any",
    "valvular_any",
    "heartvalvesurgery_any",
    "copd_any",
    "liver_any",
    "cancer_any"
)


modvars <- tabvars
# 
# c(
#     "IND_GEN", - Gender
#     "IND_YRS", - Age
#     "IND_ETHNIC",
#     "indexYear_cat",
#     "ef",
#     "FSI_NYHA", - NYHA (III-IV vs. I-II)
#     "EKG_FIB_FLUT", - atrial fibrillation
#     "F21", "F22", "VG1", "VG4", "VOG",
#     "FSI_SBP", "FSI_DBP",- Mean arterial pressure (>=90 mmHg vs <)
#     "EKG_VRATE", - heart rate (>=70 vs <70)
#     "MDRD_cat", - eGFR>=60 vs <60
#     "FSI_HEM", - anemia
#     "FSI_PROBNP",- NTproBNP (above vs below median)
#     "FSI_K",
#     "raas", - RASi
#     "mra_any", - MRA
#     "diur_any", 
#     "nitrate_any",
#     "antiplat_any", - Antiplatelet
#     "anticoag_any", - anticoagulants
#     "statin_any", - statins
#     "bbl_any", - Beta blockers
#     "cc_any",
#     "smoking",
#     "FSI_BMI_cat", - BMI >=30 vs less
#     "hypertension_any",- hypertension
#     "diabetes_any", - diabetes mellitus
#     "ihd_any",- ischemic heart disease (history of IHD or previous revise)
#     "pci_cabg_any",
#     "cor_cur_any",
#     "pvd_any",
#     "stroke_any", - any stroke
#     "stroke_cur_any",
#     "valvular_any", - any valvular disease
#     "heartvalvesurgery_any",
#     "copd_any", - any lung disease
#     "liver_any",- liver disease.
#     "cancer_any"- any cancer
# )
# 
# 
# 
# - syncope
# - peripheral oedema
# - rales
# -increased jugular venous pressure
# - dyspnea (if not encountered in all the patients”
#            