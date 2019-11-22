

# Impute missing values ---------------------------------------------------

ini <- mice(pdata, maxit = 0, print = F)
pred <- ini$pred

## Variables that should not be used in the imputation model
noimpvars <- c("PatID", "diur_no", "FSI_PREDAT", "IND_ETHNIC")

# IND_ETHNIC not imputed since to few values other than Caucasion

pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0

pred[, c("cor_cur_any", "stroke_cur_any")] <- 0 # cor_cur_any is subgroup to ihd_any and is therefore not used to predict other variables, only to be predicted in itself, same as stroke_cur_any
pred[c("cor_cur_any", "stroke_cur_any"), ] <- 0

## change method used in impuation to prop odds model and none
meth <- ini$method
meth[c("smoking", "FSI_NYHA")] <- "polr"
meth[noimpvars] <- "" # number of diuretics not imputed but diuretics any imputed

imp <- mice(pdata,
  m = 10,
  maxit = 10,
  seed = 45345,
  method = meth,
  predictorMatrix = pred,
  print = FALSE
)

# Create dataset for use in eligibility -----------------------------------

impdata <- imp$data
impvars <- names(imp$nmis[imp$nmis > 0])

# imp mode (categorical variables) and median (continous variables) of 10 imputed datasets
modefuncmatrix <- function(var) {
  names(var == max(var))[(var == max(var))][1]
}

modefunclist <- function(var) {
  names(var == max(var))[1]
}

for (i in seq_along(impvars)) {
  cl <- class(pdata[[impvars[i]]])
  if (cl %in% c("numeric", "integer")) {
    meds <- apply(imp$imp[[impvars[i]]], 1, median)
    impdata[[impvars[i]]][as.numeric(names(meds))] <- meds
  }
  if (cl %in% c("logical", "factor", "character")) {
    mod <- apply(imp$imp[[impvars[i]]], 1, table)
    if (class(mod) == "matrix") {
      mod2 <- apply(mod, 2, modefuncmatrix)
      impdata[[impvars[i]]][as.numeric(names(mod2))] <- factor(mod2)
    }
    if (class(mod) == "list") {
      mod2 <- plyr::ldply(mod, modefunclist)
      impdata[[impvars[i]]][as.numeric(mod2$.id)] <- factor(mod2$V1)
    }
    if (!class(mod) %in% c("matrix", "list")) stop("factor vars are hellish")
  }
  if (is.null(cl) | !cl %in% c("numeric", "integer", "logical", "factor", "character")) {
    stop(paste0("class does not exist for ", impvars[i]))
  }
}

## vars not in imputation model but needed for eligibilty criteria evaluation
impdata <- impdata %>%
  mutate(diur_no = case_when(
    !is.na(diur_no) ~ diur_no,
    diur_any == "yes" ~ 1,
    diur_any == "no" ~ 0
  )) # impute any diuretics, but need no for eligibilty
