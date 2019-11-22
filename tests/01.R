
context("Check tabvars")

expect_false(any(is.na(impdata[, elvars])), "all eligibilty vars seem not to be imputed")

expect_true(all(rownames(imp$predictorMatrix)[!rownames(imp$predictorMatrix) %in% c(noimpvars, "indexYear", "arb_any", "ace_any")] %in% tabvars), "all imputed vars are presented in table")

expect_true(all(tabvars %in% names(pdata)), "all tab vars are in data")

expect_true(all(elvars[!elvars %in% c("arb_any", "ace_any")] %in% tabvars), "all elvars are not in table")

context("Check modvars")

imptest <- mice::complete(implogreg)
expect_false(any(is.na(imptest[, modvars])), "all mod vars seem not to be imputed")

expect_true(imp$m == 10, "Not 10 imputations")

expect_true(all(adjmodvars %in% modvars), "adj variables in model are not subset of crude variables")

imptest1 <- mice::complete(imp, 1)
imptest2 <- mice::complete(imp, 2)
expect_true(any(imptest1 != imptest2), "diff imputations")
