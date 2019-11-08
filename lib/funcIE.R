funcIE <- function(ie, ienr, iedesc, inex = "ex", noie = FALSE, pragscenie = FALSE) {
  ieprefix <- paste0("ie_")

  if (noie) {
    ieprefix <- paste0(ieprefix, "no_")
  }
  if (pragscenie) {
    ieprefix <- paste0(ieprefix, inex, "_pragscen_")
  }

  iename <- paste0(ieprefix, inex, sub(". ", "", ienr))

  impdata <<- impdata %>%
    mutate(
      !!iename := !!enexpr(ie)
    ) # global
  pdata <<- pdata %>%
    mutate(
      !!iename := !!enexpr(ie)
    ) # global
}


funcSumIE <- function(indata, groupvar) {
  groupvar <- enexpr(groupvar)
  
  indata %>%
    filter(!is.na(UQ(rlang::sym(groupvar)))) %>%
    group_by(UQ(rlang::sym(groupvar))) %>%
    count() %>%
    ungroup() %>%
    tidyr::complete(UQ(rlang::sym(groupvar)) := c(TRUE, FALSE), fill = list(n = 0)) %>%
    mutate(
      freq = round((n / sum(n)) * 100, 1),
      out = paste0(n, " (", freq, "%)")
    ) %>%
    filter(UQ(rlang::sym(groupvar))) %>%
    select(out)
}

funcIETab <- function(ievar, iedesc, impdata_tmp, pdata_tmp) {
  flowtab_tmp <- unlist(c(
    iedesc,
    funcSumIE(impdata_tmp, groupvar = !!ievar),
    funcSumIE(pdata_tmp %>% filter(nomissing), groupvar = !!ievar),
    funcSumIE(pdata_tmp, groupvar = !!ievar)
  ))
  return(flowtab_tmp)
}


funcSumIEAll <- function(iesumname, ievars, impdata_tmp, pdata_tmp, filtvar = NULL) {
  if (!is.null(filtvar)) {
    impdata_tmp <- impdata_tmp %>%
      filter(!is.na(UQ(rlang::sym(filtvar))))
    pdata_tmp <- pdata_tmp %>%
      filter(!is.na(UQ(rlang::sym(filtvar))))
  }
  
  tempflowtab <- unlist(c(
    iesumname,
    funcSumIE(impdata_tmp %>%
                mutate(sumanyie = rowSums(select(., matches(ievars)) == FALSE) == 0),
              groupvar = "sumanyie"
    ),
    funcSumIE(pdata_tmp %>%
                mutate(sumanyie = rowSums(select(., matches(ievars)) == FALSE) == 0) %>%
                filter(nomissing),
              groupvar = "sumanyie"
    ),
    funcSumIE(pdata_tmp %>%
                mutate(sumanyie = rowSums(select(., matches(ievars)) == FALSE) == 0),
              groupvar = "sumanyie"
    )
  ))
  return(tempflowtab)
}

funcIEfake <- function(iedesc) {
  flowtab_tmp <- c(iedesc, rep(NA, 3))
  return(flowtab_tmp)
}
