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


funcIE <- function(ie, ienr, iedesc, inex = "ex", cvdeathie = FALSE, noie = FALSE, pragscenie = FALSE) {
  iedesc <- paste0(ienr, ". ", iedesc)
  ieprefix <- paste0("ie_")

  if (noie) {
    iedesc <- paste0(iedesc, footnote_marker_symbol(1))
    ieprefix <- paste0(ieprefix, "no_")
  }
  if (cvdeathie) {
    iedesc <- paste0(iedesc, footnote_marker_symbol(1))
    iedesc <- paste0(iedesc, footnote_marker_symbol(2))
    ieprefix <- paste0(ieprefix, "cvdeath_")
  }
  if (pragscenie) {
    iedesc <- paste0(iedesc, footnote_marker_symbol(3))
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

  flowtab_tmp <- unlist(c(
    iedesc,
    funcSumIE(impdata, !!iename),
    funcSumIE(pdata %>% filter(nomissing), !!iename),
    funcSumIE(pdata, !!iename)
  ))

  if (exists("flowtab")) {
    flowtab <<- rbind(flowtab, flowtab_tmp) # global variable, writes to global env
  } else {
    flowtab <<- flowtab_tmp # global variable, writes to global env
  }
}

funcSumIEAll <- function(iesumname, ievars, filtvar = NULL) {
  if (!is.null(filtvar)) {
    impdata <- impdata %>%
      filter(!is.na(UQ(rlang::sym(filtvar))))
    pdata <- pdata %>%
      filter(!is.na(UQ(rlang::sym(filtvar))))
  }

  tempflowtab <- unlist(c(
    iesumname,
    funcSumIE(impdata %>%
      mutate(sumanyie = rowSums(select(., matches(ievars)) == FALSE) == 0),
    groupvar = "sumanyie"
    ),
    funcSumIE(pdata %>%
      mutate(sumanyie = rowSums(select(., matches(ievars)) == FALSE) == 0) %>%
      filter(nomissing),
    groupvar = "sumanyie"
    ),
    funcSumIE(pdata %>%
      mutate(sumanyie = rowSums(select(., matches(ievars)) == FALSE) == 0),
    groupvar = "sumanyie"
    )
  ))

  flowtab <<- rbind(flowtab, tempflowtab) # global variable
}

funcIEfake <- function(iedesc) {
  flowtab_tmp <- c(iedesc, rep(NA, 4))

  if (exists("flowtab")) {
    flowtab <<- rbind(flowtab, flowtab_tmp) # global variable, writes to global env
  } else {
    flowtab <<- flowtab_tmp # global variable, writes to global env
  }
}
