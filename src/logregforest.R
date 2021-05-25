logregFuncplot <- function(filt, figname, out, xlab, mi = NA, ma = NA, ticks = c(0.5, 1, 1.5)) {
  
  kontunits <- data.frame(
    vars = c("IND_YRS"), # "hb", "GFR", "ntProBNP"),
    units = c(10) # , 20, 20, 100)
  )

  cmodadj <- my.glm.mids(formula(paste0(out, " ~ ", paste(adjmodvars, collapse = " + "))),
    family = binomial(link = "logit"), data = implogreg, subset = filt
  )

  scmodadj <- summary(pool(cmodadj))

  nval <- length(scmodadj$estimate)

  padj <- dF(scmodadj$p.value[2:nval], dig = 3)
  padj <- replace(padj, padj == "0.000", "<0.001")

  units <- left_join(data.frame(vars = rownames(scmodadj)[2:nval]), kontunits, by = "vars") %>%
    mutate(units = replace_na(units, 1))

  ortabadj <- tibble(var = rownames(scmodadj)[2:nval], 
                     orci = paste0(dF(exp(scmodadj$estimate[2:nval] * units$units), dig = 2),
    " (", dF(exp((scmodadj$estimate[2:nval] - z05 * scmodadj$std.error[2:nval]) * units$units), dig = 2),
    "-", dF(exp((scmodadj$estimate[2:nval] + z05 * scmodadj$std.error[2:nval]) * units$units), dig = 2), ")"),
    p = padj, 
    logor = scmodadj$estimate[2:nval] * units$units,
    loglci = (scmodadj$estimate[2:nval] - z05 * scmodadj$std.error[2:nval]) * units$units,
    loguci = (scmodadj$estimate[2:nval] + z05 * scmodadj$std.error[2:nval]) * units$units
  )

  ortabadj <- ortabadj %>%
    mutate(varsforest = case_when(
      var == "IND_GENMale" ~ "Male sex",
      var == "IND_YRS" ~ "Age, per unit of 10 years",
      var == "FSI_NYHA_catIII-IV" ~ "NYHA class III-IV",
      var == "MDRD_cat2>=60" ~ "Mean arterial pressure >=90",
      var == "FSI_PROBNP_cat>=median" ~ "NT-proBNP >=1409 pg/ml (median)",
      var == "diabetes_anyyes" ~ "Diabetes",
      var == "hypertension_anyyes" ~ "Hypertension",
      var == "ihd_anyyes" ~ "Ischemic heart disease",
      var == "EKG_FIB_FLUTYes" ~ "Atrial fibrillation or flutter on ECG",
      var == "raasyes" ~ "ACEi/ARB",
      var == "mra_anyyes" ~ "MRA",
      var == "bbl_anyyes" ~ "Betablocker"
    )) %>% arrange(desc(row_number()))
  
  if (is.na(mi)) mi <- round(exp(min(ortabadj$loglci)), 1)
  if (is.na(ma)) ma <- round(exp(max(ortabadj$loguci)), 1)

  cextext <- 0.9
  
  # c(bottom, left, top, right)
  x11()
  par(mar = c(4, 21, 1, 4) + 0.2)
  plot(ortabadj$logor, 1:nrow(ortabadj),
    xlab = "",
    xlim = c(
      log(mi),
      log(ma)
    ),
    ylim = c(1, nrow(ortabadj) + 1),
    axes = FALSE,
    ylab = NA,
    cex.lab = 1.1,
    main = NA,
    cex = 1.2,
    type = "p",
    pch = 22,
    bg = kicols[1],
    col = kicols[1]
  )

  for (i in 1:nrow(ortabadj)) {
    matplot(c(ortabadj$loglci[i], ortabadj$loguci[i]), c(i, i),
      type = "l", add = TRUE, col = kicols[1], cex = 2
    )
  }

  matplot(c(0, 0), c(-1, nrow(ortabadj) + 0.5), type = "l", lty = 3, add = TRUE, col = "black")

  #ticksall <- c(seq(mi, 1, 0.1), seq(1.5, ma, 0.5))
  ticksall <- c(seq(mi, ma, 0.1))
  ticksallpresent <- ticksall
  ticksallpresent[!ticksall %in% ticks] <- NA
    axis(1,
    cex.axis = cextext, at = log(ticksall),
    labels = ticksallpresent
  )

  axis(2,
    at = 1:(nrow(ortabadj) + 1),
    labels = c(ortabadj$varsforest, "Variable"),
    cex.axis = cextext, tick = FALSE, las = 2, line = 20, hadj = 0
  )

  axis(2,
    at = 1:(nrow(ortabadj) + 1),
    labels = c(ortabadj$orci, "OR (95% CI)"),
    cex.axis = cextext, tick = FALSE, las = 2, line = 5, hadj = 0.5
  )

  axis(2,
    at = 1:(nrow(ortabadj) + 1),
    labels = c(ortabadj$p, "P-value"),
    cex.axis = cextext, tick = FALSE, las = 2, line = 0, hadj = 0.5
  )

  axis(1,
    at = 0.05, cex.axis = cextext,
    labels = paste0("Less likely - ", xlab, " - More likely"), line = 1, tick = FALSE
  )
  savePlot(paste0("./output/figs/", figname, ".tiff"), type = "jpg")
}

# Overall
logregFuncplot(filt = quote(!is.na(IND_YRS)), figname = "forestoref57", out = "EFgroup57 == '<=57'", xlab = "EF<=57%", 
               ticks = c(0.2, 0.5, 1, 2, 3, 4), ma = 4.5)



# Overall 54/52
logregFuncplot(quote(!is.na(IND_YRS)), figname = "forestef5452", out = "EFgroupfda == '<54(female)/52(male)'", xlab = "EF<54/52%", 
               ticks = c(0.1, 0.4, 1, 4, 7), 
               mi = 0.1, ma = 7)


# Overall 64/62
logregFuncplot(quote(!is.na(IND_YRS)), figname = "forestef6462", out = "EFgroup6462 == '<64(female)/62(male)'", xlab = "EF<64/62%", 
               ticks = c(0.3, 0.5, 1, 2, 3), 
               mi = 0.3, ma = 3)

