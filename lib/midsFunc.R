my.glm.mids <- function(formula, family = binomial(link = "logit"), data, ...) {
  call <- match.call()
  if (!is.mids(data)) {
    stop("The data must have class mids")
  }
  analyses <- as.list(1:data$m)
  for (i in 1:data$m) {
    data.i <- mice::complete(data, i)
    analyses[[i]] <- do.call("glm", list(formula = quote(formula), family = quote(family), data = quote(data.i), ...))
  }
  #analyses <- lapply(seq_len(data$m), function(i) glm(formula, 
  #                                                    family = family, data = complete(data, i), ...))
  object <- list(
    call = call, call1 = data$call, nmis = data$nmis,
    analyses = analyses
  )
  oldClass(object) <- c("mira", "glm", "lm")
  return(object)
}