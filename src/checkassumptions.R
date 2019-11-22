

# Check assumptions for model (not run in report) -------------------------

dataass <- mice::complete(implogreg, 8)

# Logistic regression -----------------------------------------------------
modlm <- glm(formula(paste0("EFgroup57 == '<=57' ~ ", paste(adjmodvars, collapse = " + "))),
  family = binomial(link = "logit"), data = dataass
)


# Linearity for continous variables ---------------------------------------

probabilities <- predict(modlm, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

contdata <- dataass %>%
  select(IND_YRS)

# Bind the logit and tidying the data for plot
contdata <- contdata %>%
  mutate(logit = log(probabilities / (1 - probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(contdata, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~predictors, scales = "free_y")


# Outliers ---------------------------------------------------------------

plot(modlm, which = 4, id.n = 3)


# Multicollinearity -------------------------------------------------------

car::vif(modlm)
