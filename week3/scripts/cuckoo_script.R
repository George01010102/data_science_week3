library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
install.packages("performance")
library(performance)
library(see)
library(emmeans)
cuckoo <- read_csv(here("data_science_week3","week3","data","cuckoo.csv"))

ggplot(cuckoo, aes(x = Mass, y = Beg, colour = Species)) + 
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)", 
       y = "Begging calls per 6 seconds") +
  theme_minimal()


#makes linear model of cuckoo data
cuckoo_lm <- lm(Beg ~ Mass * Species, data = cuckoo)

summary(cuckoo_lm)


#examine diagnostics
check_model(cuckoo_lm, detrend = FALSE)


# Generate predictions
predictions_lm <- emmeans(cuckoo_lm, 
                          specs = ~ Mass + Species,
                          at = list(Mass = seq(0, 40, by = 5))) |>
  as_tibble()

# Check for negative predictions
predictions_lm |> 
  filter(emmean < 0)

performance::check_model(cuckoo_lm, 
                         detrend = FALSE)

#fitting a poisson GLM
#additive model
cuckoo_glm_add <- glm(Beg ~ Mass + Species, 
                      data = cuckoo, 
                      family = poisson(link = "log"))

summary(cuckoo_glm_add)


#generate predictions on a response scale
predictions_poisson <- emmeans(cuckoo_glm_add,
                               specs = ~ Mass + Species,
                               at = list(Mass = seq(0, 40, by = 5)),
                               type = "response") |>
  as_tibble()

ggplot(predictions_poisson, aes(x = Mass, y = rate, colour = Species)) +
  geom_line(linewidth = 1) +
  geom_point(data = cuckoo, aes(y = Beg), alpha = 0.5) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)", 
       y = "Begging calls per 6 seconds",
       title = "Poisson GLM: Additive model") +
  theme_minimal()


#examining model diagnostics
check_model(cuckoo_glm_add, 
            residual_type = "normal",
            detrend = FALSE)


#Q-Q plot: Are deviance residuals approximately normal?
check_model(cuckoo_glm_add, 
            residual_type = "normal",
            detrend = FALSE,
            check = "qq")


#Residuals vs fitted: Is there remaining pattern?
check_model(cuckoo_glm_add, 
            residual_type = "normal",
            detrend = FALSE,
            check = "homogeneity")


#Dispersion check: What does this plot show?
check_model(cuckoo_glm_add, 
            residual_type = "normal",
            detrend = FALSE,
            check = "overdispersion")


#Calculate dispersion manually
dispersion_add <- cuckoo_glm_add$deviance / cuckoo_glm_add$df.residual
dispersion_add

# performance model check
check_overdispersion(cuckoo_glm_add)


#Hypothesis: The interaction is missing
ggplot(cuckoo, aes(x = Mass, y = Beg, colour = Species)) + 
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)", 
       y = "Begging calls per 6 seconds") +
  theme_minimal()


#Fit the model with Mass × Species interaction
cuckoo_glm_int <- glm(Beg ~ Mass * Species, 
                      data = cuckoo, 
                      family = poisson(link = "log"))

summary(cuckoo_glm_int)



#Compare dispersion between models
dispersion_comparison <- tibble(
  Model = c("Additive (no interaction)", 
            "Interaction included"),
  Dispersion = c(
    cuckoo_glm_add$deviance / cuckoo_glm_add$df.residual,
    cuckoo_glm_int$deviance / cuckoo_glm_int$df.residual
  )
)

dispersion_comparison


#Has dispersion reduced?
check_overdispersion(cuckoo_glm_int)



#Compare the fits between these models
anova(cuckoo_glm_add, cuckoo_glm_int)



# Generate predictions for both models
pred_additive <- emmeans(cuckoo_glm_add,
                         specs = ~ Mass + Species,
                         at = list(Mass = seq(0, 40, by = 1)),
                         type = "response") |>
  as_tibble() |>
  mutate(Model = "Additive")

pred_interaction <- emmeans(cuckoo_glm_int,
                            specs = ~ Mass + Species,
                            at = list(Mass = seq(0, 40, by = 1)),
                            type = "response") |>
  as_tibble() |>
  mutate(Model = "Interaction")

predictions_combined <- bind_rows(pred_additive, pred_interaction)

ggplot(predictions_combined, aes(x = Mass, y = rate, colour = Species)) +
  geom_point(data = cuckoo, aes(y = Beg), alpha = 0.4) +
  geom_line(aes(linetype = Model), linewidth = 1) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Nestling mass (g)",
       y = "Begging calls per 6 seconds",
       title = "Model comparison: Additive vs Interaction") +
  theme_minimal() +
  theme(legend.position = "right")+
  facet_wrap(~Model)



#Addressing remaining model checks
check_model(cuckoo_glm_int, 
            residual_type = "normal",
            detrend = FALSE)
