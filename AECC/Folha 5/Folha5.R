library(tidyverse)
library(broom)
library(palmerpenguins)
library(nycflights13)
library(car)
library(performance)
library(GGally)
library(see)

#Conjuntos feitos pela prof
penguins <- palmerpenguins::penguins |> drop_na()
glimpse(penguins)

flts <- nycflights13::flights |>
  filter(!is.na(dep_delay), !is.na(arr_delay),
         !is.na(distance), !is.na(air_time)) |>
  # amostra reprodutível
  slice_sample(n = 30000, replace = FALSE)
glimpse(flts)

#Ex1
#1.
penguins %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()

cor_peng <- penguins %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  cor(use = "complete.obs")

#2.
flts %>%
  select(arr_delay, dep_delay, distance, air_time) %>%
  ggpairs()

cor_flts <- flts %>%
  select(arr_delay, dep_delay, distance, air_time) %>%
  cor(use = "complete.obs")

#Ex2
#1.
mod_penguins <- lm(body_mass_g ~ bill_length_mm + 
                     bill_depth_mm * flipper_length_mm + 
                     species + sex, data = penguins)

summary(mod_penguins)

#3.
vif(mod_penguins)

#Ex3
#1.
par(mfrow = c(2,2))
plot(mod_penguins)

#2.
mod_penguins_log <- lm(log(body_mass_g) ~ bill_length_mm +
                         bill_depth_mm * flipper_length_mm +
                         species + sex,
                       data = penguins)

list(
  base = list(AIC = AIC(mod_penguins), BIC = BIC(mod_penguins), R2 = summary(mod_penguins)$adj.r.squared),
  log = list(AIC = AIC(mod_penguins_log), BIC = BIC(mod_penguins_log), R2 = summary(mod_penguins_log)$adj.r.squared)
)

#Ex4
#1.
mod_penguins_species <- lm(body_mass_g ~ bill_length_mm +
                         bill_depth_mm + flipper_length_mm *
                         species + sex,
                       data = penguins)

summary(mod_penguins_species)

#2.
fix_vals <- penguins %>%
  summarise(
    bill_length_mm = median(bill_length_mm, na.rm = TRUE),
    bill_depth_mm = median(bill_depth_mm, na.rm = TRUE)
  )

rng <- range(penguins$flipper_length_mm, na.rm = TRUE, finite = TRUE)

grid <- tidyr::expand_grid(
  flipper_length_mm = seq(from = rng[1], to = rng[2], length.out = 100),
  bill_length_mm    = fix_vals$bill_length_mm,
  bill_depth_mm     = fix_vals$bill_depth_mm,
  species           = factor("Adelie", levels = levels(penguins$species)),
  sex               = factor("male",   levels = levels(penguins$sex))
)

pred <- cbind(
  grid,
  as.data.frame(predict(mod_penguins_species, newdata = grid, interval = "confidence", level = 0.95))
)

ggplot(pred, aes(flipper_length_mm, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line() +
  labs(x = "Flipper length (mm)", y = "Pred. body mass (g)",
       title = "Efeito parcial de flipper_length_mm (Adelie, male)")

#Ex5
AIC(mod_penguins); AIC(mod_penguins_species)
BIC(mod_penguins); BIC(mod_penguins_species)
summary(mod_penguins)$adj.r.squared; summary(mod_penguins_species)$adj.r.squared

#Ex6
novo <- tibble(
  bill_length_mm = 45,
  bill_depth_mm = 18,
  flipper_length_mm = 200,
  species = "Adelie",
  sex = "male"
)

pred_mean <- predict(mod_penguins_species, newdata = novo, interval = "confidence", level = 0.95)
pred_pred <- predict(mod_penguins_species, newdata = novo, interval = "prediction", level = 0.95)

#Ex7
#1.
mod_flts <- lm(arr_delay ~ dep_delay +
                 log(distance) + air_time +
                 carrier, data = flts)

summary(mod_flts)

#3.
par(mfrow = c(2,2))
plot(mod_flts)

#4.
vif(mod_flts)