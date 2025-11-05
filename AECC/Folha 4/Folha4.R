library(tidyverse)
library(broom)
library(gapminder)
library(nycflights13)

#Ex1
#1.
peng <- penguins %>%
  drop_na(flipper_len, bill_len)

ggplot(peng, aes(x = bill_len, y = flipper_len)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Comprimento do Bico (mm)",
       y = "Comprimento da Barbatana (mm)",
       title = "Pinguis: Barbatana vs Bico")
#2.
mod_peng <- lm(flipper_len ~ bill_len, data = peng) 
summary(mod_peng)
tidy(mod_peng, conf.int = TRUE)
glance(mod_peng)[,c("r.squared", "adj.r.squared", "sigma")]

#4.
novo_peng <- tibble(bill_len = 45)
predict(mod_peng, newdata = novo_peng, interval = "prediction", level = 0.95)

#5.
augment(mod_peng) %>%
  ggplot(aes(.fitted, .resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(alpha = 0.5) + 
  labs(x = "Ajustados",
       y = "Resíduos",
       title = "Pinguins: Resíduos vs Ajustados")

qqnorm(resid(mod_peng)); qqline(resid(mod_peng))

#Ex2
data(diamonds, package = "ggplot2")

#1.
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Carat (Quilates)",
       y = "Preço ($)",
       title = "Diamonds: Carat vs Preço")

#2.
mod_diamonds_lin <- lm(price ~ carat, data = diamonds)
summary(mod_diamonds_lin)$r.squared
glance(mod_diamonds_lin)[,c("r.squared", "adj.r.squared", "sigma")]

#3.
mod_diamonds_lin_log <- lm(log(price) ~ log(carat), data = diamonds)
bind_rows(
  glance(mod_diamonds_lin) %>%
    mutate(modelo = "price ~ carat"),
  glance(mod_diamonds_lin_log) %>%
    mutate(modelo = "log(price) ~ log(carat)")
) %>%
  select(modelo, r.squared, adj.r.squared, sigma)

par(mfrow = c(1,2))
plot(mod_diamonds_lin, which = 1)
plot(mod_diamonds_lin_log, which = 1)

#4.
novo_diamond <- tibble(carat = 1)
predict(mod_diamonds_lin, newdata = novo_diamond, interval = "prediction", level = 0.95)

#Ex3
gm <- gapminder %>%
  filter(year == 2007)

#1.
ggplot(gm, aes(y = lifeExp, x = gdpPercap)) + 
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(y = "Esperança Média de Vida",
       x = "PIB (Log10)",
       title = "Países: Esperança Média de Vida vs PIB (2007)")

#2.
mod_gm <- lm(lifeExp ~ gdpPercap, data = gm)
mod_gm_log <- lm(lifeExp ~ log(gdpPercap), data = gm)
bind_rows(
  glance(mod_gm) %>%
    mutate(modelo = "lifeExp ~ gdpPercap"),
  glance(mod_gm_log) %>%
    mutate(modelo = "lifeExp ~ log(gdpPercap)")
) %>%
  select(modelo, r.squared, adj.r.squared, sigma)

#3.
tidy(mod_gm_log, conf.int = TRUE)

#4.
augment(mod_gm_log) %>%
  ggplot(aes(.fitted, .resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(alpha = 0.5) + 
  labs(x = "Ajustados",
       y = "Resíduos",
       title = "Países: Resíduos vs Ajustados")

qqnorm(resid(mod_gm_log)); qqline(resid(mod_gm_log))

#Ex4
#1.
fl <- flights %>%
  drop_na(dep_delay, distance) %>%
  filter(year == 2013)

ggplot(fl, aes(x = distance, y = dep_delay)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Distãncia (milhas)",
      y = "Atraso (min)",
      title = "Voos: Atrasos vs Distãncia")

#2.
mod_fl <- lm(dep_delay ~ distance, data = fl)
summary(mod_fl)

tidy(mod_fl, conf.int = TRUE)

#3.
aug <- augment(mod_fl)
cutoff <- 4/nrow(fl)

aug %>%
  arrange(desc(.cooksd)) %>%
  slice_head(n = 5)

i_max <- which.max(aug$.cooksd)
fl2 <- fl[-i_max, ]
mod_fl_2 <- lm(dep_delay ~ distance ,data = fl2)

bind_rows(
  tidy(mod_fl) %>%
    mutate(modelo = "com todos"),
  tidy(mod_fl_2) %>%
    mutate(modelo = "sem o mais influente")
) %>%
  filter(term == "distance")

#Ex5
#1.
subcuts <- c("Ideal", "Good")
dsub <- diamonds %>%
  filter(cut %in% subcuts)

#2.
tbl_slopes <- dsub %>%
  group_by(cut) %>%
  do(tidy(lm(price ~ carat, data = .))) %>%
  filter(term == "carat") %>%
  select(cut, estimate, std.error, p.value)

tbl_fit <- dsub %>%
  group_by(cut) %>%
  do(glance(lm(price ~ carat, data = .))) %>%
  select(cut, r.squared, adj.r.squared, sigma)

list(tbl_slopes = tbl_slopes, tbl_fit = tbl_fit)

#4.
par(mfrow=c(1,2))
plot(lm(price ~ carat, data = filter(dsub, cut == "Ideal")), which = 1, main = "Ideal: Resíduos vs Ajustados")
plot(lm(price ~ carat, data = filter(dsub, cut == "Good")), which = 1, main = "Good: Resíduos vs Ajustados")