library(tidyverse)
library(ggplot2)

#Ex1
load("dados_eurostat.RData")
head(dados_eurostat)
dim(dados_eurostat)
view(dados_eurostat)

#Ex2
eurostat_portugal <- dados_eurostat %>%
  filter(country == "Portugal")

ggplot(eurostat_portugal, aes(x = time, y = Total)) + 
  geom_line()

#Ex3
dif_sex <- dados_eurostat %>%
  group_by(country) %>%
  filter(year == 2019) %>%
  mutate(dif = Females - Males) %>%
  summarise(media = mean(dif, na.rm = TRUE)) %>%
  arrange(desc(media)) %>%
  head(5)
  
ggplot(dif_sex, aes(x = country, y = media)) + 
  geom_bar(stat = "identity")

#Ex4
racio_juv_ad <- dados_eurostat %>%
  group_by(country) %>%
  filter(year > 2014) %>%
  mutate(ratio = Under25 / Over25) %>%
  summarise(media = mean(ratio, na.rm = TRUE)) %>%
  arrange(desc(media)) %>%
  head(5)

ggplot(racio_juv_ad, aes(x = country, y = media)) +
  geom_bar(stat = "identity")

#Ex5
taxa_total <- dados_eurostat %>%
  group_by(country) %>%
  summarise(media = mean(Total, na.rm = TRUE)) %>%
  arrange(desc(media)) %>%
  head(5)

graph_tt <- dados_eurostat %>%
  filter(country %in% taxa_total$country)

ggplot(graph_tt, aes(x = time, y = Total, color = country)) + 
  geom_line()

#Ex6
evo_sex <- graph_tt %>%
  pivot_longer(cols = c(Females, Males), names_to = "Sexo", values_to = "Valor")

ggplot(evo_sex, aes(x = time, y = Valor, color = Sexo)) + 
  geom_line() + 
  facet_wrap(~country)