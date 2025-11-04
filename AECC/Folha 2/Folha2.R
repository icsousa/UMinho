#Ex1
mtcars %>%
  as_tibble(rownames = "modelo") %>%
  select(modelo,mpg,cyl,hp) %>%
  filter(mpg > 25, cyl %in% c(4,6)) %>%
  mutate(
    kmpl = mpg * 0.4251,
    potencia = case_when(
      hp < 100 ~ "baixa",
      hp < 150 ~ "média",
      hp > 150 ~ "alta"
    )
  ) %>%
  arrange(desc(kmpl)) %>%
  head(8)

#Ex2
library(nycflights13)

flights %>%
  group_by(carrier) %>%
  summarise(
    n_voos = n(),
    atraso_medio_partida = mean(dep_delay, na.rm = TRUE),
    atraso_medio_chegada = mean(arr_delay, na.rm = TRUE),
    across(c(dep_delay, arr_delay),
           list(m = ~mean(.x, na.rm = TRUE),
                sd = ~sd(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  arrange(desc(n_voos)) %>%
  head(10)

#Ex3
vendas <- tibble(
  id = 1:4,
  Jan = c(10,12,9,11),
  Fev = c(11,8,13,10),
  Mar = c(9,12,7,15)
)

vendas_long <- vendas %>%
  pivot_longer(cols = Jan:Mar, names_to = "mês", values_to = "valor")

vendas_wide <- vendas_long %>%
  pivot_wider(names_from = "mês", values_from = "valor")

nomes <- tibble(
  nome_completo = c("Ana Silva", "Bruno Costa", "C. Rocha")
) %>%
  separate(nome_completo, into = c("primeiro", "apelido")) %>%
  unite(nome_composto, primeiro, apelido, sep="_", remove = TRUE)

#Ex4
voos_dest <- flights %>%
  count(dest, name = "n_voos")

tleft_join <- left_join(voos_dest, airports, by = c("dest" = "faa")) %>%
  select(dest, n_voos, name, lat, lon)

tinner_join <- inner_join(voos_dest, airports, by = c("dest" = "faa")) %>%
  nrow()

tfull_join <- full_join(voos_dest, airports, by = c("dest" = "faa")) %>%
  summarise(
    sem_meta = sum(is.na(name)),
    sem_voos = sum(is.na(n_voos))
  )

  