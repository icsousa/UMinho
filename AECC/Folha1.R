library(tidyverse)

set.seed(1234)

n <- 200
alunos <- tibble(
  id            = 1:n,
  curso         = sample(c("CC", "SI"), n, replace = TRUE, prob = c(.65, .35)),
  ano           = sample(1:3, n, replace = TRUE, prob = c(.4, .35, .25)),
  sexo          = sample(c("F", "M"), n, replace = TRUE),
  horas_estudo  = round(rlnorm(n, meanlog = log(6), sdlog = 0.5), 1),
  faltas        = pmax(0, round(rpois(n, lambda = 3) - rbinom(n, 1, .1)*3)),
  nota          = pmin(20,
                       pmax(0,
                            round( 8 +
                                     0.6 * pmin(horas_estudo, 20) -
                                     0.4 * pmin(faltas, 10) +
                                     rnorm(n, 0, 2.5), 1)))
) %>%
  mutate(
    # introduzir alguns outliers na nota
    nota = replace(nota, sample(1:n, 3), c(2, 19.5, 0)),
    # introduzir NAs em horas_estudo
    horas_estudo = replace(horas_estudo, sample(1:n, 6), NA_real_)
  )

head(alunos)

#Ex1
glimpse(alunos)

#Ex2
with(alunos, c(mean = mean(nota, na.rm=TRUE),
               median = median(nota, na.rm=TRUE),
               sd = sd(nota, na.rm=TRUE),
               IQR = IQR(nota, na.rm=TRUE),
               MAD = mad(nota, constant = 1, na.rm=TRUE)))

summary(alunos$nota)

#Ex3
q1 <- quantile(alunos$nota, .25, na.rm=TRUE)
q3 <- quantile(alunos$nota, .75, na.rm=TRUE)
iqr <- q3 - q1
low <- q1 - 1.5*iqr; high <- q3 + 1.5*iqr
which_out <- which(alunos$nota < low | alunos$nota > high)

ggplot(alunos, aes(y = nota)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Boxplot de nota")

#Ex4
bw_fd <- 2*IQR(alunos$nota, na.rm=TRUE) * length(na.omit(alunos$nota))^(-1/3)
ggplot(alunos, aes(nota)) +
  geom_histogram(binwidth = bw_fd, color="white") +
  geom_density(aes(y = after_stat(count * bw_fd)), alpha=.2, fill="steelblue") +
  labs(title = "Histograma + densidade (FD binwidth)")

ggplot(alunos, aes(nota)) + stat_ecdf() + labs(title="ECDF de nota")

#Ex5
ggplot(alunos, aes(x = curso, y = nota, fill = curso)) +
  geom_boxplot(alpha=.6) + theme(legend.position="none")

alunos %>%
  group_by(curso, sexo) %>%
  summarise(n = n(),
            media = mean(nota, na.rm=TRUE),
            mediana = median(nota, na.rm=TRUE),
            IQR = IQR(nota, na.rm=TRUE),
            .groups = "drop")

#Ex6
dados2 <- alunos %>% drop_na(horas_estudo, nota)
ggplot(dados2, aes(horas_estudo, nota)) +
  geom_point(alpha=.6) + geom_smooth(method = "lm", se = FALSE)

cor_pearson  <- cor(dados2$horas_estudo, dados2$nota, method="pearson")
cor_spearman <- cor(dados2$horas_estudo, dados2$nota, method="spearman")
c(pearson = cor_pearson, spearman = cor_spearman)

#Ex7
p1 <- ggplot(alunos, aes(horas_estudo)) + geom_histogram() + 
            labs(title="Horas de estudo")
p2 <- ggplot(alunos %>% mutate(log_horas = log(horas_estudo)),
            aes(log_horas)) + geom_histogram() + 
            labs(title="log(Horas de estudo)")
p1
p2

ggplot(alunos %>% drop_na(horas_estudo, nota) %>% 
         mutate(log_horas = log(horas_estudo)),
          aes(log_horas, nota)) + geom_point(alpha=.6) + 
          geom_smooth(method="lm", se=FALSE)

#Ex8
colSums(is.na(alunos))

lista_completa <- alunos %>% 
                    drop_na(horas_estudo)
imp_mediana <- alunos %>%
                mutate(horas_estudo = if_else(is.na(horas_estudo),
                                median(horas_estudo, na.rm=TRUE),
                                horas_estudo))

lista_completa %>% 
  group_by(curso) %>% 
  summarise(media_nota = mean(nota, na.rm=TRUE))
imp_mediana %>% 
  group_by(curso) %>% 
  summarise(media_nota = mean(nota, na.rm=TRUE))