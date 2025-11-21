library(tidyverse)
library(MASS)
library(lubridate)
library(xtable)
library(keras)
library(recipes)
library(pROC)
library(caret)
library(nnet)
library(neuralnet)
library(ggpubr)
library(ggnewscale)

install.packages("")

################################

dados <- read_csv("dados_volumetria_cardapio.csv")

dados %>% glimpse

precip_2024 <- read_csv("precip_diaria_2024_Cepagri.csv", col_names = c("dia", "precip"))
precip_2025 <- read_csv("precip_diaria_2025parcial_Cepagri.csv", col_names = c("dia", "precip"))
temp <- readxl::read_xlsx("export.xlsx") %>%
  dplyr::select(date, tavg, tmin, tmax)

dados2 <- dados %>% 
  dplyr::select(c(Data, Total_A, Total_J, Total_Dia, Dia_Semana, Mes, refeicao,
                  prato, acompanhamento, guarnicao, salada, sobremesa, suco))

################

dados_transformados <- dados2 %>%
  rename(refeicao_original = refeicao) %>%
  pivot_longer(
    cols = c(Total_A, Total_J),
    names_to = "tipo_total",
    values_to = "total_refeicao"
  ) %>%
  mutate(refeicao = ifelse(tipo_total == "Total_A", "Almoço", "Jantar")) %>%
  separate(refeicao_original,
           into = c("refeicao_tmp", "tipo_cardapio"),
           sep = " - ") %>%
  mutate(
    tipo_cardapio = ifelse(tipo_cardapio == "Cardápio Padrão",
                           "cardapio_padrao", "cardapio_vegano"),
    refeicao_tmp = trimws(refeicao_tmp)
  ) %>%
  filter(refeicao == refeicao_tmp) %>%
  pivot_wider(
    id_cols = c(Data, refeicao, total_refeicao, Dia_Semana, Mes),
    names_from = tipo_cardapio,
    values_from = prato
  ) %>%
  dplyr::select(Data, refeicao, total_refeicao, cardapio_padrao, cardapio_vegano,
                everything())


# juntando temp e prec

precip_2024 <- precip_2024 %>% 
  mutate(precip = as.double(precip))

precip_2024$ano <- rep(2024, nrow(precip_2024))
precip_2025$ano <- rep(2025, nrow(precip_2025))

precip <- bind_rows(precip_2024, precip_2025) %>% 
  mutate(Data = ymd(paste0(ano, "-01-01")) +
           days(dia-1)) %>% 
  filter(Data <= as.Date("2025-07-31"))

dados_transformados <- dados_transformados %>% 
  left_join(precip, by = "Data")

temp$Data <- temp$date
temp <- temp %>% 
  dplyr::select(Data, tavg, tmin, tmax) %>% 
  filter(Data <= as.Date("2025-07-31"))

temp$Data <- as.Date(temp$Data)

dados_transformados <- dados_transformados %>% left_join(temp, by = "Data")

# separação por tipo de proteina

cardapio_final <- dados_transformados %>% 
  mutate(
    cardapio_trans = case_when(
      grepl("STROGONOFF", cardapio_padrao, ignore.case = TRUE) ~ "strogonoff",
      grepl("PUCHERO|GUISADO|GUIZADO|FRITADA|OMELETE|SALSICHA", cardapio_padrao, ignore.case = TRUE) ~ "outros",
      grepl("LINGUIÇA|BISTECA|COPA LOMBO|CUBOS SUÍNOS|PERNIL|PALETA|SUÍNO|SUÍNA", cardapio_padrao, ignore.case = TRUE) ~ "carne suína",
      grepl("CARNE|BIFE|BOVINOS|ALMÔNDEGA|BOVINA|QUIBE|ACÉM|PATINHO|POLPETONE|BOVINAS|BOVINO|LAGARTO", cardapio_padrao, ignore.case = TRUE) ~ "carne bovina",
      grepl("FRANGO|SOBRECOXA|SASSAMI|STEAK|NUGGETS|NUGGET", cardapio_padrao, ignore.case = TRUE) ~ "frango",
      grepl("PEIXE|PESCADA|TILÁPIA|MOQUECA", cardapio_padrao, ignore.case = TRUE) ~ "peixe",
      grepl("FEIJOADA", cardapio_padrao, ignore.case = TRUE) ~ "feijoada"
    )
  )

# reordenando var. categoricas

cardapio_final <- cardapio_final %>%
  mutate(
    refeicao = factor(refeicao, levels = c("Almoço", "Jantar")),
    Dia_Semana = factor(
      Dia_Semana,
      levels = c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado")
    )
  )

# teste modelo binomial negativa

fit_p <- glm(total_refeicao ~ cardapio_trans + refeicao + Dia_Semana,
             data = cardapio_final, family = poisson())

fit_p %>% summary()



fit <- glm.nb(total_refeicao ~ cardapio_trans + refeicao + Dia_Semana,
              data = cardapio_final)

fit %>% summary() # residuo ok

car::vif(fit) # vif mt bom
 
car::residualPlot(fit) # ok

# frequencia por dias da semana

cardapio_final %>% 
  ggplot(aes(x = Dia_Semana, y = total_refeicao, fill = refeicao)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  scale_fill_manual(values = c("Almoço" = "steelblue4", "Jantar" = "orange")) +
  labs(
    title = "Frequência Média por Dia Semanal",
    x = "Dia Semana",
    y = "Frequência Média",
    fill = "Refeição"
  ) +
  theme_minimal()


# frequencia anual

cardapio_final %>% ggplot(aes(x = Data, y = total_refeicao, color = refeicao)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Almoço" = "steelblue4", "Jantar" = "orange")) +
  labs(
    title = "Frequência por Data",
    x = "Data",
    y = "Frequência",
    color = "Refeição"
  ) +
  theme_minimal()

# frequencia por pratos

cardapio_final %>% drop_na() %>% 
  ggplot(aes(x = cardapio_trans, y = total_refeicao, fill = refeicao)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = 21, outlier.size = 1.5) +
  scale_fill_manual(values = c("Almoço" = "steelblue4", "Jantar" = "orange")) +
  labs(
    title = "Frequência Média por Cardápio",
    x = "Cardápio",
    y = "Frequência Média",
    fill = "Refeição"
  ) +
  theme_minimal()

### chuva e n chuva

chuva <- dados_transformados %>% mutate(chuva = ifelse(precip < 1, "Não Choveu", "Choveu"))

chuva$chuva <- factor(chuva$chuva, levels = c("Não Choveu", "Choveu"))  
  
chuva %>% 
  ggplot(aes(x = chuva, y = total_refeicao, fill = refeicao)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  scale_fill_manual(values = c("Almoço" = "steelblue4", "Jantar" = "orange")) +
  labs(
    title = "Frequência Média por Cardápio",
    x = "Cardápio",
    y = "Frequência Média",
    fill = "Refeição"
  ) +
  theme_minimal()

test <- dados_transformados %>% mutate(chuva = ifelse(precip >= 1, "chuva", "n chuva")) %>% 
  drop_na()

fittest <- glm.nb(total_refeicao ~ chuva + refeicao,
                  data = test)
summary(fittest)    

# temperatura

dados_transformados %>% 
  ggplot(aes(x = tmax, y = total_refeicao)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

# ferias nao ferias

ferias <- dados_transformados %>% filter(Data >= as.Date("2025-01-01")) %>% 
  mutate(ferias = ifelse(Data <= as.Date("2025-02-24") | Data >= as.Date("2025-07-12"), "férias", "aula")) %>% 
  dplyr::select(Data, refeicao, total_refeicao, ferias)

fitferias <- glm.nb(total_refeicao ~ ferias + refeicao,
                    data = ferias)

summary(fitferias)

ferias %>% 
  ggplot(aes(x = ferias, y = total_refeicao, fill = refeicao)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  scale_fill_manual(values = c("Almoço" = "steelblue4", "Jantar" = "orange")) +
  labs(
    title = "Frequência Média por Cardápio",
    x = "Cardápio",
    y = "Frequência Média",
    fill = "Refeição"
  ) +
  theme_minimal()

# suco e sobremesa

dados_transformados$suco %>% unique()
dados_transformados$sobremesa %>% unique()

suco <- dados_transformados %>% 
  mutate(
    suco2 = case_when(
      grepl("LARANJA", suco, ignore.case = TRUE) ~ "LARANJA",
      grepl("CAJU|CAJÚ", suco, ignore.case = TRUE) ~ "CAJU",
      grepl("MANGA", suco, ignore.case = TRUE) ~ "MANGA",
      grepl("GOIABA", suco, ignore.case = TRUE) ~ "GOIABA",
      grepl("UVA", suco, ignore.case = TRUE) ~ "UVA",
      grepl("ABACAXI", suco, ignore.case = TRUE) ~ "ABACAXI",
      grepl("MORANGO|FRAMBOESA", suco, ignore.case = TRUE) ~ "MORANGO",
      grepl("ACEROLA", suco, ignore.case = TRUE) ~ "ACEROLA"
    )
  )

suco$suco2 %>% unique()

suco %>% drop_na() %>% 
  ggplot(aes(x = suco2, y = total_refeicao, fill = refeicao)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  scale_fill_manual(values = c("Almoço" = "steelblue4", "Jantar" = "orange")) +
  labs(
    title = "Frequência Média por Cardápio",
    x = "Cardápio",
    y = "Frequência Média",
    fill = "Refeição"
  ) +
  theme_minimal()
  
#### exportanto dados

dados_transformados$precip <- dados_transformados$precip.x
dados_transformados

export <- dados_transformados %>% 
  dplyr::select(Data, refeicao, total_refeicao, cardapio_padrao, cardapio_vegano, Dia_Semana,
         precip, tavg, tmin, tmax)

write_csv(export, "dados_transformados.csv")

### ferias e nao ferias todo periodo

dadosf <- dados_transformados %>%
  mutate(
    Ferias = case_when(
      
      Data <= as.Date("2024-02-28") ~ "Férias",
      Data >= as.Date("2024-07-06") & Data <= as.Date("2024-07-31") ~ "Férias", 
      # Dez 2024 até 24 Fev 2025
      Data >= as.Date("2024-12-07") & Data <= as.Date("2025-02-24") ~ "Férias",
      
      # Julho 2025
      Data >= as.Date("2025-07-12") & Data <= as.Date("2025-07-31") ~ "Férias",
      
      TRUE ~ "Período letivo"
    )
  )

dadosf$Ferias <- factor(dadosf$Ferias, levels = c("Período letivo", "Férias"))

dadosf %>% 
  ggplot(aes(x = Ferias, y = total_refeicao, fill = refeicao)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  scale_fill_manual(values = c("Almoço" = "steelblue4", "Jantar" = "orange")) +
  labs(
    title = "Frequência Média por Cardápio",
    x = "Cardápio",
    y = "Frequência Média",
    fill = "Refeição"
  ) +
  theme_minimal()

### graficos finais 

# série

ggplot(dados_transformados, aes(x = Data, y = total_refeicao, color = refeicao)) +
  geom_line(linewidth = 0.1) +
  scale_color_manual(values = c("Almoço" = "steelblue", "Jantar" = "orange")) +
  labs(
    title = "Série Temporal da Frequência",
    x = "Data",
    y = "Número de Pessoas",
    color = "Refeição"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave("serietemporalfrequencia.pdf",
       width = 3.5, height = 2.5, units = "in",
       dpi = 600, device = cairo_pdf)



# dias da semana



dados_transformados$Dia_Semana <- factor(dados_transformados$Dia_Semana,
                                         levels = c("Domingo", "Segunda", 
                                                    "Terça", "Quarta",
                                                    "Quinta", "Sexta",
                                                    "Sábado"))

ggplot(dados_transformados, aes(x = Dia_Semana, y = total_refeicao, fill = refeicao)) +
  #stat_summary(fun = mean, geom = "bar", position = "dodge", width = 0.7) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = 21, outlier.size = 1.5)
  scale_fill_manual(values = c("Almoço" = "steelblue", "Jantar" = "orange")) +
  labs(
    title = "Média de Frequência por Dia da Semana",
    x = "Dia da Semana",
    y = "Média de Pessoas",
    fill = "Refeição"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave("frequenciamediasemanal.pdf",
       width = 3.5, height = 2.5, units = "in",
       dpi = 600, device = cairo_pdf)

ggplot(dados_transformados, aes(x = Dia_Semana, y = total_refeicao, fill = refeicao)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = 21, outlier.size = 1.5) +
  scale_fill_manual(values = c("Almoço" = "steelblue", "Jantar" = "orange")) +
  labs(
    title = "Frequência por Dia da Semana",
    x = "Dia da Semana",
    y = "Número de Pessoas",
    fill = "Refeição"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


#

# cardapio proteina

cardapio_final %>% drop_na() %>% 
ggplot(aes(x = cardapio_trans, y = total_refeicao, fill = refeicao)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = 21, outlier.size = 1.5) +
  scale_fill_manual(values = c("Almoço" = "steelblue", "Jantar" = "orange")) +
  labs(
    title = "Média de Frequência por Cardapio",
    x = "Cardápio",
    y = "Média de Pessoas",
    fill = "Refeição"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )
ggsave("FrequenciaCardapio.pdf",
       width = 3.5, height = 2.5, units = "in",
       dpi = 600, device = cairo_pdf)

# a

dados_transformados

dados_transformados %>% 
  ggplot() +
  geom_bar(aes(x = precip, y = total_refeicao))

plot(dados_transformados$precip,dados_transformados$total_refeicao, xlim = c(0,50))

# periodo de ferias

dadosf %>% ggplot(aes(x = Ferias, y = total_refeicao, fill = refeicao)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = 21, outlier.size = 1.5) +
  scale_fill_manual(values = c("Almoço" = "steelblue", "Jantar" = "orange")) +
  labs(
    title = "Frequência por Período",
    x = "Período",
    y = "Número de Pessoas",
    fill = "Refeição"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave("frequenciaferias.pdf",
       width = 3.5, height = 2.5, units = "in",
       dpi = 600, device = cairo_pdf)

# chuva

chuva %>%
  ggplot(aes(x = chuva, y = total_refeicao, fill = refeicao)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = 21, outlier.size = 1.5) +
  scale_fill_manual(values = c("Almoço" = "steelblue", "Jantar" = "orange")) +
  labs(
    title = "Frequência por Clima",
    x = "Clima",
    y = "Número de Pessoas",
    fill = "Refeição"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave("frequenciachuva.pdf",
       width = 3.5, height = 2.5, units = "in",
       dpi = 600, device = cairo_pdf)


chuva %>% 
  ggplot(aes(x = precip, y = total_refeicao)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.4) +
  
  # linha de tendência linear
  geom_smooth(method = "lm", se = F, color = "orange", size = 1.2) +
  
  labs(
    title = "Frequência nos Restaurantes por Precipitação",
    x = "Precipitação (mm)",
    y = "Frequência Média"
  ) +
  
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(b = 10)),
    legend.position = "none"
  ) +
  xlim(c(0,50))

chuva %>% 
  group_by(chuva, refeicao) %>% 
  summarise(media = mean(total_refeicao))

# p1 de calculo

p1 <- dados_transformados %>% 
  filter(between(Data, as.Date("2025-02-24"), as.Date("2025-05-05")))

p1 <- p1 %>% 
  mutate(prova = ifelse(Data <= "2025-04-04", "antes p1", "depois p1"))

p1 %>% 
  group_by(prova) %>% 
  summarise(media = mean(total_refeicao)) %>% 
  xtable()

################## modelo nn bn

dadosm <- cardapio_final %>% mutate(chuva = ifelse(precip < 1, "Não Choveu", "Choveu")) %>% 
  dplyr::select(-c(cardapio_padrao, cardapio_vegano, tmin, tmax, dia, ano, precip, Data,
                   Mes))

modelom <- glm.nb(total_refeicao ~., data = dadosm)

summary(modelom)

################

cardapiof <- cardapio_final %>% 
  mutate(Mes = as.character(Mes), ano = as.character(ano),
         chuva = ifelse(precip > 1, "chuva", "nao chuva")) 

write_csv(cardapiof, "cardapio_final.csv")

cep <- read_csv("precip_diaria_2024_Cepagri.csv", col_names = c("dia", "precip"))

cep <- cep %>% 
  filter(dia <= 192)

meteo <- read_csv("meteostat 1jan 10jul 2024.csv") %>% 
  dplyr::select(date, prcp) %>% 
  filter(date <= as.Date(""))

t.test(cardapiof$precip, meteo$prcp)

write_csv(cep, "cepagri 1jan 10jul 2024.csv")

t.test(meteo$prcp, cep$precip, paired = T)

cardapiof <- cardapiof %>% 
  mutate(
    Período = case_when(
      
      Data <= as.Date("2024-01-03") ~ "Férias",
      Data >= as.Date("2024-01-04") & Data <= as.Date("2024-02-17") ~ "Aulas de Verão", 
      Data >= as.Date("2024-02-18") & Data <= as.Date("2024-02-27") ~ "Férias",
      Data >= as.Date("2024-02-28") & Data <= as.Date("2024-07-06") ~ "1º Semestre",
      Data >= as.Date("2024-07-07") & Data <= as.Date("2024-07-31") ~ "Férias",
      Data >= as.Date("2024-08-01") & Data <= as.Date("2024-12-07") ~ "2º Semestre",
      Data >= as.Date("2024-12-08") & Data <= as.Date("2025-01-05") ~ "Férias",
      Data >= as.Date("2025-01-06") & Data <= as.Date("2025-02-11") ~ "Aulas de Verão", 
      Data >= as.Date("2025-02-12") & Data <= as.Date("2025-02-23") ~ "Férias",
      Data >= as.Date("2025-02-24") & Data <= as.Date("2025-07-12") ~ "1º Semestre",
      Data >= as.Date("2025-07-13") & Data <= as.Date("2025-07-31") ~ "Férias",
      
      TRUE ~ "har har"
    ),
      cardapio_trans = case_when(
        grepl("STROGONOFF", cardapio_padrao, ignore.case = TRUE) ~ "strogonoff",
        grepl("PUCHERO|GUISADO|GUIZADO|FRITADA|OMELETE|SALSICHA", cardapio_padrao, ignore.case = TRUE) ~ "outros",
        grepl("LINGUIÇA|BISTECA|COPA LOMBO|CUBOS SUÍNOS|PERNIL|PALETA|SUÍNO|SUÍNA", cardapio_padrao, ignore.case = TRUE) ~ "carne suína",
        grepl("CARNE|BIFE|BOVINOS|ALMÔNDEGA|BOVINA|QUIBE|ACÉM|PATINHO|POLPETONE|BOVINAS|BOVINO|LAGARTO", cardapio_padrao, ignore.case = TRUE) ~ "carne bovina",
        grepl("FRANGO|SOBRECOXA|SASSAMI|STEAK|NUGGETS|NUGGET", cardapio_padrao, ignore.case = TRUE) ~ "frango",
        grepl("PEIXE|PESCADA|TILÁPIA|MOQUECA", cardapio_padrao, ignore.case = TRUE) ~ "peixe",
        grepl("FEIJOADA", cardapio_padrao, ignore.case = TRUE) ~ "feijoada"
    )
  ) 

cardapiof %>% 
  dplyr::group_by(Ferias) %>% 
  summarise(a = n())

#####

ggplot(chuva, aes(x = chuva, y = total_refeicao, fill = chuva)) +
  geom_boxplot() + 
  stat_compare_means(
    method = "anova",    
    label = "p.signif"
  ) +
  labs(
    title = "a",
    x = "b",
    y = "Número de Pessoas"
  ) +
  theme_minimal(base_size = 9)

##

intervalos <- cardapiof %>%
  group_by(Ferias) %>%
  summarise(
    inicio = min(Data),
    fim = max(Data)
  )

# gráfico
ggplot(cardapiof, aes(x = Data, y = total_refeicao)) +
  geom_rect(data = intervalos,
            aes(xmin = inicio, xmax = fim,
                ymin = -Inf, ymax = Inf,
                fill = Ferias),
            alpha = 0.3,
            inherit.aes = FALSE) +
  
  geom_line(color="black") +
  
  scale_fill_manual(values = cores) +
  theme_minimal() +
  labs(
    title = "Série temporal com períodos marcados",
    x = "Data",
    y = "Frequência",
    fill = "Período"
  )

# aaa

df_blocos <- cardapiof %>%
  arrange(Data) %>%
  mutate(
    bloco = cumsum(Período != lag(Período, default = first(Período)))
  )

intervalos <- df_blocos %>%
  group_by(Período, bloco) %>%
  summarise(
    xmin = min(Data),
    xmax = max(Data),
    .groups = "drop"
  )

df_blocos %>% 
  group_by(Ferias) %>% 
  summarise(n = n())

# periodo final

ggplot(df_blocos, aes(x = Data, y = total_refeicao, color = refeicao)) +
  geom_line(linewidth = 0.1) +
  scale_fill_manual(name = "Período",
                    values = c("Aulas de Verão" = "blue",
                               "1º Semestre"  = "orange",
                               "2º Semestre"  = "red",
                               "Férias"= "green"
  ))+
  scale_color_manual(name = "Refeição",
                     values = c("Almoço" = "steelblue", "Jantar" = "orange"
                                )) +
  geom_rect(data = intervalos,
            aes(xmin = xmin, xmax = xmax,
                ymin = -Inf, ymax = Inf,
                fill = Período),
            alpha = 0.15,
            inherit.aes = FALSE) +
  labs(
    title = "Frequência por Período",
    x = "Data",
    y = "Número de Pessoas",
    color = "Refeição"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    #legend.text = element_text(size = 3),
    legend.title = element_blank(),
    #legend.box = "vertical"
  ) +
  guides(
    fill = guide_legend(nrow = 2),
    color = guide_legend(nrow = 2)
  )

ggsave("serietemporalperiodo.pdf",
       width = 3.5, height = 2.5, units = "in",
       dpi = 600, device = cairo_pdf)

cardapiof %>% ggplot(aes(x = Ferias, y = total_refeicao, fill = refeicao)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = 21, outlier.size = 1.5) +
  scale_fill_manual(values = c("Almoço" = "steelblue", "Jantar" = "orange")) +
  labs(
    title = "Frequência por Período",
    x = "Período",
    y = "Número de Pessoas",
    fill = "Refeição"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

cardapiof %>% 
  group_by(Ferias, refeicao) %>% 
  summarise(media = mean(total_refeicao),
            mediana = median(total_refeicao))
