# Carregar pacotes
library(dplyr)
library(ineq)
library(ggplot2)
library(tidyr)
library(forcats)

# 1. Mediana da votação por município e região — Candidatas
mediana_fem <- database_candidatas %>%
  group_by(reg, municipio = mun) %>%
  summarise(vt_mediana = median(vt, na.rm = TRUE), .groups = 'drop') %>%
  mutate(genero = "Candidatas")

# 2. Mediana da votação por município e região — Candidatos
mediana_masc <- database_candidatos %>%
  group_by(reg, municipio) %>%
  summarise(vt_mediana = median(votos, na.rm = TRUE), .groups = 'drop') %>%
  mutate(genero = "Candidatos")

# 3. Unir os dados
mediana_total <- bind_rows(mediana_masc, mediana_fem)

# 4. Calcular curva de Lorenz por região e gênero
df_lorenz <- mediana_total %>%
  group_by(reg, genero) %>%
  arrange(vt_mediana) %>%
  summarise(curva = list(Lc(vt_mediana)), .groups = "drop") %>%
  rowwise() %>%
  mutate(
    prop_municipios = list(curva$p),
    prop_votacao = list(curva$L)
  ) %>%
  unnest(c(prop_municipios, prop_votacao)) %>%
  select(reg, genero, prop_municipios, prop_votacao)

# 5. Calcular Índice de Gini por região e gênero
gini_por_regiao <- mediana_total %>%
  group_by(reg, genero) %>%
  summarise(gini = ineq(vt_mediana, type = "Gini"), .groups = "drop") %>%
  mutate(
    texto = paste0("Gini: ", format(round(gini, 3), decimal.mark = ",")),
    y_pos = 0.05,  # mesma altura para ambos
    x_pos = ifelse(genero == "Candidatos", 0.60, 0.90)  # ajusta horizontalmente
    
  )

# 6. Gráfico final
ggplot(df_lorenz, aes(x = prop_municipios, y = prop_votacao, color = genero)) +
  geom_line(size = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.9) +
  geom_text(
    data = gini_por_regiao,
    aes(x = x_pos, y = y_pos, label = texto, color = genero),
    inherit.aes = FALSE,
    size = 3.8, #aumentar o tamanho dos índices
    fontface = "plain",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Candidatos" = "blue3", "Candidatas" = "red"),
    breaks = c("Candidatos", "Candidatas")
  ) +
  facet_wrap(~ reg, ncol = 2) +
  labs(
    title = "",
    x = "Municípios organizados em ordem crescente de votação mediana",
    y = "Distribuição da votação mediana dos candidatos e das candidatas por região",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10.4),  # tamanho do texto da legenda
    plot.title = element_text(size = 11.5, family = "sans", face = "bold", hjust = 0.2),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold")
  )
