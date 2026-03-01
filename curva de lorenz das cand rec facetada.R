# Carregar pacotes
library(dplyr)
library(ineq)
library(ggplot2)
library(tidyr)
library(forcats)

# 1. Mediana do gasto por voto por município e região — Candidatas
gastovt_fem <- database_candidatas %>%
  filter(!is.na(reg)) %>%
  group_by(reg, municipio = mun) %>%
  summarise(gastovt_mediana = median(gastovt, na.rm = TRUE), .groups = 'drop') %>%
  mutate(genero = "Candidatas")

# 2. Mediana do gasto por voto por município e região — Candidatos
gastovt_masc <- database_candidatos %>%
  filter(!is.na(reg)) %>%
  group_by(reg, municipio) %>%
  summarise(gastovt_mediana = median(gastovt, na.rm = TRUE), .groups = 'drop') %>%
  mutate(genero = "Candidatos")

# 3. Unir os dados
gastovt_total <- bind_rows(gastovt_masc, gastovt_fem)

# 4. Calcular curva de Lorenz por região e gênero
df_lorenz <- gastovt_total %>%
  group_by(reg, genero) %>%
  arrange(gastovt_mediana) %>%
  summarise(curva = list(Lc(gastovt_mediana)), .groups = "drop") %>%
  rowwise() %>%
  mutate(
    prop_municipios = list(curva$p),
    prop_gastovt = list(curva$L)
  ) %>%
  unnest(c(prop_municipios, prop_gastovt)) %>%
  select(reg, genero, prop_municipios, prop_gastovt)

# 5. Calcular Índice de Gini por região e gênero
gini_por_regiao <- gastovt_total %>%
  group_by(reg, genero) %>%
  summarise(gini = ineq(gastovt_mediana, type = "Gini"), .groups = "drop") %>%
  mutate(
    texto = paste0("Gini: ", format(round(gini, 3), decimal.mark = ",")),
    y_pos = 0.05,
    x_pos = ifelse(genero == "Candidatos", 0.60, 0.90)
  )

# 6. Gráfico final facetado por região
ggplot(df_lorenz, aes(x = prop_municipios, y = prop_gastovt, color = genero)) +
  geom_line(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.9) +
  geom_text(
    data = gini_por_regiao,
    aes(x = x_pos, y = y_pos, label = texto, color = genero),
    inherit.aes = FALSE,
    size = 3.8,
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
    x = "Municípios organizados em ordem crescente do gasto por voto (mediana)",
    y = "Distribuição do gasto por voto mediano por candidatos e candidatas por região",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10.4),
    plot.title = element_text(size = 11.5, family = "sans", face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold")
  )

