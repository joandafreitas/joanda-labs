# === 0. Carregar pacotes ===
library(dplyr)
library(ineq)
library(ggplot2)

# === 1. Calcular a mediana do gasto por voto por município ===

# Candidatos (masculino)
mediana_masc <- database_candidatos %>%
  group_by(municipio) %>%
  summarise(
    gastovt_mediana = median(gastovt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(genero = "Candidatos")

# Candidatas (feminino)
mediana_fem <- database_candidatas %>%
  group_by(municipio = mun) %>%
  summarise(
    gastovt_mediana = median(gastovt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(genero = "Candidatas")

# === 2. Criar vetores ordenados para a Curva de Lorenz ===

gvt_masc <- mediana_masc %>%
  arrange(gastovt_mediana) %>%
  pull(gastovt_mediana)

gvt_fem <- mediana_fem %>%
  arrange(gastovt_mediana) %>%
  pull(gastovt_mediana)

# === 3. Calcular curvas de Lorenz ===

lc_masc <- Lc(gvt_masc)
lc_fem <- Lc(gvt_fem)

# === 4. Criar data frames com proporções acumuladas ===

df_masc <- data.frame(
  prop_municipios = lc_masc$p,
  prop_gastovt = lc_masc$L,
  genero = "Candidatos"
)

df_fem <- data.frame(
  prop_municipios = lc_fem$p,
  prop_gastovt = lc_fem$L,
  genero = "Candidatas"
)

# Juntar os dois data frames
df_lorenz <- rbind(df_masc, df_fem)

# Ordenar os fatores
df_lorenz$genero <- factor(df_lorenz$genero, levels = c("Candidatos", "Candidatas"))

# === 5. Calcular os índices de Gini ===

gini_masc <- ineq(gvt_masc, type = "Gini")
gini_fem <- ineq(gvt_fem, type = "Gini")

# === 6. Plotar a Curva de Lorenz com Gini na legenda ===

ggplot(df_lorenz, aes(x = prop_municipios, y = prop_gastovt, color = genero)) +
  geom_line(linewidth = 1.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1.1) +
  scale_color_manual(
    values = c("Candidatos" = "blue3", "Candidatas" = "red"),
    labels = c(
      paste0("Candidatos (Gini = ", round(gini_masc, 3), ")"),
      paste0("Candidatas (Gini = ", round(gini_fem, 3), ")")
    )
  ) +
  labs(
    title = "",
    x = "Municípios organizados em ordem crescente de gasto por voto mediano",
    y = "Distribuição do gasto por voto mediano por município",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 11.5, family = "sans", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9)
  )

# === 7. Imprimir índices de Gini no console ===

cat("Índice de Gini para candidatos (gastovt):", round(gini_masc, 3), "\n")
cat("Índice de Gini para candidatas (gastovt):", round(gini_fem, 3), "\n")

