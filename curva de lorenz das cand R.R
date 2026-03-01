# === 0. Carregar pacotes ===
library(dplyr)
library(ineq)
library(ggplot2)

# === 1. Calcular a mediana da votação por município ===

# Candidatos (masculino)
mediana_masc <- database_candidatos %>%
  group_by(municipio) %>%
  summarise(
    vt_mediana = median(votos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(genero = "Candidatos")

# Candidatas (feminino)
mediana_fem <- database_candidatas %>%
  group_by(municipio = mun) %>%  # padroniza nome da coluna
  summarise(
    vt_mediana = median(vt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(genero = "Candidatas")

# === 2. Criar vetores de votação ordenados para a Curva de Lorenz ===

vt_masc <- mediana_masc %>%
  arrange(vt_mediana) %>%
  pull(vt_mediana)

vt_fem <- mediana_fem %>%
  arrange(vt_mediana) %>%
  pull(vt_mediana)

# === 3. Calcular curvas de Lorenz ===

lc_masc <- Lc(vt_masc)
lc_fem <- Lc(vt_fem)

# === 4. Criar data frames com as proporções acumuladas ===

df_masc <- data.frame(
  prop_municipios = lc_masc$p,
  prop_votacao = lc_masc$L,
  genero = "Candidatos"
)

df_fem <- data.frame(
  prop_municipios = lc_fem$p,
  prop_votacao = lc_fem$L,
  genero = "Candidatas"
)

# Juntar os dois data frames com candidatos primeiro
df_lorenz <- rbind(df_masc, df_fem)

# Converter a variável 'genero' em fator com ordem
df_lorenz$genero <- factor(df_lorenz$genero, levels = c("Candidatos", "Candidatas"))

# === 5. Calcular os índices de Gini ===

gini_masc <- ineq(vt_masc, type = "Gini")
gini_fem <- ineq(vt_fem, type = "Gini")

# === 6. Plotar a Curva de Lorenz com Gini na legenda ===

ggplot(df_lorenz, aes(x = prop_municipios, y = prop_votacao, color = genero)) +
  geom_line(linewidth = 1.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1.1) +
  scale_color_manual(
    values = c("Candidatos" = "blue3", "Candidatas" = "red"),
    labels = c(
      paste0("Candidatos (Gini = ", round(gini_masc, 3), ")"),
      paste0("Candidatas (Gini = ", round(gini_fem, 3), ")")
    )
  ) +
  labs(
    title = "",
    x = "Municípios organizados em ordem crescente de votação mediana",
    y = "Distribuição da votação mediana dos candidatos e das candidatas por região",
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

cat("Índice de Gini para candidatos:", round(gini_masc, 3), "\n")
cat("Índice de Gini para candidatas:", round(gini_fem, 3), "\n")
