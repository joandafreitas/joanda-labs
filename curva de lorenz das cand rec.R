library(dplyr)
library(ineq)
library(ggplot2)

# === 1. Candidatos (masculino) - recursos recebidos ===
rec_masc <- database_candidatos %>%
  group_by(municipio) %>%
  summarise(
    rec_mediana = median(total_liquido_rrec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(genero = "Candidatos")

# === 2. Candidatas (feminino) - recursos recebidos ===
rrec_fem <- database_candidatas %>%
  group_by(municipio = mun) %>%
  summarise(
    rec_mediana = median(tlrrec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(genero = "Candidatas")

# === 3. Vetores ordenados para a Curva de Lorenz ===
vt_masc <- rec_masc %>%
  arrange(rec_mediana) %>%
  pull(rec_mediana)

vt_fem <- rrec_fem %>%
  arrange(rec_mediana) %>%
  pull(rec_mediana)

# === 4. Calcular curvas de Lorenz ===
lc_masc <- Lc(vt_masc)
lc_fem <- Lc(vt_fem)

# === 5. Criar data frames para o gráfico ===
df_masc <- data.frame(
  prop_municipios = lc_masc$p,
  prop_recursos = lc_masc$L,
  genero = "Candidatos"
)

df_fem <- data.frame(
  prop_municipios = lc_fem$p,
  prop_recursos = lc_fem$L,
  genero = "Candidatas"
)

df_lorenz <- rbind(df_masc, df_fem)
df_lorenz$genero <- factor(df_lorenz$genero, levels = c("Candidatos", "Candidatas"))

# === 6. Calcular índices de Gini ===
gini_masc <- ineq(vt_masc, type = "Gini")
gini_fem <- ineq(vt_fem, type = "Gini")

# === 7. Plotar a curva de Lorenz ===
ggplot(df_lorenz, aes(x = prop_municipios, y = prop_recursos, color = genero)) +
  geom_line(linewidth = 1.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1.1) +
  scale_color_manual(
    values = c("Candidatos" = "blue3", "Candidatas" = "red"),
    labels = c(
      paste0("Candidatos (Gini = ", sprintf("%.3f", gini_masc), ")"),
      paste0("Candidatas (Gini = ", sprintf("%.3f", gini_fem), ")")
    )
  ) +
  labs(
    title = "",
    x = "Municípios organizados em ordem crescente do total mediano de recursos líquidos recebidos",
    y = "Distribuição dos recursos líquidos medianos recebidos por candidatos e candidatas por região",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 11.5, family = "sans", face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9)
  )

# === 8. Exibir índices de Gini no console ===
cat("Índice de Gini para candidatos (recursos):", sprintf("%.3f", gini_masc), "\n")
cat("Índice de Gini para candidatas (recursos):", sprintf("%.3f", gini_fem), "\n")
