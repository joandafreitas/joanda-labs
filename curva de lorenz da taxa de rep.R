library(ineq)
library(tidyr)
library(ggplot2)

# Calcular curva de Lorenz
lorenz_fem <- Lc(dados$taxa_ver_fem)
lorenz_masc <- Lc(dados$taxa_ver_masc)

# Criar dataframe
df_lorenz <- data.frame(
  p = lorenz_fem$p,
  fem = lorenz_fem$L,
  masc = lorenz_masc$L
)

# Calcular índice de Gini
gini_fem <- round(Gini(dados$taxa_ver_fem), 3)
gini_masc <- round(Gini(dados$taxa_ver_masc), 3)

# Criar rótulos com índices Gini para legenda
labels_genero <- c(
  paste0("taxa de vereadora (Gini = ", gini_fem, ")"),
  paste0("taxa de vereador (Gini = ", gini_masc, ")")
)

# Transformar para formato longo
df_lorenz_long <- pivot_longer(df_lorenz, 
                               cols = c(fem, masc), 
                               names_to = "Genero", 
                               values_to = "Valor")

# Ajustar fator com rótulos com Gini
df_lorenz_long$Genero <- factor(df_lorenz_long$Genero,
                                levels = c("fem", "masc"),
                                labels = labels_genero)

# Plotar
ggplot(df_lorenz_long, aes(x = p, y = Valor, color = Genero, group = Genero)) +
  geom_line(size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "",
    x = "Percentil dos municípios ordenados pela taxa por 1000 habitantes crescente",
    y = "Taxa de vereadores e vereadoras por 1000 habitantes",
    color = ""
  ) +
  scale_color_manual(values = setNames(c("red", "black"), labels_genero)) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  )

# Imprimir no console
cat("Índice de Gini - taxa de vereadora:", gini_fem, "\n")
cat("Índice de Gini - taxa de vereador:", gini_masc, "\n")
