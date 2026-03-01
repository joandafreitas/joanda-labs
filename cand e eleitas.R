# Carregar pacotes
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Função para MAD (desvio absoluto mediano sem escala)
mad_robusta <- function(x) mad(x, constant = 1, na.rm = TRUE)

# Calcular mediana e MAD por região
resumo <- database_candidatos_candidatas %>%
  group_by(reg) %>%
  summarise(
    mediana_cand_fem = median(`% cand_fem`, na.rm = TRUE),
    mad_cand_fem = mad_robusta(`% cand_fem`),
    mediana_eleitas = median(`%eleitas`, na.rm = TRUE),
    mad_eleitas = mad_robusta(`%eleitas`)
  ) %>%
  pivot_longer(
    cols = -reg,
    names_to = c(".value", "variavel"),
    names_pattern = "(mediana|mad)_(.*)"
  ) %>%
  mutate(
    reg = str_to_title(reg),
    variavel = recode(variavel,
                      cand_fem = "% candidatas",
                      eleitas = "% eleitas"),
    variavel = factor(variavel, levels = c("% candidatas", "% eleitas"))
  )

# Paleta de tons de cinza
cores_cinza <- c("gray40", "gray60")

# Criar gráfico
ggplot(resumo, aes(x = reg, y = mediana, fill = variavel)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mediana - mad, ymax = mediana + mad),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0(round(mediana, 1), "%"), y = mediana + mad + 1),
    position = position_dodge(width = 0.8),
    size = 3.2,
    vjust = -0.6
  ) +
  scale_fill_manual(values = cores_cinza) +
  labs(
    title = "",
    x = "Região",
    y = "Percentual mediano das candidatas e eleitas (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    legend.title = element_text(family = "sans", face = "bold", size = 9),
    legend.text = element_text(size = 9),
    legend.key.size = unit(1, "lines"),  # diminuído
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_text(size = 9)
  )
