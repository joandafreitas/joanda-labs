library(dplyr)
library(ggplot2)
library(tidyr)


# Função para MAD (desvio absoluto mediano sem escala)
mad_robusta <- function(x) mad(x, constant = 1, na.rm = TRUE)

# Calcular mediana e mad por região e variável
resumo <- Dados_quantitativos_femininos_e_masculinos_com_as_variavéis_construídas %>%
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
    variavel = recode(variavel,
                      cand_fem = "% candidatas",
                      eleitas = "% eleitas")
  )

# Paleta cinza
cores_cinza <- c("#666666", "#AAAAAA")

# Plotar
ggplot(resumo, aes(x = reg, y = mediana, fill = variavel)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mediana - mad, ymax = mediana + mad),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "black") +
  geom_text(aes(label = round(mediana, 1), y = mediana + mad + 1),
            position = position_dodge(width = 0.8),
            size = 3.5) +
  scale_fill_manual(values = cores_cinza) +
  labs(
    title = "Candidatas e eleitas por região paraibana",
    x = "Região",
    y = "Percentual mediano e MAD das candidatas e eleitas (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_text(size = 9)
  )
