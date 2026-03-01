library(dplyr)
library(ggplot2)
library(forcats)

# Contar candidatas por partido na região do Agreste
dados_mata <- database_candidatas %>%
  filter(reg == "mata") %>%
  group_by(part) %>%
  summarise(n = n(), .groups = "drop")

# Garantir todos os partidos do banco original, preenchendo com 0 se necessário
partido_fed_por_regiao <- database_candidatas %>%
  select(part) %>%
  distinct() %>%
  left_join(dados_mata, by = "part") %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    part = fct_reorder(part, n, .desc = TRUE)
  )

# Gráfico
ggplot(partido_fed_por_regiao, aes(x = part, y = n)) +
  geom_line(aes(color = "Linha de tendência", group = 1), size = 1.2) +
  geom_point(aes(color = "Total de candidatas"), size = 2.3) +
  geom_text(aes(label = n), hjust = -0.4, size = 3.2, color = "black") +
  coord_flip() +
  labs(
    x = "Partido",
    y = "Número de candidatas",
    color = ""
  ) +
  scale_color_manual(
    values = c("Linha de tendência" = "gray70", "Total de candidatas" = "red")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.y = element_line(color = "gray90", size = 0.4),
    panel.grid.major.x = element_line(color = "gray80", size = 0.3),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.margin = margin(10, 40, 10, 10),
    legend.position = "top",
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, max(partido_fed_por_regiao$n) * 1.1)
  )
