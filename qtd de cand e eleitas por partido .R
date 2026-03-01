library(dplyr)
library(tidyr)
library(ggplot2)

# Agrupar e somar por partido
dados_agrupados <- Tabela_cand_e_eleitas_por_partido %>%
  group_by(partido) %>%
  summarise(
    candidatas = sum(`qtd de cand`),
    eleitas = sum(`qtd de eleitas`)
  ) %>%
  ungroup()

# Transformar para formato longo
dados_long <- dados_agrupados %>%
  pivot_longer(cols = c("candidatas", "eleitas"),
               names_to = "tipo",
               values_to = "quantidade") %>%
  mutate(tipo = recode(tipo,
                       candidatas = "Candidatas",
                       eleitas = "Eleitas"))

ggplot(dados_long, aes(x = reorder(partido, -quantidade), y = quantidade, color = tipo, group = tipo)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = quantidade, vjust = ifelse(tipo == "Eleitas", 1.7, -2)),
            size = 3,
            color = "black",
            show.legend = FALSE) +
  scale_color_manual(values = c("Candidatas" = "grey20", "Eleitas" = "firebrick3")) +
  labs(
    title = "",
    x = "Partido",
    y = "Quantidade",
    color = NULL
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "plain"),
    axis.text.y = element_text(size = 10, face = "plain"),
    axis.title.x = element_text(size = 9, face = "plain"),
    axis.title.y = element_text(size = 9, face = "plain"),
    plot.title = element_text(size = 9, face = "plain", hjust = 0.9),
    legend.position = "top",
    legend.text = element_text(size = 10, face = "plain")
  ) +
  ylim(min(dados_long$quantidade)*0.8, max(dados_long$quantidade) * 1.3)

