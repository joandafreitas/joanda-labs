library(dplyr)
library(ggplot2)
library(forcats)
library(writexl)

# 1. Agregar dados por região e cor/raça
corraça_por_regiao <- database_candidatas %>%
  group_by(reg, corraça) %>%
  summarise(n = n(), .groups = "drop") 

# 2. Exportar para Excel
write_xlsx(
  list(CorRacaPorRegiao = corraça_por_regiao),
  path = "C:\\Users\\joand\\Desktop\\Dissertação em construção\\Pré-banca\\Terceiro produto\\cor_raca_regiao.xlsx"
)

# 3. Filtrar apenas a região desejada (ex: agreste)
corraça_agreste <- corraça_por_regiao %>%
  filter(reg == "agreste") %>%
  mutate(corraça = fct_reorder(corraça, n, .desc = TRUE))

# 4. Gráfico apenas da região agreste
ggplot(corraça_agreste, aes(x = corraça, y = n, fill = corraça)) +
  geom_col(show.legend = TRUE) +
  geom_text(
    aes(label = n),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  labs(
    title = "",
    x = "Cor/raça",
    y = "Número de candidatas",
    fill = "Cor/raça"
  ) +
  scale_fill_grey(start = 0.2, end = 0.7) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.position = "right"
  ) +
  expand_limits(y = max(corraça_agreste$n) * 1.15)
