library(dplyr)
library(ggplot2)
library(writexl)

# 1. Agrupar por região e estado civil
estado_civil_por_regiao <- database_candidatas %>%
  group_by(reg, eci) %>%
  summarise(n = n(), .groups = "drop")

# 2. Exportar para Excel (opcional)
write_xlsx(
  list(EstadoCivilPorRegiao = estado_civil_por_regiao),
  path = "C:\\Users\\joand\\Desktop\\Dissertação em construção\\Pré-banca\\Terceiro produto\\estado_civil_regiao.xlsx"
)

# 3. Filtrar apenas a região "sertão"
estado_civil_sertao <- estado_civil_por_regiao %>%
  filter(reg == "agreste") %>%
  mutate(eci = factor(eci, levels = unique(eci)))

# 4. Gráfico apenas para o Sertão com legenda lateral
ggplot(estado_civil_sertao, aes(x = eci, y = n, fill = eci)) +
  geom_col(show.legend = TRUE) +
  geom_text(
    aes(label = n),
    hjust = -0.15,
    size = 3
  ) +
  coord_flip() +
  labs(
    title = "",
    x = "Estado civil",
    y = "Número de candidatas",
    fill = "Estado civil"
  ) +
  scale_fill_grey(start = 0.1, end = 0.8) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)
  ) +
  expand_limits(y = max(estado_civil_sertao$n) * 1.15)
