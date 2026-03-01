# Carregar pacotes
library(dplyr)
library(writexl)
library(ggplot2)
library(forcats)

# 1. Agrupar por região e escolaridade
escolaridade_por_regiao <- database_candidatas %>%
  group_by(reg, esc) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(reg, desc(n))

# 2. Exportar para Excel
write_xlsx(
  list(EscolaridadePorRegiao = escolaridade_por_regiao),
  path = "C:\\Users\\joand\\Desktop\\Dissertação em construção\\Pré-banca\\Terceiro produto\\escolaridade_regiao.xlsx"
)

# 3. Filtrar apenas a região "agreste"
escolaridade_agreste <- escolaridade_por_regiao %>%
  filter(reg == "sertão") %>%
  mutate(esc = factor(esc, levels = unique(esc)))

# 4. Gráfico só para a região agreste
ggplot(escolaridade_agreste, aes(x = esc, y = n, fill = esc)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.8),
    hjust = -0.3,   # afastar números para fora da barra no gráfico horizontal
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "",
    x = "Escolaridade",
    y = "Número de candidatas",
    fill = "Escolaridade"
  ) +
  scale_fill_grey(start = 0.1, end = 0.8) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "right",  # legenda lateral
    legend.title = element_text(size = 8),  # tamanho do título da legenda
    legend.text = element_text(size = 7)     # tamanho do texto da legenda
  ) +
  expand_limits(y = max(escolaridade_agreste$n) * 1.15)