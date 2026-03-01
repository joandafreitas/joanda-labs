# Carregar pacotes
library(dplyr)
library(ggplot2)
library(writexl)
library(forcats)

# 1. Criar faixas etárias ajustadas
database_candidatas <- database_candidatas %>%
  mutate(
    faixa_idade = case_when(
      ida >= 18 & ida <= 25 ~ "18-25",
      ida >= 26 & ida <= 35 ~ "26-35",
      ida >= 36 & ida <= 45 ~ "36-45",
      ida >= 46 & ida <= 55 ~ "46-55",
      ida >= 56 & ida <= 69 ~ "56-69",
      ida >= 70             ~ "70+",
      TRUE                  ~ "Desconhecida"
    )
  )

# 2. Agrupar por região e faixa etária
ida_por_regiao <- database_candidatas %>%
  group_by(reg, faixa_idade) %>%
  summarise(n = n(), .groups = "drop") %>%
  ungroup() %>%
  group_by(reg) %>%
  mutate(total_regiao = sum(n)) %>%
  ungroup() %>%
  mutate(
    reg = fct_reorder(reg, total_regiao, .desc = TRUE),
    faixa_idade = factor(
      faixa_idade,
      levels = c("18-25", "26-35", "36-45", "46-55", "56-69", "70+", "Desconhecida")
    )
  ) %>%
  arrange(reg, faixa_idade)

# 3. Exportar tabela para Excel (opcional)
write_xlsx(
  list(IdaPorRegiao = ida_por_regiao),
  path = "C:\\Users\\joand\\Desktop\\Dissertação em construção\\Pré-banca\\Terceiro produto\\ida_faixa_regiao.xlsx"
)

# 4. Filtrar apenas a região Agreste
ida_agreste <- ida_por_regiao %>%
  filter(reg == "sertão")

# 5. Gráfico da distribuição de faixas etárias da região Agreste (barras horizontais com legenda)
ggplot(ida_agreste, aes(x = faixa_idade, y = n, fill = faixa_idade)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  scale_fill_grey(start = 0.2, end = 0.8) +
  labs(
    title = "",
    x = "Faixa etária",
    y = "Número de candidatas",
    fill = "Faixa etária"
  ) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  ) +
  expand_limits(y = max(ida_agreste$n) * 1.15)