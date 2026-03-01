# Carregar pacotes
library(dplyr)
library(writexl)
library(ggplot2)
library(forcats)

# 1. Agrupar por região e escolaridade
identidade_gen_por_regiao <- database_candidatas_idg_os %>%
  group_by(reg, id_gen) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(reg, desc(n))

# 2. Exportar para Excel
write_xlsx(
  list(identidadedegeneroporregiao = identidade_genero_por_regiao),
  path = "C:\\Users\\joand\\Desktop\\Dissertação em construção\\Pré-banca\\Terceiro produto\\id_gen_regiao.xlsx"
)

# 3. Filtrar apenas a região "agreste"
identidade_gen_agreste <- identidade_gen_por_regiao %>%
  filter(reg == "mata") %>%
  mutate(id_gen = factor(id_gen, levels = unique(id_gen)))

# 4. Gráfico só para a região agreste
ggplot(identidade_gen_agreste, aes(x = id_gen, y = n, fill = id_gen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.3), width = 0.5) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.8),
    hjust = -0.3,   # números fora da barra, à direita
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "",
    x = "Identidade de gênero",
    y = "Número de candidatas",
    fill = "Identidade de gênero"
  ) +
  scale_fill_grey(start = 0.3, end = 0.8) +  # ajuste a escala entre 0.3 e 0.8 para tons médios e claros
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9),
    legend.position = "right",  
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)
  ) +
  expand_limits(y = max(identidade_gen_agreste$n) * 1.15)