# Carregar pacotes
library(dplyr)
library(writexl)
library(ggplot2)
library(forcats)

# 1. Agrupar por região e escolaridade
orientação_sexual_região <- database_candidatas_idg_os %>%
  group_by(reg, ori_sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(reg, desc(n))

# 2. Exportar para Excel
write_xlsx(
  list(orientacaosexualporporregiao = orientação_sexual_região),
  path = "C:\\Users\\joand\\Desktop\\Dissertação em construção\\Pré-banca\\Terceiro produto\\id_gen_regiao.xlsx"
)

# 3. Filtrar apenas a região "agreste"
orientação_sexual_região_sertão <- orientação_sexual_região %>%
  filter(reg == "sertão") %>%
  mutate(id_gen = factor(ori_sex, levels = unique(ori_sex)))

# 4. Gráfico só para a região agreste
ggplot(orientação_sexual_região_sertão, aes(x = ori_sex, y = n, fill = ori_sex)) +
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
    x = "Orientação sexual",
    y = "Número de candidatas",
    fill = "Orientação sexual"
  ) +
  scale_fill_grey(start = 0.3, end = 0.8) +  # ajuste a escala entre 0.3 e 0.8 para tons médios e claros
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.45),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9),
    legend.position = "right",  
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)
  ) +
  expand_limits(y = max(orientação_sexual_região_sertão$n) * 1.15)