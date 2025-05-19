library(hexSticker)
library(ggplot2)

# Diseño más equilibrado y limpio
p <- ggplot() +
  annotate("text", x = 0, y = 0.2, label = "InteRculturales", 
           size = 14.5, color = "#EB90F2", family = "sans", fontface = "bold") +
  annotate("text", x = 0, y = -0.4, label = "Centro de Estudios Interculturales e Indígenas", 
           size = 4.8, color = "#666666", family = "sans") +
  xlim(-1, 1) + ylim(-1, 1) +
  theme_void()

# Sticker ajustado
sticker(
  subplot = p,
  package = "",
  s_x = 1, s_y = 1, s_width = 1.4,
  h_fill = "#FFFFFF", h_color = "#EB90F2",
  dpi = 300,
  filename = "sticker_ciir_rosa_v2.png"
)

