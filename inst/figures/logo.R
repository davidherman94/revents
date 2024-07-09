# Cargar las bibliotecas necesarias
library(ggplot2)
library(hexSticker)

# Definir las coordenadas para las tres líneas horizontales y los puntos
line_data <- data.frame(
  x_start = rep(2, 3),
  x_end = rep(8, 3),
  y = c(5, 4, 3)
)

points_data <- data.frame(
  x = c(3, 5, 7, 4, 6, 8, 3.5, 5.5, 7.5),
  y = c(5, 5, 5, 4, 4, 4, 3, 3, 3)
)

# Crear el gráfico con las tres líneas horizontales y los puntos
survival_plot <- ggplot() +
  geom_segment(data = line_data, aes(x = x_start, y = y, xend = x_end, yend = y), color = "black", size = 1) +
  geom_point(data = points_data, aes(x, y), color = "black", size = 2) +
  theme_void()

# Personalizar el sticker hexagonal
hexSticker::sticker(
  filename = "inst/figures/logo2.png",
  # Estética del subplot
  subplot = survival_plot,
  s_width = 1.0, s_height = .85,
  s_x = 1, s_y = .75,
  # Estética del nombre del paquete
  package = "revents",
  p_size = 24,
  p_color = "blue",
  # Estética del hexágono
  h_size = 1,
  h_fill = "yellow",
  h_color = "black"
) |> plot() # Previsualizar con plot()

