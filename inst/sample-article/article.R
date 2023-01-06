library(plotly)
library(ggplot2)
library(palmerpenguins)

p <- penguins %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm,
             color = species)) +
  geom_point()
ggplotly(p)

