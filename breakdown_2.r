library(tidyverse)

data <- read_csv("data/breakdown_2.csv") %>%
  mutate(
    tokens = fct_rev(fct_inorder(tokens)),
    component = fct_inorder(component)
  )

fig <- ggplot(data, aes(x = memory, y = tokens, fill = component, colour = component)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(
    values = c("Model Weights" = "#DAE8FC", "KV Cache" = "#F8CECC"),
  ) +
  scale_colour_manual(
    values = c("Model Weights" = "#6C8EBF", "KV Cache" = "#B85450"),
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    colour = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(x = "Size (GB)", y = NULL, title = "(c) Memory Usage") +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
    strip.clip = "off",
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
  )

ggsave("breakdown_2.pdf", fig, width = 2.9, height = 3.92, units = "in")
