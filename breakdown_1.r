library(tidyverse)

data <- read_csv("data/breakdown_1.csv") %>%
  mutate(
    stage = fct_inorder(stage),
    tokens = fct_rev(fct_inorder(tokens)),
    component = fct_inorder(component)
  )

fig <- ggplot(data, aes(x = latency, y = tokens, fill = component, colour = component)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = data %>% filter(component == "Attention"),
    aes(
      x = 0.98,
      y = tokens,
      label = scales::percent(latency, accuracy = 1)
    ),
    colour = "#B85450",
    size = 5.5,
    inherit.aes = FALSE,
    hjust = 1
  ) +
  facet_grid(. ~ stage) +
  scale_fill_manual(
    values = c("Non-Attention" = "#DAE8FC", "Attention" = "#F8CECC"),
    labels = c("Non-Attention" = "Non-Attention (FFN, QKV Proj, etc.)", "Attention" = "Attention"),
  ) +
  scale_colour_manual(
    values = c("Non-Attention" = "#6C8EBF", "Attention" = "#B85450"),
    labels = c("Non-Attention" = "Non-Attention (FFN, QKV Proj, etc.)", "Attention" = "Attention"),
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0", "0.25", "0.5", "0.75", "1"),
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    colour = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(x = "Normalized Time", y = "Sequence Length") +
  theme_minimal(base_size = 18) +
  theme(
    strip.text = element_text(size = 21, face = "bold"),
    strip.clip = "off",
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    panel.grid.major.y = element_blank(),
  )

ggsave("breakdown_1.pdf", fig, width = 6.7, height = 4.1, units = "in")
