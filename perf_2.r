library(tidyverse)

data <- read_csv("data/perf_2.csv") %>%
  filter(tokens %in% c("100K", "200K", "300K", "400K")) %>%
  mutate(
    tokens = fct_inorder(tokens),
    method = fct_relevel(method, "WG-KV", "Full"),
    component = fct_inorder(component)
  ) %>%
  group_by(tokens, method) %>%
  mutate(total_latency = sum(latency)) %>%
  ungroup()

reduction_labels <- data %>%
  distinct(tokens, method, total_latency) %>%
  group_by(tokens) %>%
  mutate(
    vanilla_latency = total_latency[method == "Full"],
    reduction = (total_latency - vanilla_latency) / vanilla_latency,
    label = if_else(
      method == "WG-KV",
      scales::label_percent(accuracy = 1)(reduction),
      ""
    )
  ) %>%
  ungroup()

fig <- ggplot(data, aes(x = latency, y = method, fill = component, colour = component)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = reduction_labels,
    aes(x = total_latency, y = method, label = label),
    hjust = -0.13,
    size = 6,
    inherit.aes = FALSE
  ) +
  facet_grid(tokens ~ ., switch = "y") +
  scale_fill_manual(
    values = c("Non-Attention" = "#DAE8FC", "Attention" = "#F8CECC"),
  ) +
  scale_colour_manual(
    values = c("Non-Attention" = "#6C8EBF", "Attention" = "#B85450"),
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    colour = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(
    title = "(b) Decode Latency",
    x = "TPOT (ms)",
    y = "Sequence Length",
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 21),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 23),
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 19, colour = "black"),
    strip.text.y = element_text(size = 19),
    strip.placement = "outside",
    panel.spacing = unit(0, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
  )

ggsave("perf_2.pdf", fig, width = 5.1, height = 5.2, units = "in")
