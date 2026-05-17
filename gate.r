library(tidyverse)

read_gate_matrix <- function(path, panel_label) {
  read_csv(path) %>%
    mutate(layer_index = row_number()) %>%
    pivot_longer(
      cols = starts_with("head_"),
      names_to = "head_index",
      values_to = "gate_value"
    ) %>%
    mutate(
      panel = panel_label,
      head_index = parse_number(head_index) + 1
    )
}

data <- bind_rows(
  read_gate_matrix("data/gate_1.csv", "(a) Code Summarization"),
  read_gate_matrix("data/gate_2.csv", "(b) HTML to TSV")
) %>%
  mutate(panel = fct_inorder(panel))

fig <- ggplot(data, aes(x = layer_index, y = head_index, fill = gate_value)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  facet_grid(. ~ panel, switch = "x") +
  scale_fill_viridis_c(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    option = "viridis",
  ) +
  scale_x_continuous(
    breaks = c(seq(1, 29, by = 4), 32),
    expand = expansion(mult = 0),
    position = "top"
  ) +
  scale_y_reverse(
    breaks = seq(1, 8, by = 1),
    expand = expansion(mult = 0)
  ) +
  guides(
    fill = guide_colorbar(
      barheight = grid::unit(12, "line"),
      barwidth = grid::unit(1, "line"),
      title.position = "right"
    )
  ) +
  labs(
    x = "Layer Index",
    y = "KV Head Index",
    fill = "Normalized KV Cache Size"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 20) +
  theme(
    strip.text.x = element_text(size = 20, face = "bold"),
    strip.placement = "outside",
    legend.position = "right",
    legend.title = element_text(size = 16, angle = 90, vjust = 0.5),
    legend.text = element_text(size = 13),
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 13, colour = "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank()
  )

ggsave("gate.pdf", fig, width = 17.0, height = 3.5, units = "in")
