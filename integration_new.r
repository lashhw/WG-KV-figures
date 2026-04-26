library(tidyverse)
library(patchwork)

method_priority <- c("Quest Only", "WG-KV + Quest")

data <- read_csv("data/integration.csv") %>%
  mutate(
    method = fct_relevel(method, method_priority),
    score = case_when(
      panel_title == "NarrativeQA" ~ score * (100 / 3),
      panel_title == "InfiniteBench Sum" | panel_title == "Multi-LexSum" ~ score * 100,
      TRUE ~ score
    )
  )

panel_info <- data %>%
  distinct(panel_title, y_label)

fig_list <- map(seq_len(nrow(panel_info)), \(idx) {
  panel_label <- panel_info$panel_title[[idx]]
  axis_label <- panel_info$y_label[[idx]]

  panel_data <- data %>%
    filter(panel_title == panel_label, y_label == axis_label, kv_attended %in% c(1024, 2048, 4096)) %>%
    mutate(kv_attended = factor(kv_attended, levels = c(1024, 2048, 4096)))

  plot <- ggplot(panel_data, aes(x = kv_attended, y = score, fill = method, colour = method)) +
    geom_col(
      position = position_dodge(width = 0.74),
      width = 0.5,
      linewidth = 0.5
    )

  plot +
    scale_fill_manual(
      breaks = method_priority,
      labels = c("Quest", "Quest with WG-KV"),
      values = c(
        "Quest Only" = "#D5E8D4",
        "WG-KV + Quest" = "#FFF2CC"
      )
    ) +
    scale_colour_manual(
      values = c(
        "Quest Only" = "#82B366",
        "WG-KV + Quest" = "#D79B00"
      )
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(
          linewidth = 0.5,
          colour = c("#82B366", "#D79B00")
        ),
        keywidth = 1.7
      ),
      colour = "none"
    ) +
    labs(
      x = "# KV Selected / Query",
      y = axis_label,
      title = panel_label
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      legend.title = element_blank(),
      legend.text = element_text(size = 15),
      axis.text.x = element_text(size = 10, colour = "black"),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      panel.grid.major.x = element_blank()
    )
})

fig <- wrap_plots(fig_list, ncol = 7, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("integration_new.pdf", fig, width = 14.0, height = 4.7, units = "in")
