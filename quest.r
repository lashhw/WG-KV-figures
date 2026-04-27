library(tidyverse)
library(patchwork)

num_kv_heads <- 8
num_hidden_layers <- 32
bytes_per_token <- 256

memory_usage_scale <- num_kv_heads * num_hidden_layers * bytes_per_token / 1024 / 1024 / 1024
memory_access_scale <- num_kv_heads * num_hidden_layers * bytes_per_token / 1024 / 1024 / 1024
method_levels <- c("Quest", "Quest with WG-KV")

data <- read_csv("data/quest.csv", show_col_types = FALSE) %>%
  mutate(method = factor(method, levels = method_levels))

task_accuracy_score <- read_csv("data/integration.csv", show_col_types = FALSE) %>%
  filter(kv_attended == 2048, method %in% c("Quest Only", "WG-KV + Quest")) %>%
  select(panel_title, method, score) %>%
  pivot_wider(names_from = method, values_from = score) %>%
  summarise(score = mean(`WG-KV + Quest` / `Quest Only`)) %>%
  pull(score)

task_accuracy_data <- tibble(
  method = factor(c("Quest", "Quest with WG-KV"), levels = method_levels),
  score = c(1.0, task_accuracy_score)
)

make_stacked_plot <- function(data, title, bottom_col, top_col, y_label, legend_labels, value_scale = 1) {
  plot_data <- data %>%
    select(method, bottom = {{ bottom_col }}, top = {{ top_col }}) %>%
    pivot_longer(
      cols = c(bottom, top),
      names_to = "component",
      values_to = "value"
    ) %>%
    mutate(
      component = factor(component, levels = c("bottom", "top")),
      value = value * value_scale
    )

  reduction_labels <- plot_data %>%
    group_by(method) %>%
    summarise(total_value = sum(value), .groups = "drop") %>%
    mutate(
      baseline_value = total_value[method == "Quest"][1],
      reduction = (total_value - baseline_value) / baseline_value,
      label = if_else(
        method == "Quest with WG-KV" & is.finite(reduction),
        scales::label_percent(accuracy = 1)(reduction),
        ""
      )
    )

  ggplot(plot_data, aes(y = method, x = value, fill = component, colour = component)) +
    geom_col(width = 0.6, position = position_stack(reverse = TRUE), linewidth = 0.5) +
    geom_text(
      data = reduction_labels,
      aes(y = method, x = total_value, label = label),
      hjust = -0.2,
      size = 4.2,
      colour = "black",
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      breaks = c("bottom", "top"),
      labels = rev(legend_labels),
      values = c(
        "bottom" = "#DAE8FC",
        "top" = "#F8CECC"
      )
    ) +
    scale_colour_manual(
      values = c(
        "bottom" = "#6C8EBF",
        "top" = "#B85450"
      )
    ) +
    scale_y_discrete(
      limits = rev(method_levels),
      labels = \(x) str_wrap(x, width = 14)
    ) +
    scale_x_continuous(
      labels = scales::label_comma(),
      expand = expansion(mult = c(0, 0.14))
    ) +
    guides(
      fill = guide_legend(
        reverse = FALSE,
        nrow = 1,
        byrow = TRUE,
        override.aes = list(
          linewidth = 0.5,
          colour = c("#6C8EBF", "#B85450")
        )
      ),
      colour = "none"
    ) +
    labs(
      title = title,
      x = y_label,
      y = NULL,
      fill = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 13, colour = "black"),
      axis.title.y = element_text(size = 13, colour = "black"),
      axis.text.x = element_text(size = 11, colour = "black"),
      axis.text.y = element_text(size = 13, colour = "black"),
      legend.position = "top",
      legend.text = element_text(size = 13),
      legend.key.width = grid::unit(0.8, "lines"),
      legend.key.height = grid::unit(0.6, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(0, 100, 0, 0)
    )
}

memory_usage_plot <- make_stacked_plot(
  data = data,
  title = "Memory Usage",
  bottom_col = stored_kvs,
  top_col = key_landmarks,
  y_label = "KV Cache Size (GB)",
  legend_labels = c("Key Landmarks", "Stored KVs"),
  value_scale = memory_usage_scale
)

memory_access_plot <- make_stacked_plot(
  data = data,
  title = "Decode-Time Memory Access",
  bottom_col = selected_kvs,
  top_col = key_landmarks,
  y_label = "Per-Query KV Cache Read (GB)",
  legend_labels = c("Key Landmarks", "Selected KVs"),
  value_scale = memory_access_scale
)

attention_latency_plot <- make_stacked_plot(
  data = data,
  title = "Decode-Time Attention Latency",
  bottom_col = attention_time,
  top_col = select_time,
  y_label = "Latency (us)",
  legend_labels = c("Select KVs", "Attn on Selected KVs")
)

task_accuracy_plot <- ggplot(task_accuracy_data, aes(y = method, x = score, fill = method, colour = method)) +
  geom_col(width = 0.6, linewidth = 0.5) +
  scale_fill_manual(
    values = c(
      "Quest" = "#D5E8D4",
      "Quest with WG-KV" = "#FFF2CC"
    ),
    guide = "none"
  ) +
  scale_colour_manual(
    values = c(
      "Quest" = "#82B366",
      "Quest with WG-KV" = "#D79B00"
    ),
    guide = "none"
  ) +
  scale_y_discrete(
    limits = rev(method_levels),
    labels = \(x) str_wrap(x, width = 14)
  ) +
  labs(
    title = "Task Accuracy",
    x = "Relative Score",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    axis.text.x = element_text(size = 11, colour = "black"),
    axis.text.y = element_text(size = 13, colour = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(0, 100, 0, 0)
  )

fig <- memory_usage_plot + memory_access_plot +
  attention_latency_plot + task_accuracy_plot +
  plot_layout(ncol = 1)

ggsave("quest.pdf", fig, width = 5, height = 8, units = "in")
