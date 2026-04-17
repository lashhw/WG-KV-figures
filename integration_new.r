library(tidyverse)
library(patchwork)

num_kv_heads <- 8
num_hidden_layers <- 32
bytes_per_token <- 256

memory_usage_scale <- num_kv_heads * num_hidden_layers * bytes_per_token / 1024 / 1024 / 1024
memory_access_scale <- num_kv_heads * num_hidden_layers * bytes_per_token / 1024 / 1024 / 1024

data <- read_csv("data/integration_new.csv", show_col_types = FALSE) %>%
  mutate(method = fct_inorder(method))

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

  ggplot(plot_data, aes(x = method, y = value, fill = component)) +
    geom_col(width = 0.8, position = position_stack(reverse = TRUE)) +
    geom_text(
      data = reduction_labels,
      aes(x = method, y = total_value, label = label),
      vjust = -0.6,
      size = 4.2,
      colour = "black",
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      breaks = c("top", "bottom"),
      labels = legend_labels,
      values = c(
        "bottom" = "#DAE8FC",
        "top" = "#F8CECC"
      )
    ) +
    scale_x_discrete(labels = \(x) str_wrap(x, width = 14)) +
    scale_y_continuous(
      labels = scales::label_comma(),
      expand = expansion(mult = c(0, 0.14))
    ) +
    guides(
      fill = guide_legend(reverse = FALSE, nrow = 2, byrow = TRUE)
    ) +
    labs(
      title = title,
      x = NULL,
      y = y_label,
      fill = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 13, colour = "black"),
      axis.title.y = element_text(size = 13, colour = "black"),
      axis.text.x = element_text(size = 11, colour = "black"),
      axis.text.y = element_text(size = 11, colour = "black"),
      legend.position = "top",
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
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
  title = "Decode-Time\nMemory Access",
  bottom_col = selected_kvs,
  top_col = key_landmarks,
  y_label = "Per-Query KV Cache Read (GB)",
  legend_labels = c("Key Landmarks", "Selected KVs"),
  value_scale = memory_access_scale
)

attention_latency_plot <- make_stacked_plot(
  data = data,
  title = "Decode-Time\nAttention Latency",
  bottom_col = attention_time,
  top_col = select_time,
  y_label = "Latency (ms)",
  legend_labels = c("KV Selection", "Attention on Selected KVs")
)

task_accuracy_plot <- ggplot(data, aes(x = method, y = score, fill = method)) +
  geom_col(width = 0.8) +
  scale_fill_manual(
    values = c(
      "Quest" = "#D5E8D4",
      "Quest with WG-KV" = "#FFF2CC"
    ),
    guide = "none"
  ) +
  scale_x_discrete(labels = \(x) str_wrap(x, width = 14)) +
  labs(
    title = "Task Accuracy",
    x = NULL,
    y = "Relative Score"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    axis.text.x = element_text(size = 11, colour = "black"),
    axis.text.y = element_text(size = 11, colour = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

fig <- memory_usage_plot + memory_access_plot +
  attention_latency_plot + task_accuracy_plot +
  plot_layout(ncol = 4)

ggsave("integration_new.pdf", fig, width = 9, height = 4.2, units = "in")
