library(tidyverse)
library(patchwork)

method_priority <- c("Quest Only", "WG-KV + Quest")
method_labels <- c(
  "Quest Only" = "Quest",
  "WG-KV + Quest" = "Quest with WG-KV"
)

data <- read_csv("data/integration.csv", show_col_types = FALSE) %>%
  mutate(method = factor(method, levels = method_priority))

num_kv_heads <- 8
num_hidden_layers <- 32
bytes_per_token <- 256
page_size <- 8
target_kv_attended <- 1024

multiplier <- (num_kv_heads * num_hidden_layers * bytes_per_token) / 1024 / 1024

summary_data <- data %>%
  group_by(panel_title, method) %>%
  mutate(max_kv = max(kv_attended, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(kv_attended == target_kv_attended) %>%
  mutate(
    key_size = max_kv * multiplier,
    key_landmarks = key_size / page_size,
    stored_kvs = key_size * 2,
    selected_kvs = kv_attended * multiplier,
    method_label = recode(as.character(method), !!!method_labels)
  ) %>%
  select(panel_title, y_label, method, method_label, score, key_landmarks, stored_kvs, selected_kvs)

expected_rows <- data %>%
  distinct(panel_title, method) %>%
  nrow()

if (nrow(summary_data) != expected_rows) {
  stop(glue::glue("Expected {expected_rows} rows at kv_attended == {target_kv_attended}, found {nrow(summary_data)}."))
}

build_component_plot <- function(panel_data, components, colors, y_title, plot_title) {
  plot_data <- panel_data %>%
    select(method_label, all_of(components)) %>%
    pivot_longer(
      cols = all_of(components),
      names_to = "component",
      values_to = "cache_size"
    ) %>%
    mutate(
      method_label = factor(method_label, levels = unname(method_labels)),
      component = recode(
        component,
        "key_landmarks" = "Key Landmarks",
        "stored_kvs" = "Stored KVs",
        "selected_kvs" = "Selected KVs"
      ),
      component = factor(component, levels = c("Key Landmarks", setdiff(names(colors), "Key Landmarks")))
    )

  total_data <- plot_data %>%
    group_by(method_label) %>%
    summarise(total_cache_size = sum(cache_size), .groups = "drop")

  quest_total <- total_data %>%
    filter(method_label == "Quest") %>%
    pull(total_cache_size) %>%
    first()

  annotation_data <- total_data %>%
    filter(method_label == "Quest with WG-KV") %>%
    mutate(
      label = paste0("-", round((1 - total_cache_size / quest_total) * 100), "%"),
      y = total_cache_size + max(total_data$total_cache_size) * 0.05
    )

  ggplot(
    plot_data,
    aes(x = method_label, y = cache_size, fill = component)
  ) +
    geom_col(
      width = 0.7,
      colour = "white",
      linewidth = 0.5
    ) +
    scale_fill_manual(
      values = colors,
      breaks = names(colors)
    ) +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ",", accuracy = 1),
      expand = expansion(mult = c(0, 0.12))
    ) +
    geom_text(
      data = annotation_data,
      aes(x = method_label, y = y, label = label),
      inherit.aes = FALSE,
      colour = "#B85450",
      fontface = "bold",
      size = 4.2
    ) +
    labs(
      x = NULL,
      y = y_title,
      fill = NULL,
      title = plot_title
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "top",
      axis.text.x = element_text(size = 11, colour = "black"),
      axis.text.y = element_text(size = 11, colour = "black"),
      axis.title.y = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
}

build_score_plot <- function(panel_data) {
  plot_data <- panel_data %>%
    mutate(method_label = factor(method_label, levels = unname(method_labels)))

  y_title <- plot_data %>%
    distinct(y_label) %>%
    pull(y_label) %>%
    first()

  ggplot(
    plot_data,
    aes(x = method_label, y = score, fill = method_label)
  ) +
    geom_col(
      width = 0.7,
      colour = "white",
      linewidth = 0.5
    ) +
    scale_fill_manual(
      values = c(
        "Quest" = "#82B366",
        "Quest with WG-KV" = "#D79B00"
      ),
      guide = "none"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = NULL,
      y = y_title,
      title = "Task Accuracy"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 11, colour = "black"),
      axis.text.y = element_text(size = 11, colour = "black"),
      axis.title.y = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
}

save_combined_figures <- function(summary_data, output_dir) {
  dir.create(output_dir, showWarnings = FALSE)

  panel_titles <- summary_data %>%
    distinct(panel_title) %>%
    pull(panel_title)

  walk(panel_titles, \(panel_label) {
    panel_data <- summary_data %>%
      filter(panel_title == panel_label)

    cache_plot <- build_component_plot(
      panel_data = panel_data,
      components = c("key_landmarks", "stored_kvs"),
      colors = c(
        "Key Landmarks" = "#F8CECC",
        "Stored KVs" = "#DAE8FC"
      ),
      y_title = "KV Cache Size",
      plot_title = "KV Cache Size"
    )

    read_plot <- build_component_plot(
      panel_data = panel_data,
      components = c("key_landmarks", "selected_kvs"),
      colors = c(
        "Key Landmarks" = "#F8CECC",
        "Selected KVs" = "#DAE8FC"
      ),
      y_title = "Per-Query KV Cache Read",
      plot_title = "Per-Query KV Cache Read"
    )

    score_plot <- build_score_plot(panel_data)

    combined_plot <- (cache_plot | read_plot | score_plot) +
      plot_annotation(title = panel_label) &
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )

    file_stem <- panel_label %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_replace_all("^_+|_+$", "")

    ggsave(
      filename = file.path(output_dir, paste0(file_stem, ".pdf")),
      plot = combined_plot,
      width = 12.6,
      height = 4.8,
      units = "in"
    )
  })
}

save_combined_figures(
  summary_data = summary_data,
  output_dir = "integration_new_combined_figures"
)
