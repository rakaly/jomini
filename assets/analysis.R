library(tidyverse)
library(scales)

theme_set(theme_minimal(base_size = 11))

byte_rate <- function(x) {
  number_bytes(x, symbol = "MB", units = "si")
}

throughput_label <- function(mib_s) {
  paste0(number(mib_s, accuracy = 1), " MiB/s")
}

save_plot <- function(filename, plot, width = 7, height = 4) {
  ggsave(filename, plot, width = width, height = height, dpi = 180)
}

hero_data <- read_csv("jomini-benchmarks.csv" , show_col_types = FALSE) |>
  filter(throughput_type == "bytes") |>
  mutate(
    workload = case_when(
      group == "binary" & `function` == "reader-token-kind" ~ "Binary",
      group == "text" & `function` == "reader" ~ "Text",
      TRUE ~ NA_character_
    ),
    game_id = value,
    game = case_when(
      game_id == "eu4" ~ "EU4",
      game_id == "ck3" ~ "CK3",
      game_id %in% c("v3", "vic3") ~ "Victoria 3",
      TRUE ~ str_to_upper(game_id)
    ),
    sample_ns = sample_measured_value / iteration_count,
    throughput = throughput_num * 1e9 / sample_ns,
    mib_s = throughput / 1024 / 1024,
    input_mib = throughput_num / 1024 / 1024
  ) |>
  filter(!is.na(workload), !is.na(game_id), is.finite(throughput), throughput > 0) |>
  group_by(workload, game) |>
  summarize(
    throughput = median(throughput),
    mib_s = throughput / 1024 / 1024,
    input_mib = median(input_mib),
    .groups = "drop"
  ) |>
  mutate(
    workload = fct_reorder(workload, mib_s, .fun = median),
    game = fct_relevel(game, "EU4", "CK3", "Victoria 3"),
    label = throughput_label(mib_s)
  )

if (nrow(hero_data) == 0) {
  stop("No hero benchmark rows found in jomini-benchmarks.csv.")
}

hero_plot <- ggplot(hero_data, aes(workload, throughput, fill = game)) +
  geom_col(position = position_dodge2(width = 0.76, preserve = "single"), width = 0.68) +
  geom_text(
    aes(label = label),
    position = position_dodge2(width = 0.76, preserve = "single"),
    hjust = -0.08,
    size = 3.1
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = byte_rate,
    breaks = pretty_breaks(7),
    expand = expansion(mult = c(0, 0.24))
  ) +
  scale_fill_manual(
    values = c("EU4" = "#245c8c", "CK3" = "#2f8f5b", "Victoria 3" = "#8a4fbf"),
    name = NULL
  ) +
  labs(
    title = "Parse Saves at Gigabytes per Second",
    x = NULL,
    y = "Throughput per second"
  ) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(6, 48, 6, 6)
  )

save_plot("jomini-bench-throughput.png", hero_plot, width = 7.6, height = 4.4)
