library(tidyverse)
library(readr)
library(scales)

df <- read_csv("./jomini-benchmarks.csv")

df <- mutate(df,
             fn = `function`,
             throughput = iteration_count * throughput_num * 10^9 / sample_measured_value
)

byte_rate <- function(l) {
  scales::number_bytes(l, symbol = "MB", units = "si")
}

ggplot(df, aes(fn, throughput)) +
  stat_summary(position="dodge2", geom = "bar", fun = median, fill = "#9e9ac8") +
  scale_y_continuous(labels = byte_rate, breaks = pretty_breaks(10)) +
  expand_limits(y = 0) +
  xlab("File Format") +
  ylab("Throughput\n(per second)") +
  ggtitle("Jomini Parsing Throughput",
          subtitle = "Across text and binary formats") +
  guides(fill=guide_legend(title="Function"))

ggsave('jomini-bench-throughput.png', width = 4, height = 3, dpi = 180)
