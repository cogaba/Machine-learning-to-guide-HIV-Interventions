################################################################################
# S3_Fig_flow_diagram.R
#
# Participant-flow diagram (S3 Fig): eligible sample (master_merged_all.xlsx,
# definitive HIV result, age 15+) to analytic sample (analytic_final.xlsx).
#
# Output: artifacts/figures/S3_Fig_flow_diagram.png and .tif
################################################################################

suppressMessages({ library(readxl); library(dplyr); library(ggplot2) })
source("00_config.R")
fig_dir <- file.path(artifact_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

master <- read_excel(data_path("master_merged_all.xlsx"), guess_max = 200000)
final  <- read_excel(data_path("analytic_final.xlsx"),    guess_max = 200000)

n_elig  <- sum(!is.na(master$hivstatusfinal) & master$age >= 15)
n_final <- nrow(final)
n_excl  <- n_elig - n_final
sex_counts <- final %>% group_by(sex) %>% summarise(n = n(), pos = sum(hivstatusfinal == 1), .groups = "drop")
m <- sex_counts %>% filter(sex == 0); f <- sex_counts %>% filter(sex == 1)
fmt <- function(x) format(x, big.mark = ",")
pct <- function(a, b) sprintf("%.1f%%", 100 * a / b)

boxes <- tribble(
  ~x, ~y, ~w, ~h, ~label,
  27, 88, 46, 16, sprintf("Eligible sample\nRespondents aged 15 years or older\nwith a definitive HIV test result\nn = %s", fmt(n_elig)),
  27, 54, 46, 16, sprintf("Analytic sample\nn = %s", fmt(n_final)),
  76, 71, 44, 20, sprintf("Excluded: missing data on the outcome\nor on one or more predictors,\nincluding responses coded\n\"don't know\" or \"no response\"\nn = %s (%s)", fmt(n_excl), pct(n_excl, n_elig)),
  25, 14, 38, 16, sprintf("Male\nn = %s\nHIV-positive: n = %s (%s)", fmt(m$n), fmt(m$pos), pct(m$pos, m$n)),
  77, 14, 38, 16, sprintf("Female\nn = %s\nHIV-positive: n = %s (%s)", fmt(f$n), fmt(f$pos), pct(f$pos, f$n))
)
arrows <- tribble(~x, ~y, ~xend, ~yend,
  27, 80, 27, 62,   27, 71, 54, 71,   27, 46, 27, 29,   25, 29, 25, 22,   77, 29, 77, 22)
p <- ggplot() +
  geom_segment(data = arrows, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"), linewidth = 0.5) +
  annotate("segment", x = 25, xend = 77, y = 29, yend = 29, linewidth = 0.5) +
  geom_tile(data = boxes, aes(x = x, y = y, width = w, height = h), fill = "white", colour = "grey20", linewidth = 0.5) +
  geom_text(data = boxes, aes(x = x, y = y, label = label), size = 3.4, lineheight = 1.1) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) + theme_void()
ggsave(file.path(fig_dir, "S3_Fig_flow_diagram.png"), p, width = 9, height = 8, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "S3_Fig_flow_diagram.tif"), p, width = 9, height = 8, dpi = 300, bg = "white", compression = "lzw")
message(sprintf("Flow: %d eligible -> %d excluded -> %d analytic", n_elig, n_excl, n_final))
