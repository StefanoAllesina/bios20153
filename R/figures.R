# One function per figure, named plot_<thing>().
# All stochastic functions call set.seed(42) internally.
# Themes and palettes are set once here; do not add ad hoc theme() calls elsewhere.

source("R/utils.R")

theme_set(theme_classic(base_size = 12))

fibonacci_seq <- function(n = 36) {
  R <- numeric(n)
  R[1] <- 1
  R[2] <- 1
  for (t in 3:n) R[t] <- R[t - 1] + R[t - 2]
  tibble(t = 1:n, R = R)
}

plot_fibonacci <- function(n = 36) {
  fibonacci_seq(n) |>
    ggplot(aes(t, R)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(0, n, by = 6)) +
    labs(x = expression(t ~ "(months)"),
         y = expression(R[t] ~ "(pairs of rabbits)"))
}

plot_fibonacci_log <- function(n = 36) {
  fibonacci_seq(n) |>
    ggplot(aes(t, log(R))) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(0, n, by = 6)) +
    labs(x = expression(t ~ "(months)"),
         y = expression(log ~ R[t]))
}
