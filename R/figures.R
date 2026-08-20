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

plot_births_by_day <- function() {
  holidays <- tribble(
    ~month, ~dom, ~label,
    1,  1,  "New Year's Day",
    7,  4,  "Independence Day",
    11, 11, "Veterans Day",
    12, 25, "Christmas Day",
    12, 31, "New Year's Eve"
  ) |>
    mutate(date = as.Date(sprintf("2001-%02d-%02d", month, dom)),
           doy  = as.integer(format(date, "%j")))

  read_csv("data/US_births_2000-2014_SSA.csv", show_col_types = FALSE) |>
    filter(!(month == 2 & date_of_month == 29)) |>
    group_by(month, date_of_month) |>
    summarise(avg = mean(births), .groups = "drop") |>
    mutate(date = as.Date(sprintf("2001-%02d-%02d", month, date_of_month)),
           doy  = as.integer(format(date, "%j"))) |>
    ggplot(aes(doy, avg)) +
    geom_line(color = "grey60", linewidth = 0.4) +
    geom_point(data = \(d) semi_join(d, holidays, by = "doy"),
               color = "#c0392b", size = 2) +
    geom_text(data = \(d) semi_join(d, holidays, by = "doy") |>
                left_join(holidays |> select(doy, label), by = "doy"),
              aes(label = label), vjust = -0.7, size = 2.8,
              color = "#c0392b", hjust = 0.5) +
    scale_x_continuous(
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
      labels = month.abb
    ) +
    labs(x = NULL, y = "Average births per day")
}
