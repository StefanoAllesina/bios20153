# One function per figure, named plot_<thing>().
# All stochastic functions call set.seed(42) internally.
# Themes and palettes are set once here; do not add ad hoc theme() calls elsewhere.

source("R/utils.R")

theme_set(theme_classic(base_size = 12))

# ---- helpers for life-table / Euler-Lotka ----

.read_us_life_table_2020 <- function() {
  readxl::read_xlsx("data/us_life_table_female_2020.xlsx", skip = 2,
    col_names = c("age_range", "qx", "lx", "dx", "Lx", "Tx", "ex")) |>
    filter(!is.na(lx), str_detect(age_range, "^[0-9]")) |>
    mutate(age = as.integer(str_extract(age_range, "^[0-9]+")),
           lx  = as.numeric(lx) / 100000) |>
    select(age, lx)
}

.read_us_asfr_2018 <- function() {
  midpoints <- c("15-19 Years" = 17.5, "20-24 Years" = 22.5,
                 "25-29 Years" = 27.5, "30-34 Years" = 32.5,
                 "35-39 Years" = 37.5, "40-44 Years" = 42.5,
                 "45-49 Years" = 47.5)
  read_csv("data/us_asfr.csv", show_col_types = FALSE) |>
    filter(Year == 2018, `Age Group` %in% names(midpoints)) |>
    mutate(age = midpoints[`Age Group`],
           mx  = `Birth Rate` / 1000 * 0.4886) |>
    select(age, mx)
}

compute_euler_lotka_us <- function() {
  lt   <- .read_us_life_table_2020()
  asfr <- .read_us_asfr_2018()
  lx_i <- approx(lt$age, lt$lx, xout = asfr$age)$y
  R0   <- sum(lx_i * asfr$mx) * 5
  Tg   <- sum(asfr$age * lx_i * asfr$mx) / sum(lx_i * asfr$mx)
  r_approx <- log(R0) / Tg
  el <- function(r) sum(lx_i * asfr$mx * exp(-r * asfr$age)) * 5 - 1
  r_exact <- uniroot(el, c(-0.5, 0.5))$root
  list(R0 = R0, T = Tg, r_approx = r_approx, r_exact = r_exact)
}

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

plot_exponential_growth <- function(t_max = 5, N0 = 1) {
  r_vals <- c(-0.5, 0, 0.5)
  cols   <- c("#e74c3c", "#95a5a6", "#27ae60")

  purrr::map_dfr(r_vals, \(r)
    tibble(t     = seq(0, t_max, length.out = 300),
           log_N = log(N0) + r * t,
           r     = factor(r, levels = r_vals,
                          labels = c("r = -1/2", "r = 0", "r = 1/2")))
  ) |>
    ggplot(aes(t, log_N, color = r)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = setNames(cols, c("r = -1/2", "r = 0", "r = 1/2")),
                       name = NULL) +
    labs(x = expression(t), y = expression(log ~ N(t))) +
    theme(legend.position = c(0.12, 0.78))
}

plot_exponential_growth_linear <- function(t_max = 5, N0 = 1) {
  r_vals <- c(-0.5, 0, 0.5)
  cols   <- c("#e74c3c", "#95a5a6", "#27ae60")

  purrr::map_dfr(r_vals, \(r)
    tibble(t   = seq(0, t_max, length.out = 300),
           N   = N0 * exp(r * t),
           r   = factor(r, levels = r_vals,
                        labels = c("r = -1/2", "r = 0", "r = 1/2")))
  ) |>
    ggplot(aes(t, N, color = r)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = setNames(cols, c("r = -1/2", "r = 0", "r = 1/2")),
                       name = NULL) +
    labs(x = expression(t), y = expression(N(t))) +
    theme(legend.position = c(0.88, 0.65))
}

plot_us_lx <- function() {
  .read_us_life_table_2020() |>
    ggplot(aes(age, lx)) +
    geom_line(color = "#2980b9", linewidth = 0.9) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Age (years)", y = expression(l[x]),
         caption = "US females, 2020 (CDC NVSR 71-01)")
}

plot_us_mx <- function() {
  .read_us_asfr_2018() |>
    ggplot(aes(age, mx)) +
    geom_col(fill = "#e67e22", width = 4.5) +
    scale_x_continuous(breaks = seq(15, 50, by = 5), limits = c(12, 52)) +
    labs(x = "Age (years)",
         y = expression(m[x] ~ "(female births · female"^{-1} ~ "· yr"^{-1} * ")"),
         caption = "US females, 2018 (CDC NCHS)")
}

plot_logistic_map_bif <- function(n_r = 600, n_burn = 500, n_keep = 120) {
  r_vals <- seq(1, 4, length.out = n_r)
  purrr::map_dfr(seq_len(n_r), function(idx) {
    r <- r_vals[idx]; N <- 0.5
    for (j in seq_len(n_burn)) N <- r * N * (1 - N)
    Ns <- numeric(n_keep)
    for (j in seq_len(n_keep)) { N <- r * N * (1 - N); Ns[j] <- N }
    tibble(r = r, N = Ns)
  }) |>
    ggplot(aes(r, N)) +
    geom_point(size = 0.05, alpha = 0.25, color = "#555555") +
    scale_x_continuous(breaks = 1:4) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "r", y = expression(N[t] ~ "(attractor)"))
}

plot_logistic_ts_facet <- function(r_show = c(2.0, 3.2, 3.5, 3.9), n_ts = 60) {
  purrr::map_dfr(r_show, function(r) {
    Ns <- numeric(n_ts + 1L); Ns[1L] <- 0.5
    for (t in seq_len(n_ts)) Ns[t + 1L] <- r * Ns[t] * (1 - Ns[t])
    tibble(label = sprintf("r = %.1f", r), t = 0:n_ts, N = Ns)
  }) |>
    mutate(label = factor(label, levels = sprintf("r = %.1f", r_show))) |>
    ggplot(aes(t, N)) +
    geom_line(color = "#2980b9", linewidth = 0.6) +
    geom_point(size = 0.4, color = "#2980b9") +
    facet_wrap(~label, nrow = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "t", y = expression(N[t]))
}

plot_iceman_c14 <- function() {
  meas <- tribble(
    ~lab,                ~material, ~c14_age, ~sigma,
    "Oxford (tissue)",   "tissue",    4500,    30,
    "Oxford (bone)",     "bone",      4580,    30,
    "ETH-8345 (tissue)", "tissue",    4555,    35,
    "ETH-8342 (bone)",   "bone",      4560,    65,
    "ETH-8345-3 (grass)","grass",     4535,    60,
    "Uppsala Ua-2373",   "grass",     4612,    51,
    "Uppsala Ua-2374",   "grass",     4343,   100,
    "Paris GifA",        "grass",     4452,   148,
    "Krueger/Oxford",    "grass",     4555,    48
  ) |>
    mutate(lab = factor(lab, levels = rev(lab)))

  wt_mean <- 4546; wt_sd <- 17

  ggplot(meas, aes(y = lab, x = c14_age)) +
    annotate("rect", xmin = wt_mean - 2*wt_sd, xmax = wt_mean + 2*wt_sd,
             ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#2980b9") +
    geom_vline(xintercept = wt_mean, color = "#2980b9", linetype = "dashed") +
    geom_errorbarh(aes(xmin = c14_age - 2*sigma, xmax = c14_age + 2*sigma),
                   height = 0.3, color = "grey40") +
    geom_point(aes(shape = material), size = 3) +
    scale_shape_manual(values = c(tissue = 16, bone = 15, grass = 17), name = NULL) +
    labs(x = expression(""^14*"C age (yr BP)"), y = NULL) +
    theme(legend.position = "bottom")
}

plot_human_population <- function() {
  breaks_yr <- c(-10000, -5000, 0, 2000)
  labels_yr  <- c("10000 BCE", "5000 BCE", "1 CE", "2000 CE")

  read_csv("data/owid_world_population.csv", show_col_types = FALSE) |>
    filter(Entity == "World") |>
    ggplot(aes(Year, Population)) +
    geom_line(linewidth = 0.8, color = "#2980b9") +
    scale_x_continuous(breaks = breaks_yr, labels = labels_yr) +
    scale_y_log10(
      labels = \(x) dplyr::case_when(
        x >= 1e9 ~ paste0(x / 1e9, "B"),
        x >= 1e6 ~ paste0(x / 1e6, "M"),
        TRUE     ~ scales::comma(x)
      )
    ) +
    labs(x = NULL, y = "World population (log scale)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_births_by_day <- function() {
  holidays <- tribble(
    ~month, ~dom, ~label,
    1,  1,  "New Year's Day",
    7,  4,  "Independence Day",
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
