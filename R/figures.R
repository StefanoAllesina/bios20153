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

plot_sir_diagram <- function() {
  bw <- 1.4; bh <- 1.0; ay <- 2.0
  xs <- c(1.0, 4.5, 8.0)
  ggplot() +
    annotate("rect", xmin=xs[1]-bw/2, xmax=xs[1]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[1], y=ay, label="S", size=7, fontface="bold") +
    annotate("rect", xmin=xs[2]-bw/2, xmax=xs[2]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[2], y=ay, label="I", size=7, fontface="bold") +
    annotate("rect", xmin=xs[3]-bw/2, xmax=xs[3]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[3], y=ay, label="R", size=7, fontface="bold") +
    annotate("segment",
             x=xs[1]+bw/2, xend=xs[2]-bw/2, y=ay, yend=ay,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=(xs[1]+xs[2])/2, y=ay+0.42, label="β S I", size=4) +
    annotate("segment",
             x=xs[2]+bw/2, xend=xs[3]-bw/2, y=ay, yend=ay,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=(xs[2]+xs[3])/2, y=ay+0.42, label="γ I", size=4) +
    coord_cartesian(xlim=c(-0.2, 9.2), ylim=c(0.8, 3.2)) +
    theme_void()
}

plot_sir_demography_diagram <- function() {
  bw <- 1.4; bh <- 1.0; ay <- 2.0; dlen <- 0.85
  xs <- c(1.0, 4.5, 8.0)
  ggplot() +
    annotate("segment",
             x=-0.2, xend=xs[1]-bw/2, y=ay, yend=ay,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=-0.1, y=ay+0.42, label="Λ", size=4, hjust=0) +
    annotate("rect", xmin=xs[1]-bw/2, xmax=xs[1]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[1], y=ay, label="S", size=7, fontface="bold") +
    annotate("rect", xmin=xs[2]-bw/2, xmax=xs[2]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[2], y=ay, label="I", size=7, fontface="bold") +
    annotate("rect", xmin=xs[3]-bw/2, xmax=xs[3]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[3], y=ay, label="R", size=7, fontface="bold") +
    annotate("segment",
             x=xs[1]+bw/2, xend=xs[2]-bw/2, y=ay, yend=ay,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=(xs[1]+xs[2])/2, y=ay+0.42, label="β S I", size=4) +
    annotate("segment",
             x=xs[2]+bw/2, xend=xs[3]-bw/2, y=ay, yend=ay,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=(xs[2]+xs[3])/2, y=ay+0.42, label="γ I", size=4) +
    annotate("segment", x=xs[1], xend=xs[1], y=ay-bh/2, yend=ay-bh/2-dlen,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=xs[1]+0.3, y=ay-bh/2-dlen/2, label="μ S", size=3.8, hjust=0) +
    annotate("segment", x=xs[2], xend=xs[2], y=ay-bh/2, yend=ay-bh/2-dlen,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=xs[2]+0.3, y=ay-bh/2-dlen/2, label="μ I", size=3.8, hjust=0) +
    annotate("segment", x=xs[3], xend=xs[3], y=ay-bh/2, yend=ay-bh/2-dlen,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=xs[3]+0.3, y=ay-bh/2-dlen/2, label="μ R", size=3.8, hjust=0) +
    coord_cartesian(xlim=c(-0.5, 9.5), ylim=c(0.0, 3.2)) +
    theme_void()
}

plot_sis_diagram <- function() {
  bw <- 1.6; bh <- 1.0; ay <- 2.0; off <- 0.22
  xs <- c(2.0, 7.0)
  mid <- (xs[1] + xs[2]) / 2
  ggplot() +
    annotate("rect", xmin=xs[1]-bw/2, xmax=xs[1]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[1], y=ay, label="S", size=7, fontface="bold") +
    annotate("rect", xmin=xs[2]-bw/2, xmax=xs[2]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[2], y=ay, label="I", size=7, fontface="bold") +
    annotate("segment",
             x=xs[1]+bw/2, xend=xs[2]-bw/2, y=ay+off, yend=ay+off,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=mid, y=ay+off+0.38, label="β S I", size=4) +
    annotate("segment",
             x=xs[2]-bw/2, xend=xs[1]+bw/2, y=ay-off, yend=ay-off,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=mid, y=ay-off-0.38, label="γ I", size=4) +
    coord_cartesian(xlim=c(0.5, 8.5), ylim=c(0.8, 3.2)) +
    theme_void()
}

plot_seir_diagram <- function() {
  bw <- 1.2; bh <- 1.0; ay <- 1.8
  xs <- c(1.0, 3.5, 6.0, 8.5)
  ggplot() +
    annotate("rect", xmin=xs[1]-bw/2, xmax=xs[1]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[1], y=ay, label="S", size=6, fontface="bold") +
    annotate("rect", xmin=xs[2]-bw/2, xmax=xs[2]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[2], y=ay, label="E", size=6, fontface="bold") +
    annotate("rect", xmin=xs[3]-bw/2, xmax=xs[3]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[3], y=ay, label="I", size=6, fontface="bold") +
    annotate("rect", xmin=xs[4]-bw/2, xmax=xs[4]+bw/2, ymin=ay-bh/2, ymax=ay+bh/2,
             fill="#ecf0f1", color="black", linewidth=0.6) +
    annotate("text", x=xs[4], y=ay, label="R", size=6, fontface="bold") +
    annotate("segment",
             x=xs[1]+bw/2, xend=xs[2]-bw/2, y=ay, yend=ay,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=(xs[1]+xs[2])/2, y=ay+0.4, label="β S I", size=3.5) +
    annotate("segment",
             x=xs[2]+bw/2, xend=xs[3]-bw/2, y=ay, yend=ay,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=(xs[2]+xs[3])/2, y=ay+0.4, label="σ E", size=3.5) +
    annotate("segment",
             x=xs[3]+bw/2, xend=xs[4]-bw/2, y=ay, yend=ay,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=(xs[3]+xs[4])/2, y=ay+0.4, label="γ I", size=3.5) +
    annotate("curve",
             x=xs[4], xend=xs[1], y=ay+bh/2, yend=ay+bh/2,
             curvature=0.4,
             arrow=arrow(length=unit(0.12,"in")), linewidth=0.7) +
    annotate("text", x=(xs[1]+xs[4])/2, y=ay+bh/2+1.0, label="δ R", size=3.5) +
    coord_cartesian(xlim=c(0, 9.5), ylim=c(0.6, 3.8)) +
    theme_void()
}

plot_sir_dynamics_static <- function(
    S0_show = c(0.25, 0.65, 0.95),
    beta = 2, gamma = 1, I0 = 0.01,
    t_max = 20, dt = 0.05
) {
  sir_ode <- function(S0) {
    n  <- as.integer(t_max / dt)
    Sv <- Iv <- Rv <- numeric(n + 1L)
    Sv[1] <- S0; Iv[1] <- I0; Rv[1] <- 1 - S0 - I0
    dv <- function(s, i) c(-beta*s*i, beta*s*i - gamma*i, gamma*i)
    for (k in seq_len(n)) {
      y  <- c(Sv[k], Iv[k], Rv[k])
      k1 <- dv(y[1], y[2])
      k2 <- dv((y + dt/2*k1)[1], (y + dt/2*k1)[2])
      k3 <- dv((y + dt/2*k2)[1], (y + dt/2*k2)[2])
      k4 <- dv((y + dt*k3)[1],   (y + dt*k3)[2])
      yn  <- pmax(y + dt/6*(k1 + 2*k2 + 2*k3 + k4), 0)
      Sv[k+1] <- yn[1]; Iv[k+1] <- yn[2]; Rv[k+1] <- yn[3]
    }
    data.frame(t = seq(0, t_max, by = dt), S = Sv, I = Iv, R = Rv)
  }

  purrr::map_dfr(S0_show, function(S0) {
    sir_ode(S0) |>
      pivot_longer(c(S, I, R), names_to = "compartment", values_to = "value") |>
      mutate(S0_label = paste0("S[0] == ", S0))
  }) |>
    mutate(
      S0_label    = factor(S0_label, levels = paste0("S[0] == ", sort(S0_show))),
      compartment = factor(compartment, levels = c("S", "I", "R"))
    ) |>
    ggplot(aes(t, value, color = compartment)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~S0_label, nrow = 1, labeller = label_parsed) +
    scale_color_manual(
      values = c(S = "#2980b9", I = "#e74c3c", R = "#27ae60"),
      name = NULL
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = expression(t), y = "Proportion") +
    theme(legend.position = "bottom")
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
