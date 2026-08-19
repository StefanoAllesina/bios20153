# One function per figure, named plot_<thing>().
# All stochastic functions call set.seed(42) internally.
# Themes and palettes are set once here; do not add ad hoc theme() calls elsewhere.

source("R/utils.R")

theme_set(theme_classic(base_size = 12))
