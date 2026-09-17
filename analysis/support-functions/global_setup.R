
options(scipen = 999)
# text sizes ----
base       <- 6
upper_base <- base + 0.5
title      <- base + 1

# Themes ----
base_theme <-
  theme_ggstatsplot() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        panel.background  = element_rect(fill = "white", color = "black", linetype = "solid"),
        strip.background  = element_rect(fill = "white", color = "black"),
        strip.text        = element_text(size = upper_base, face = "plain"),
        plot.title        = element_text(size = title, face = "plain"),
        axis.line         = element_line(color = "black"),
        axis.ticks        = element_line(color = "black"),
        axis.title        = element_text(size = upper_base, face = "plain"),
        axis.text         = element_text(size = base),
        legend.text       = element_text(size = base),
        legend.title      = element_text(size = base),
        legend.key.size   = unit(0.3, "cm"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.margin = margin(2.5,2.5,2.5,2.5, unit="pt")) 

# Colour palettes ----
s1_cols  <- c("#AF2820", "#345DA9")
s1_blue_grad <- c("#1E3562", "#345DA9", "#90ABCE")
s1_red_grad <- c("#650F09", "#AF2820", "#D4908C")
s2_blue_grad <- c("#345282", "#5B8FD9", "#A3BFEA")
s2_red_grad <- c("#872F29", "#E85A52", "#F3AAA7")
s2_cols  <- c("#AF2820", "#345DA9", "#E85A52", "#5B8FD9")
mf_cols  <- c("#CC79A7", "#D55E00", "#56B4E9", "#E69F00", "#009E73")
mf2_cols <- c("#CC79A7", "#D55E00", "#56B4E9", "#E69F00", "#009E73",
              "#EABCD6", "#FF8F37", "#A8DBF8", "#FFC748", "#33E8AD")

# Line and point args ----
s2_lines <- c("dotted", "solid")
m_line <- 0.75
s_line <- 0.1
s_alpha <- 0.03
p_alpha <- 0.05
p_size <- 0.75
homog_alpha <- 0.1
rib_alpha <- 0.3
c_p_size <- 2
c_lab_size <- 2
c_lab_nudge <- 0.5
s_m_p_size <- 1.5
s_m_p_alpha <- 0.1
s_m_p_shape <-  18
s_m_p_nudge <- -0.05

# ggbetweenstats wrapper ----
# general
ggbetween_cus <- function(...) {
  ggbetweenstats(
    results.subtitle      = FALSE,
    pairwise.display      = "none",
    point.args            = list(position = position_jitterdodge(dodge.width = 0.75),
                                 alpha = p_alpha, size = p_size, stroke = 0),
    centrality.label.args = list(size = c_lab_size, nudge_x = c_lab_nudge),
    centrality.point.args = list(size = c_p_size, colour = "black"),
    ...
  )
}

# Homogeneity variant — uses homog_alpha instead of p_alpha because only a
# subset of agents are clustered, giving fewer points to plot.
ggbetween_homog_cus <- function(...) {
  ggbetweenstats(
    results.subtitle      = FALSE,
    pairwise.display      = "none",
    point.args            = list(position = position_jitterdodge(dodge.width = 0.75),
                                 alpha = homog_alpha, size = p_size, stroke = 0),
    centrality.label.args = list(size = c_lab_size, nudge_x = c_lab_nudge),
    centrality.point.args = list(size = c_p_size, colour = "black"),
    ...
  )
}

# x axis for time plots ----
x_time <- scale_x_continuous(limits = c(1, 201),
                             breaks = seq(1, 201, by = 20),
                             labels = function(x) x - 1,
                             expand = expansion(mult = c(0.02,0.02)))

# for plots needing fewer points
x_time_2 <- scale_x_continuous(limits = c(1, 201),
                               breaks = seq(1, 201, by = 50),
                               labels = function(x) x - 1,
                               expand = expansion(mult = c(0.02,0.02)))
