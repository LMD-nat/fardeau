library(ggplot2)
library(pROC)
library(cowplot)
library(scales)
library(Cairo)


# A. Predictors of thirty-day revisits include ZBI score, number of ED visits in the previous year, and the COVID-19 period, which interacted with both ZBI score and past ED visits.
# B. Predictors of seven-day revisits include number of ED visits in the previous year, female sex, patient living in a care home, a caregiver living alone, and a CTAS triage score below 5.
# C. Predictors of three-day revisits include number of ED visits in the previous year, a caregiver living alone, a CTAS triage score below 5, and time on stretcher at the ED.
# D. Predictors of thirty-day revisits resulting in admission include Charlson score, a walk-in arrival to the ED, and a greater caregiver income.

caption1 <- ggdraw() + draw_label("  AUC = 0.63\n  Model of thirty-day revisits", size = 10, hjust = 0, x = 0)
caption2 <- ggdraw() + draw_label("  AUC = 0.65\n  Model of seven-day revisits", size = 10, hjust = 0, x = 0)
caption3 <- ggdraw() + draw_label("  AUC = 0.65\n  Model of three-day revisits", size = 10, hjust = 0, x = 0)
caption4 <- ggdraw() + draw_label("  AUC = 0.67\n  Model of thirty-day revisits\n resulting in hospitalization", size = 10, hjust = 0, x = 0)

add_box <- function(p) {
  ggdraw(p) + 
    theme(plot.background = element_rect(color = "lightgrey", size = 0.5))
}

plot1 <- add_box(plot_grid(roc30, caption1, ncol = 1, rel_heights = c(1, 0.3)))
plot2 <- add_box(plot_grid(roc7, caption2, ncol = 1, rel_heights = c(1, 0.3)))
plot3 <- add_box(plot_grid(roc3, caption3, ncol = 1, rel_heights = c(1, 0.3)))
plot4 <- add_box(plot_grid(roca30, caption4, ncol = 1, rel_heights = c(1, 0.3)))

final_plot <- plot_grid(plot1, plot2, plot3, plot4, ncol = 2, labels = "AUTO")

final_plot

ggsave("path", plot = final_plot, 
       width = 5, height = 6.5, units = "in", dpi = 200,
       device = cairo_ps)

ggsave("path", plot = final_plot, 
       width = 5, height = 6.5, units = "in", dpi = 200,
       device = "png")
