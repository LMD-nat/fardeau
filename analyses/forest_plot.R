library(ggplot2)
library(cairo)

# Data
terms <- c("Overall ZBI Score",
           "ZBI Score at Wave 1",
           "ZBI Score between Wave 1 and Wave 2",
           "ZBI Score at Wave 2",
           "ZBI Score at Wave 3",
           "ZBI Score at Wave 4")

OR <- as.numeric(c(1.03, 1.04, 0.89, 1.01, 1.03, 0.96))
lower <- as.numeric(c(1.00, 0.98, 0.78, 0.96, 0.95, 0.89))
upper <- as.numeric(c(1.05, 1.10, 0.97, 1.05, 1.11, 1.03))

df <- data.frame(term = terms, OR = OR, lower = lower, upper = upper)

# Format the label text: OR [95% CI]
df$label <- sprintf("%.2f [%.2f, %.2f]", df$OR, df$lower, df$upper)

# Reorder terms based on OR for plotting
df$term <- factor(df$term, levels = df$term[order(df$OR)])

# Plot
p <- ggplot(df, aes(x = OR, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, size = 0.8) +
  geom_text(aes(label = label), hjust = -1.5, size = 3.5, color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  scale_x_continuous(limits = c(0.75, 1.5), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Odds Ratio",
    y = NULL,
    title = "Moderation of the effect of ZBI scores on ED revisits\nby Wave of the COVID-19 pandemic"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 10)
  )

print(p)


# Save as PNG
ggsave("forest_plot_wCI.png", plot = p, width = 7, height = 5, dpi = 300)

# Save as EPS
ggsave("forest_plotwCI.eps", plot = p, width = 7, height = 5, device = cairo_ps)
