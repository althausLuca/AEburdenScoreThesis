library(ggplot2)
library(tweedie)
library(latex2exp)

x <- seq(0, 200, length.out = 500)[-1]  # Adjusted to exclude zero for the line

phi_fixed <- 1
mu <- 1
xis <- c(1.1, 1.5, 1.8)

# Prepare the data frame for lines
df <- data.frame(
  x = rep(x, times = length(xis)),
  xi = factor(rep(xis, each = length(x))),
  Density = unlist(lapply(xis, function(xi) dtweedie(x, power = xi, phi = phi_fixed, mu = mu)))
)

# Data for the zero mass point
zero_mass <- sapply(xis, function(xi) ptweedie(0, mu = mu, phi = phi_fixed, power = xi))
df_zero_mass <- data.frame(
  x = 0,
  Density = zero_mass,
  xi = factor(xis)
)

colors <- c("red", "blue", "green")
linetypes <- c("solid", "dashed", "dotted")
shapes <- c(16, 17, 18)  # Filled circle, triangle, and diamond

p <- ggplot() +
  geom_line(data = df, aes(x = x, y = Density, color = xi, linetype = xi), size = 1) +
  geom_point(data = df_zero_mass, aes(x = x, y = Density, color = xi, shape = xi), size = 3) +
  scale_color_manual(values = colors,
                     labels = lapply(xis, function(xi) TeX(sprintf("$\\xi = %.2f$", xi)))) +
  scale_linetype_manual(values = linetypes) +
  scale_shape_manual(values = shapes) +
  labs(title = "",
       x = "x", y = "Density") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position  = c(0.8, 0.9),
        legend.text = element_text(size = 15),
        legend.key.width= unit(1.5, 'cm'),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  guides(color = guide_legend(override.aes = list(shape = shapes, linetype = linetypes)),
         shape = FALSE,
         linetype = FALSE)

# Print the plot
print(p)
