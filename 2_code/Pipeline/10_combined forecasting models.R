# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/09a_forecasting_TS.R")
source("./2_code/Pipeline/09b_forecasting_ARIMA.R")
source("./2_code/Pipeline/09c_forecasting_ETS.R")
source("./2_code/Pipeline/09d_forecasting_prophet.R")

# 2. Combining information -----------------------------------------------------


# 3. Combined plots ------------------------------------------------------------

library(grid)

plot_fcast_ARIMA   <- plot_fcast_ARIMA   +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())
plot_fcast_ETS     <- plot_fcast_ETS     +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())
plot_fcast_TS      <- plot_fcast_TS      +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())
plot_fcast_Prophet <- plot_fcast_Prophet +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())

grid.arrange(
  plot_fcast_ARIMA, plot_fcast_ETS,
  plot_fcast_TS, plot_fcast_Prophet,
  ncol = 2, nrow = 2
)

grid_with_title <- grid.arrange(
  grobs = list(
    arrangeGrob(plot_fcast_ARIMA,   top = "(a) ARIMA"),
    arrangeGrob(plot_fcast_ETS,     top = "(b) ETS"),
    arrangeGrob(plot_fcast_TS,      top = "(c) Linear"),
    arrangeGrob(plot_fcast_Prophet, top = "(d) Prophet")
  ),
  ncol = 2,
  top = textGrob("Forecast Comparison Across Models", gp = gpar(fontsize = 16, fontface = "bold"))
)
