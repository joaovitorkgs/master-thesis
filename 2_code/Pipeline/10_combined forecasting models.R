# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/09a_forecasting_TS.R")
source("./2_code/Pipeline/09b_forecasting_ARIMA.R")
source("./2_code/Pipeline/09c_forecasting_ETS.R")
source("./2_code/Pipeline/09d_forecasting_prophet.R")

# 2. Combining information -----------------------------------------------------



# 3. Combined plots ------------------------------------------------------------

library(grid)

plot_fcast_ARIMA_blank    <- plot_fcast_ARIMA   +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())

plot_fcast_ETS_blank      <- plot_fcast_ETS     +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())+
  scale_y_continuous(limits = c(0, 150000), labels = scales::comma)
  
plot_fcast_TS_blank       <- plot_fcast_TS      +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())+
  scale_y_continuous(limits = c(0, 750000), labels = scales::comma)

plot_fcast_Prophet_blank  <- plot_fcast_Prophet +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())


plot_fcast_all <- grid.arrange(
  grobs = list(
    arrangeGrob(plot_fcast_ARIMA_blank,   top = "(a) ARIMA"),
    arrangeGrob(plot_fcast_ETS_blank,     top = "(b) ETS"),
    arrangeGrob(plot_fcast_TS_blank,      top = "(c) Linear"),
    arrangeGrob(plot_fcast_Prophet_blank, top = "(d) Prophet")
  ),
  ncol = 2,
  top = textGrob("BEV Stock in Brazil: Forecast Comparison Across Models", gp = gpar(fontsize = 16)),
  bottom = textGrob("Year", gp = gpar(fontsize = 10)),
  left = textGrob("BEV Stock", 
                  rot = 90,  # Full 90° rotation
                  gp = gpar(fontsize = 10))
)


if (!file.exists("./4_plots/plot_fcast_all.png")) {
  ggsave(        "./4_plots/plot_fcast_all.png",
         plot   = plot_fcast_all,
         height = 8,
         width  = 10)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}
