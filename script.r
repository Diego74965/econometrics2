####### CUSTOM GRAPH THEME | IGNORE
theme.fxdat <- theme_gdocs() +
     theme(plot.title = element_text(size = 15),
           plot.subtitle = element_text(size = 11),
           plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "grey50"),
           axis.title.y = element_text(face = "bold", color = "gray30"),
           axis.title.x = element_text(face = "bold", color = "gray30", vjust = -1),
           panel.background = element_rect(fill = "grey95", colour = "grey75"),
           panel.border = element_rect(colour = "grey75"),
           panel.grid.major.y = element_line(colour = "white"),
           panel.grid.minor.y = element_line(colour = "white", linetype = "dotted"),
           panel.grid.major.x = element_line(colour = "white"),
           panel.grid.minor.x = element_line(colour = "white", linetype = "dotted"),
           strip.background = element_rect(size = 1, fill = "white", colour = "grey75"),
           strip.text.y = element_text(face = "bold"),
           axis.line = element_line(colour = "grey75"))

#### CUSTOM FUNCTION FOR PLOTTING FORECAST OBJECTS
plot_fx <- function(fx.dat,
                    PI = TRUE,
                    line.cols = NA,
                    shade.cols = NA,
                    show.gap = FALSE,
                    date.breaks = NA,
                    date.format = "%Y-%b",
                    main.title = NULL,
                    sub.title = NULL,
                    caption = NULL,
                    x.title = NULL,
                    y.title = "IMAI") {
 
     # manage package libraries
     pkgs <- c("dplyr",
               "ggplot2",
               "RColorBrewer",
               "scales",
               "forecast",
               "zoo")
     attached <- search()
     attached_pkgs <- attached[grepl("package", attached)]
     need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
     if (length(need_to_attach) > 0) {
          for (i in 1:length(need_to_attach)) {
               require(need_to_attach[i], character.only = TRUE)
          }
     }
 
     # data input testing and formatting
     if (class(fx.dat) != "forecast") {
          stop("forecast data object required", call. = FALSE)
     }
     if (is.na(line.cols[1])) {
          line.cols = c("#333749", "#00ced1", "goldenrod1")
     }
     if (length(line.cols) != 3) {
          stop("length of line.cols not equal to 3", call. = FALSE)
     }
     if (PI == TRUE) {
          pi.levels <- fx.dat$level
          n.levels <- length(pi.levels)
          if (is.na(shade.cols)) {
               shade.cols = brewer.pal(3, "PuBuGn")
               }
          if (n.levels != length(shade.cols)) {
               stop("length of shade.cols not equal to number of predictive intervals",
                    call. = FALSE)
          }
     }
     if (!show.gap) {
          freq <- attr(fx.dat$x, "tsp")[3]
          last.obs <- fx.dat$x[length(fx.dat$x)]
          last.time <- time(fx.dat$x)[length(fx.dat$x)]
          fx.dat$mean <- ts(c(last.obs, fx.dat$mean), start = last.time,
                            frequency = freq)
          fx.dat$upper <- ts(rbind(last.obs, fx.dat$upper), start = last.time,
                             frequency = freq)
          fx.dat$lower <- ts(rbind(last.obs, fx.dat$lower), start = last.time,
                             frequency = freq)
     }
     if (is.na(date.breaks)) {
          print("date.breaks to set to '6 months' absent user input")
          date.breaks <- "6 months"
     }
 
     # define dataframe with training (x), forecast (y) and interval (pi) data
     len.x <- length(fx.dat$x)
     len.y <- length(fx.dat$mean)
 
     df <- tibble(date = c(as.Date(time(fx.dat$x)), as.Date(time(fx.dat$mean))),
                  x = c(fx.dat$x, rep(NA, len.y)),
                  fitted = c(fx.dat$fitted, rep(NA, len.y)),
                  forecast = c(rep(NA, len.x), fx.dat$mean),
                  lo.80 = c(rep(NA, len.x), fx.dat$lower[, 1]),
                  up.80 = c(rep(NA, len.x), fx.dat$upper[, 1]),
                  lo.95 = c(rep(NA, len.x), fx.dat$lower[, 2]),
                  up.95 = c(rep(NA, len.x), fx.dat$upper[, 2]),
                  lo.99 = c(rep(NA, len.x), fx.dat$lower[, 3]),
                  up.99 = c(rep(NA, len.x), fx.dat$upper[, 3]))
 
     # plot training, fitted and forecast data
     ggplot(df,  aes(date, x)) +
          geom_line(aes(colour = "Training")) +
          #geom_line(data = df, aes(date, fitted, colour = "Fitted"), size = 0.75) +
          geom_ribbon(data = df, aes(date, ymin = lo.99, ymax = up.99, fill = "99%")) +
          geom_ribbon(data = df, aes(date, ymin = lo.95, ymax = up.95, fill = "95%")) +
          geom_ribbon(data = df, aes(date, ymin = lo.80, ymax = up.80, fill = "80%")) +
          geom_line(data = df, aes(date, forecast, colour = "Pronostico"), size = 0.75) +
          scale_x_date(breaks = seq(df$date[1], df$date[length(df$date)],
                                    by = date.breaks),
                       date_labels = date.format) +
          scale_colour_manual(name = "Datos",
                              values = c("Training" = line.cols[1],
                                         "Fitted" = line.cols[2],
                                         "Pronostico" = line.cols[3]),
                              breaks = c("Training", "Fitted", "Pronostico")) +
          scale_fill_manual(name = "Intervalos",
                            values = c("99%" = shade.cols[1], "95%" = shade.cols[2],
                                       "80%" = shade.cols[3])) +
          guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
          labs(title = main.title,
               subtitle = sub.title,
               caption = caption,
               x = x.title,
               y = y.title) +
          theme.fxdat + theme(plot.background = element_blank())
}