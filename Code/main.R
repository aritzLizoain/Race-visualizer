# Libraries & functions
require(styler)
require(ggplot2)
require(dplyr)
require(animation)
require(ggtext)
require(gridExtra)
require(grid)
require(png)

# Source utility functions from the utils folder
function_path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/utils/")
invisible(sapply(list.files(pattern = "[.]R$", path = function_path, full.names = TRUE), source))

# Define race ----
total_distance <- 1500
distance <- c(1:15 * 100)
time <- cumsum(c(
  13.75,
  13.73,
  13.72,
  13.72,
  13.71,
  13.73,
  13.77,
  13.79,
  13.77,
  13.74,
  13.71,
  13.66,
  13.58,
  13.65,
  13.75
))

race <- data.frame(
  distance,
  time
)

# References ----
time.wr <- "3:26.00"
time.dlr <- "3:26.69"
time.er <- "3:27.14"
time.lowest <- "3:27.20"
time.highest <- "3:25.40"

reference0 <- list(avg.pace = convert_time_txt2sec(time.highest) / total_distance * 1000, name = "", time = time.er)
reference1 <- list(avg.pace = convert_time_txt2sec(time.wr) / total_distance * 1000, name = "WR", time = time.wr)
reference2 <- list(avg.pace = convert_time_txt2sec(time.dlr) / total_distance * 1000, name = "DLR", time = time.dlr)
reference3 <- list(avg.pace = convert_time_txt2sec(time.er) / total_distance * 1000, name = "ER", time = time.er)
reference4 <- list(avg.pace = convert_time_txt2sec(time.lowest) / total_distance * 1000, name = "", time = time.er)
references <- as.data.frame(rbind(reference0, reference1, reference2, reference3, reference4))

# Start recording video ----
saveGIF(
  {
    # Loop the race
    resolution <- 15 # one measurement every 100/resolution meters
    total_n_points <- total_distance / 100 * resolution + 1
    # Visualize ----
    # Frames before start
    for (i in 1:10) {
      visualize_race_empty(
        race = race,
        position = 1,
        references,
        title = "1500m - Wanda Diamond League 2024",
        subtitle = "Jakob Ingebrigtsen"
      )
    }
    # Start
    for (i in 1:total_n_points) {
      # Actual race
      visualize_race(race,
        position = i,
        references,
        title = "1500m - Wanda Diamond League 2024",
        subtitle = "Jakob Ingebrigtsen"
      )
    }
    # Frames after finish
    for (i in 1:10) {
      visualize_race(race,
        position = total_n_points,
        references,
        title = "1500m - Wanda Diamond League 2024",
        subtitle = "Jakob Ingebrigtsen"
      )
    }
  },
  movie.name = paste0(dirname(function_path), "/result_ggplot.gif"),
  interval = 0.1, # Lower value goes faster
  ani.width = 1300,
  ani.height = 900
)

# Tidy style ----
style_file(utils::getSrcFilename(function() {}, full.names = TRUE))
