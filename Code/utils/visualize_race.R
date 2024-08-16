# Diamond league colors ----
# Darkest to lightest
blue1 <- "#0e003c"
blue2 <- "#0381ba"
blue3 <- "#07d2e6"
blue4 <- "#04ffff"
white1 <- "#ffffff"
gold1 <- "#a98011"
gold2 <- "#d0ab4c"
gold3 <- "#e5b122"

# Convert number to pace  ----
# Function to convert pace from numeric seconds to min/km text
convert_pace <- function(pace.numeric) {
  pace_minutes <- floor(pace.numeric / 60)
  pace_seconds <- pace.numeric %% 60
  pace.text <- paste0(pace_minutes, ":", round(pace_seconds, 2))
  return(pace.text)
}

# Convert time to number ----
# Function to convert time as text into numeric seconds
convert_time_txt2sec <- function(time.text) {
  minutes <- as.numeric(substr(time.text, 1, 1))
  seconds <- as.numeric(substr(time.text, 3, 7))
  time.seconds <- minutes * 60 + seconds
  return(time.seconds)
}

# Convert number to time ----
# Function to convert time as numeric into text
convert_time_sec2txt <- function(time.seconds) {
  minutes <- floor(time.seconds / 60)
  seconds <- round(time.seconds %% 60, 2)
  time.text <- paste0(minutes, ":", seconds)
  return(time.text)
}

# Add reference lines ----
# Function to draw reference line with label and time
draw_reference <- function(plot, r, kol, right_side = TRUE) {
  avg.pace <- as.numeric(r$avg.pace)
  plot <- plot +
    # Horizontal line
    geom_hline(yintercept = avg.pace, col = alpha(kol, 0.6), linewidth = 2)
  # Projected time
  if (right_side) {
    plot <- plot +
      geom_text(
        inherit.aes = FALSE,
        aes(
          x = total_distance,
          y = avg.pace,
          label = as.character(r$time)
        ),
        fontface = "bold",
        color = alpha(kol, 0.2), hjust = -0.95, size = 7
      ) +
      theme(plot.margin = unit(c(0, 6, 1.5, 0), "lines")) +
      coord_cartesian(clip = "off")
  } else {
    plot <- plot +
      geom_text(
        inherit.aes = FALSE,
        aes(
          x = total_distance,
          y = avg.pace,
          label = ""
        ),
        fontface = "bold",
        color = alpha(kol, 0.2), hjust = -0.6, size = 7
      )
  }
  return(plot)
}

# Visualize race ----
visualize_race <- function(race,
                           position,
                           references,
                           title,
                           subtitle) {
  # Interpolate
  distance_interpolated <- seq(from = min(race$distance), to = max(race$distance), length.out = total_distance / 100 * resolution + 1)
  # Linear
  # time_interpolated <- approx(x = race$distance, y = race$time, xout = distance_interpolated, method = "linear")
  # time_interpolated <- time_interpolated$y
  # Smooth splines
  time_interpolated <- spline(x = race$distance, y = race$time, xout = distance_interpolated)$y

  race <- data.frame(time = time_interpolated, distance = distance_interpolated)

  # Calculate pace
  race <- race %>% mutate(pace = time / distance * 1000)
  # COLORS
  # Color change if on pace
  race.pace <- mean(race$pace[2:(position - 1)])
  on_pace <- ifelse(race.pace < references$avg.pace[[2]],
    TRUE,
    FALSE
  )
  # Reference line colors
  race.pace.col <- ifelse(on_pace, gold3, white1)
  title.col <- ifelse(on_pace, gold2, white1)
  # Distance colors
  distance_colors <- c(blue2, blue2, blue3, blue3, blue4)

  # colors for pace
  pace_colors <- c("black", blue4, blue3, blue2, "black")

  # Race plot ----
  race <- race[1:position, ]
  distance_colors[max(race$distance) < c(0, 400, 800, 1200, 1500)] <- "lightgray" # If split not reached, colored in gray
  mid <- mean(race$pace[-1])
  # Add splits to the x axis
  split400 <- convert_time_sec2txt(race[which.min(abs(race$distance - 400)), ]$time)
  split800 <- convert_time_sec2txt(race[which.min(abs(race$distance - 800)), ]$time)
  split1200 <- convert_time_sec2txt(race[which.min(abs(race$distance - 1200)), ]$time)

  if (max(race$distance) >= 1200) {
    labels_with_splits <- c(
      paste0(0, "<br>"),
      paste0(400, "<br><span style='color:#757474;'>", split400, "</span>"),
      paste0(800, "<br><span style='color:#757474;'>", split800, "</span>"),
      paste0(1200, "<br><span style='color:#757474;'>", split1200, "</span>"),
      paste0(1500, "<br>")
    )
  } else if (max(race$distance) >= 800) {
    labels_with_splits <- c(
      paste0(0, "<br>"),
      paste0(400, "<br><span style='color:#757474;'>", split400, "</span>"),
      paste0(800, "<br><span style='color:#757474;'>", split800, "</span>"),
      paste0(1200, "<br>"),
      paste0(1500, "<br>")
    )
  } else if (max(race$distance) >= 400) {
    labels_with_splits <- c(
      paste0(0, "<br>"),
      paste0(400, "<br><span style='color:#757474;'>", split400, "</span>"),
      paste0(800, "<br>"),
      paste0(1200, "<br>"),
      paste0(1500, "<br>")
    )
  } else {
    labels_with_splits <- c(
      paste0(0, "<br>"),
      paste0(400, "<br>"),
      paste0(800, "<br>"),
      paste0(1200, "<br>"),
      paste0(1500, "<br>")
    )
  }

  race.plot <- race %>% ggplot(aes(
    x = distance,
    y = pace,
    color = pace
  )) +
    geom_point(size = 6) +
    scale_color_gradient2(
      midpoint = (max(race$pace) + min(race$pace)) / 2, low = "red", mid = "yellow",
      high = "green"
    ) +
    theme_minimal(base_size = 24) +
    labs(
      title = paste0(title, "                             Estimated final time: ", convert_pace(race.pace * 1.5)),
      subtitle = subtitle,
      x = "Distance [m]",
      y = "Pace [min/km]"
    ) +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "darkgray", face = "bold"),
      axis.line = element_line(color = "black"),
      axis.title = element_text(color = "darkgray"),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 30)),
      plot.title = element_text(color = title.col, face = "bold"),
      plot.subtitle = element_markdown(color = "lightgray"),
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      axis.text.y = suppressWarnings(
        element_text(color = pace_colors),
      ),
      axis.text.x = suppressWarnings(
        element_markdown(color = distance_colors)
      )
    ) +
    scale_y_reverse(
      breaks = unlist(references$avg.pace),
      labels = unlist(references$name)
    ) +
    scale_x_continuous(
      limits = c(0, total_distance),
      breaks = c(0, 400, 800, 1200, 1500),
      labels = labels_with_splits
    )
  # Add reference lines
  reference_colors <- c("black", blue4, blue3, blue2, "black")
  for (rn in 1:nrow(references)) {
    race.plot <- draw_reference(race.plot, references[rn, ], reference_colors[rn])
  }
  race.plot <- draw_reference(race.plot, data.frame(avg.pace = race.pace, time = convert_time_sec2txt(max(race$time) / max(race$distance) * total_distance)), race.pace.col, right_side = FALSE)
  # Add DL logo
  logo <- readPNG(paste0(function_path, ifelse(on_pace, "DLlogo_gold.png", "DLlogo.png")))
  plot_logo <- annotation_custom(rasterGrob(logo, interpolate = TRUE), xmin = max(race$distance) - 25, xmax = max(race$distance) + 25, ymin = -138.08, ymax = -138.23)
  race.plot <- race.plot + plot_logo
  # Add license
  license_text <- textGrob("Copyright © 2024 Aritz Lizoain Cotanda - Licensed under the MIT License", gp = gpar(fontsize = 18, col = "gray"))
  race.plot <- race.plot +
    annotation_custom(license_text, xmin = 700, xmax = 800, ymin = -138.39, ymax = -138.45)

  # Arrange the plots ----
  # Set up the layout matrix
  layout.matrix <- matrix(1, nrow = 100, ncol = 100)
  layout.matrix[3:98, 3:98] <- 2 # Leave borders out
  layout.matrix[2:99, 2:99] <- 0 # Leave borders out
  # Combine the plots with the layout
  arranged.plots <- suppressWarnings( # Avoid warning saying that some rows are deleted (we know that not all rows contain all data)
    grid.arrange(race.plot,
      layout_matrix = layout.matrix
    )
  )
  # Plot the arranged plots with a black background and common title
  grid.draw(grobTree(
    rectGrob(gp = gpar(fill = "black")), # background.col
    arranged.plots
  ))
}

# Visualize EMPTY race ----
visualize_race_empty <- function(race,
                                 position,
                                 references,
                                 title,
                                 subtitle) {
  background.col <- "black"
  distance_colors <- c(blue2, blue2, blue3, blue3, blue4)

  # colors for pace
  pace_colors <- c("black", blue4, blue3, blue2, "black")

  # Race plot ----
  race <- race[1:position, ]
  distance_colors[max(race$distance) < c(0, 400, 800, 1200, 1500)] <- "lightgray" # If split not reached, colored in gray
  race.plot <- race %>% ggplot(aes(
    x = distance,
    y = pace,
    color = pace
  )) +
    theme_minimal(base_size = 24) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Distance [m]",
      y = "Pace [min/km]"
    ) +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "darkgray", face = "bold"),
      axis.line = element_line(color = "black"),
      axis.title = element_text(color = "darkgray"),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 30)),
      plot.title = element_text(color = "lightgray", face = "bold"),
      plot.subtitle = element_markdown(color = "lightgray"),
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      axis.text.y = suppressWarnings(
        element_text(color = pace_colors),
      ),
      axis.text.x = suppressWarnings(
        element_markdown(color = distance_colors)
      )
    ) +
    scale_y_reverse(
      breaks = unlist(references$avg.pace),
      labels = unlist(references$name)
    ) +
    scale_x_continuous(
      limits = c(0, total_distance),
      breaks = c(0, 400, 800, 1200, 1500),
      labels = c(
        paste0(0, "<br>"),
        paste0(400, "<br>"),
        paste0(800, "<br>"),
        paste0(1200, "<br>"),
        paste0(1500, "<br>")
      )
    )
  # Add reference lines
  reference_colors <- c("black", blue4, blue3, blue2, "black")
  for (rn in 1:nrow(references)) {
    race.plot <- draw_reference(race.plot, references[rn, ], reference_colors[rn])
  }
  # Add DL logo
  logo <- readPNG(paste0(function_path, "DLlogo.png"))
  plot_logo <- annotation_custom(rasterGrob(logo, interpolate = TRUE), xmin = 25, xmax = 25, ymin = -138.03, ymax = -138.23)
  race.plot <- race.plot + plot_logo

  # Add license
  license_text <- textGrob("Copyright © 2024 Aritz Lizoain Cotanda - Licensed under the MIT License", gp = gpar(fontsize = 18, col = "gray"))
  race.plot <- race.plot +
    annotation_custom(license_text, xmin = 700, xmax = 800, ymin = -138.39, ymax = -138.45)

  # Arrange the plots ----
  # Set up the layout matrix
  layout.matrix <- matrix(1, nrow = 100, ncol = 100)
  layout.matrix[2:99, 2:99] <- 0 # Leave borders out
  # Combine the plots with the layout
  arranged.plots <- suppressWarnings( # Avoid warning saying that some rows are deleted (we know that not all rows contain all data)
    grid.arrange(race.plot,
      layout_matrix = layout.matrix
      # top = textGrob(paste0(title, subtitle), gp = gpar(fontsize = 14, fontface = "bold", col = title.col))
    )
  )
  # Plot the arranged plots with a black background and common title
  grid.draw(grobTree(
    rectGrob(gp = gpar(fill = background.col)),
    arranged.plots
  ))
}

# Not run when sourced ----
if (sys.nframe() == 0 || sys.nframe() == 4) {
  visualize_race(race,
    position = nrow(race),
    references,
    title = "My race!",
    subtitle = "Beamish 3k steeple"
  )
}

# Tidy style ----
style_file(utils::getSrcFilename(function() {}, full.names = TRUE))
