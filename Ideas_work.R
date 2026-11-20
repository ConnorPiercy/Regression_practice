test_data <- data.frame(
  ethnicity = c(
    "Maori", "European", "Maori", "Maori", "European", "European", "Pacific", "Pacific", "Maori", "Maori",
    "European", "Pacific", "Pacific", "Maori", "Maori", "European", "European", "Pacific", "Pacific", "Maori",
    "European", "European", "Pacific", "Pacific", "Maori", "Maori", "European", "European", "Pacific", "Pacific",
    "Maori", "Maori", "European", "European", "Pacific", "Pacific", "Maori", "Maori", "European", "European",
    "Pacific", "Pacific", "Maori", "Maori", "European", "European", "Pacific", "Pacific", "Maori", "Maori",
    "European", "European", "Pacific", "Pacific", "Maori", "Maori", "European", "European", "Pacific", "Pacific"
  ),
  ppl = c(
    100, 200, 50, 102, 38, 34, 131, 161, 51, 121,
    239, 158, 194, 183, 202, 147, 100, 83, 82, 244,
    38, 213, 127, 166, 109, 136, 141, 199, 118, 138,
    146, 53, 243, 122, 229, 126, 58, 188, 170, 57,
    164, 151, 242, 120, 35, 132, 33, 157, 38, 84,
    49, 99, 107, 197, 155, 26, 184, 62, 219, 176
  ),
  conf_ppl = c(
    102, 203, 47, 98, 40, 36, 131, 161, 48, 115,
    235, 153, 190, 180, 197, 151, 101, 89, 77, 243,
    33, 212, 121, 172, 112, 131, 146, 197, 122, 137,
    140, 58, 244, 124, 229, 125, 53, 185, 166, 57,
    163, 154, 248, 115, 32, 128, 28, 152, 35, 83,
    50, 94, 112, 193, 154, 32, 179, 61, 221, 182
  ),
  region = c(
    "North", "South", "South", "North", "North", "North", "North", "North", "South", "South",
    "South", "South", "South", "North", "North", "North", "North", "North", "North", "South",
    "South", "South", "South", "South", "North", "North", "North", "North", "North", "North",
    "South", "South", "South", "South", "South", "South", "North", "North", "North", "North",
    "North", "North", "South", "South", "South", "South", "South", "South", "North", "North",
    "North", "North", "North", "North", "South", "South", "South", "South", "South", "South"
  ),
  year = c(
    2020, 2020, 2021, 2020, 2020, 2020, 2020, 2020, 2020, 2020,
    2020, 2020, 2020, 2021, 2021, 2021, 2021, 2021, 2021, 2021,
    2021, 2021, 2021, 2021, 2022, 2022, 2022, 2022, 2022, 2022,
    2022, 2022, 2022, 2022, 2022, 2022, 2023, 2023, 2023, 2023,
    2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2024, 2024,
    2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024
  ),
  Visit_hospital = c(
    "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No",
    "Yes", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "No",
    "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No",
    "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No",
    "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No",
    "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No"
  ),
  cost = c(
    20000, 35000, 10000, 30000, 11000, 18500, 9000, 8500, 8500, 19000,
    23500, 24500, 11500, 28500, 8500, 39000, 34000, 16500, 24000, 33500,
    37500, 14500, 9500, 25000, 36500, 10500, 9000, 33500, 27000, 15500,
    18500, 20500, 10000, 22500, 40000, 29000, 16000, 5500, 21500, 39000,
    13000, 8000, 30000, 35500, 9000, 12000, 5000, 28000, 29000, 28000,
    34500, 14000, 21500, 38000, 14000, 38500, 21500, 19000, 26000, 17000
  ),
  conf_cost = c(
    20025, 32500, 10000, 30900, 10700, 16800, 8525, 9050, 8625, 17275,
    23125, 24800, 11700, 26200, 8700, 38425, 33050, 17150, 24125, 32075,
    36875, 15750, 10000, 24200, 37075, 11425, 8200, 32050, 24425, 15850,
    19500, 20075, 9325, 24225, 38225, 31650, 14875, 5475, 20550, 37975,
    13500, 7925, 29400, 36450, 9875, 11625, 5075, 28625, 26950, 27850,
    34375, 12875, 21400, 34375, 14525, 36950, 21575, 19125, 26700, 18050
  )
)

##############################################################################
#FUNCTION - 1. Classify columns in a dataset to be either
# 1. a conf and raw pair to get difference of
# 2. Grouping variables [age, ethinciity, year]
# 3. Indicators for specfic datasets [If they've been to hospital]
##############################################################################

class_columns <- function(data, indicators = character(),
                          conf_prefix = "conf_") {
  cols <- names(data)
  
  if (anyDuplicated(cols)){
    stop("Dataset contains duplicate column names. Please check and rename columns")
  }
  
  conf_cols <- cols[startsWith(cols, conf_prefix)]
  
  original_names <- substring(
    conf_cols,
    nchar(conf_prefix) + 1
  )
  
  missing_pairs <- setdiff(original_names, cols)
  if (length(missing_pairs) > 0) {
    stop(
      "Missing original columns for ",
      paste(missing_pairs, collapse = ", ")
    )
  }
  
  pairs <- data.frame(
    name = original_names,
    conf_name = conf_cols,
    stringsAsFactors = FALSE
  )
  
  paired_cols <- c(pairs$name, pairs$conf_name)
  
  #STOP INCASE OF INDICATORS SELECTED NOT IN DATASET
  unknown_indicators <- setdiff(indicators, cols)
  
  if (length(unknown_indicators) > 0) {
    stop(
      "Unknown indictor columns: ",
      paste(unknown_indicators, collapse = ", ")
    )
  }
  #STOP IF CONF AND INDICATORS OVERLAP
  overlapping <- intersect(indicators, paired_cols)
  if (length(overlapping) > 0) {
    stop(
      "Raw and Conf paired Columns cannot also be indicators: ",
      paste(overlapping, collapse = ", ")
    )
  }
  
  groupings <- setdiff(
    cols,
    c(paired_cols, indicators)
  )
  
  ordered_pairs  <- as.vector(
    t(as.matrix(pairs[c("name", "conf_name")]))
  )
  
  column_order <- c(
    groupings, 
    indicators, 
    ordered_pairs
  )
  
  data <- data[, column_order, drop = FALSE]
  
  attr(data, "pairs") <- pairs
  
  
  return(data)
}

##############################################################################
#Test
classed_columns <- class_columns(data = test_data,
                                 indicators = c("Visit_hospital")
)

##############################################################################
#FUNCTION - 2. Calculate LOSS between raw and conf columns
# loss = amount in raw that is not in conf, per row (never negative).
# e.g. raw = 100, conf = 80 -> loss = 20.  raw = 100, conf = 105 -> loss = 0.
# NOTE: columns keep the "_dist" suffix so the rest of the pipeline is unchanged.
##############################################################################
distance <- function(data) {
  
  pair_table <- attr(data, "pairs")
  
  if (is.null(pair_table)) {
    stop("Data must be processed with class_columns() first.")
  }
  
  for (i in seq_len(nrow(pair_table))) {
    
    raw_col <- pair_table$name[i]
    conf_col <- pair_table$conf_name[i]
    
    data[[paste0(raw_col, "_dist")]] <-
      pmax(data[[raw_col]] - data[[conf_col]], 0)
  }
  return(data)
}
##############################################################################
#Test
distance(data = classed_columns)

##############################################################################
#Wrapper Function - Combined 1 and 2 together 
##############################################################################

class_distance <- function(data, indicators = character(),
                           conf_prefix = "conf_") {
  
  classified_data <- class_columns(
    data = data,
    indicators = indicators,
    conf_prefix = conf_prefix
  )
  
  distance_data <- distance(
    data = classified_data
  )
  
  return(distance_data)
}

##############################################################################

data_distance <- class_distance(
  data = test_data,
  indicators = c("Visit_hospital")
)

##############################################################################
# Plot Builders for forth function. 
##############################################################################

# Every plot type is a function returning one ggplot.
# All of them are built for every distance column automatically.
# To add a new type, just add another named entry here.
plot_builders <- list(
  
  line = function(result, variable, x_axis, group_label) {
    y_max <- max(result[[variable]], na.rm = TRUE)
    if (!is.finite(y_max)) y_max <- 0
    
    ggplot2::ggplot(
      result,
      ggplot2::aes(
        x = .data[[x_axis]],
        y = .data[[variable]],
        colour = subgroup,
        group = subgroup
      )
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::labs(
        title = paste(variable, "by", x_axis),
        subtitle = paste0(
          "Loss as % of raw total | grouped by ",
          group_label
        ),
        x = x_axis,
        y = "Loss (%)",
        colour = "Subgroup"
      ) +
      ggplot2::coord_cartesian(ylim = c(NA, y_max + 3)) +
      ggplot2::theme_minimal()
  },
  
  bar = function(result, variable, x_axis, group_label) {
    y_max <- max(result[[variable]], na.rm = TRUE)
    if (!is.finite(y_max)) y_max <- 0
    
    ggplot2::ggplot(
      result,
      ggplot2::aes(
        x = factor(.data[[x_axis]]),
        y = .data[[variable]],
        fill = subgroup
      )
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(
        title = paste(variable, "by", x_axis),
        subtitle = paste0(
          "Loss as % of raw total | grouped by ",
          group_label
        ),
        x = x_axis,
        y = "Loss (%)",
        fill = "Subgroup"
      ) +
      ggplot2::coord_cartesian(ylim = c(0, y_max + 3)) +
      ggplot2::theme_minimal()
  },
  
  totals = function(result, variable, x_axis, group_label) {
    
    # "ppl_dist_percentage" -> "ppl_dist_total"
    total_col <- sub("_percentage$", "_total", variable)
    
    ggplot2::ggplot(
      result,
      ggplot2::aes(
        x = factor(.data[[x_axis]]),
        y = .data[[total_col]],
        fill = subgroup
      )
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(
        title = paste(total_col, "by", x_axis),
        subtitle = paste0("Total loss (raw - conf) | grouped by ", group_label),
        x = x_axis,
        y = "Total loss",
        fill = "Subgroup"
      ) +
      ggplot2::theme_minimal()
  },
  smooth_columns = function(result, variable, x_axis, group_label) {
    
    # "ppl_dist_percentage" -> "ppl_raw_total"
    raw_col <- sub("_dist_percentage$", "_raw_total", variable)
    
    y_max <- max(result[[variable]], na.rm = TRUE)
    if (!is.finite(y_max) || y_max <= 0) y_max <- 1
    
    raw_max <- max(result[[raw_col]], na.rm = TRUE)
    if (!is.finite(raw_max) || raw_max <= 0) raw_max <- 1
    
    # Columns occupy a band below zero, 35% as tall as the line's range
    band <- 0.35 * y_max
    k    <- band / raw_max          # converts raw units to plot units
    
    
    
    # Smoothing needs a numeric x axis (e.g. year); otherwise join the points
    smooth_layer <- if (is.numeric(result[[x_axis]])) {
      ggplot2::geom_smooth(
        ggplot2::aes(y = .data[[variable]]),
        method = "loess", formula = y ~ x,
        method.args = list(degree = 1),
        span = 1, se = FALSE, linewidth = 1
      )
    } else {
      ggplot2::geom_line(
        ggplot2::aes(y = .data[[variable]]),
        linewidth = 1
      )
    }
    
    ggplot2::ggplot(
      result,
      ggplot2::aes(
        x = .data[[x_axis]],
        colour = subgroup,
        fill = subgroup,
        group = subgroup
      )
    ) +
      # columns underneath: raw total drawn downward from zero
      ggplot2::geom_col(
        ggplot2::aes(y = -.data[[raw_col]] * k),
        position = ggplot2::position_dodge(),
        alpha = 0.5,
        colour = NA
      ) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
      smooth_layer +
      ggplot2::geom_point(ggplot2::aes(y = .data[[variable]]), size = 2) +
      ggplot2::scale_y_continuous(
        # left axis: proportion (above zero only)
        breaks = pretty(c(0, y_max)),
        # right axis: raw total (below zero only)
        sec.axis = ggplot2::sec_axis(
          ~ -. / k,
          name = "Raw total (columns)",
          breaks = pretty(c(0, raw_max))
        )
      ) +
      ggplot2::coord_cartesian(ylim = c(-band * 1.05, y_max * 1.1)) +
      ggplot2::labs(
        title = paste(variable, "by", x_axis),
        subtitle = paste0(
          "Smoothed loss (% of raw total), columns = raw total | grouped by ",
          group_label
        ),
        x = x_axis,
        y = "Loss (%)",
        colour = "Subgroup",
        fill = "Subgroup"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }
)

##############################################################################
# FUNCTION 4. Ploting category against time
##############################################################################
aggregate_distances <- function(data,
                                x_axis,
                                groups) {
  
  # groups can be:
  #   - a character vector:            c("ethnicity", "region")  -> one grouping (interaction of all)
  #   - a list of character vectors:   list("ethnicity", c("ethnicity", "region"))  -> several groupings
  if (is.character(groups)) groups <- list(groups)
  
  if (!is.list(groups) ||
      length(groups) == 0 ||
      !all(vapply(groups, is.character, logical(1)))) {
    stop("groups must be a character vector or a list of character vectors.")
  }
  
  # Label each grouping (use list names if supplied, otherwise "a x b")
  group_labels <- vapply(groups, paste, character(1), collapse = " x ")
  if (!is.null(names(groups))) {
    has_name <- nzchar(names(groups))
    group_labels[has_name] <- names(groups)[has_name]
  }
  names(groups) <- group_labels
  
  # Identify distance columns
  distance_cols <- names(data)[grepl("_dist$", names(data))]
  
  if (length(distance_cols) == 0) {
    stop("No columns ending in '_dist' found.")
  }
  
  # Check required columns across ALL groupings
  required_cols <- unique(c(x_axis, unlist(groups)))
  missing_cols  <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    hint <- if (any(grepl(",", missing_cols))) {
      "\nA name contains a comma - did you write c(\"a, b\") instead of c(\"a\", \"b\")?"
    } else {
      ""
    }
    
    stop(
      "Missing columns: ",
      paste0("'", missing_cols, "'", collapse = ", "),
      hint,
      call. = FALSE
    )
  }
  
  # Match distance columns to raw columns
  raw_cols <- sub("_dist$", "", distance_cols)
  
  if (!all(raw_cols %in% names(data))) {
    stop("Could not find matching raw columns for all distance columns.")
  }
  
  percentage_cols <- paste0(distance_cols, "_percentage")
  
  # Run the full aggregation + plotting once per grouping
  results <- lapply(names(groups), function(group_label) {
    
    group_vars <- groups[[group_label]]
    
    # Group and total distance / raw values
    result <- data |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(unique(c(group_vars, x_axis)))
        )
      ) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(distance_cols),
          ~ sum(.x, na.rm = TRUE),
          .names = "{.col}_total"
        ),
        dplyr::across(
          dplyr::all_of(raw_cols),
          ~ sum(.x, na.rm = TRUE),
          .names = "{.col}_raw_total"
        ),
        n = dplyr::n(),
        .groups = "drop"
      )
    
    # Calculate percentage loss = total loss / raw total * 100
    for (variable in distance_cols) {
      
      raw_variable <- sub("_dist$", "", variable)
      
      result[[paste0(variable, "_percentage")]] <-
        ifelse(
          result[[paste0(raw_variable, "_raw_total")]] == 0,
          NA_real_,
          (result[[paste0(variable, "_total")]] /
             result[[paste0(raw_variable, "_raw_total")]]) * 100
        )
    }
    
    # Subgroup labels: interaction of every variable in this grouping
    result <- result |>
      dplyr::mutate(
        subgroup = interaction(
          dplyr::across(dplyr::all_of(group_vars)),
          drop = TRUE,
          sep = " / "
        )
      )
    
    # Build every plot type for every distance column, stored flat
    plot_grid <- expand.grid(
      variable = percentage_cols,
      type     = names(plot_builders),
      stringsAsFactors = FALSE
    )
    
    plots <- Map(
      function(variable, type) {
        plot_builders[[type]](result, variable, x_axis, group_label)
      },
      plot_grid$variable,
      plot_grid$type
    )
    
    names(plots) <- paste(plot_grid$variable, plot_grid$type, sep = "_")
    
    list(
      aggregated_data = result,
      plots = plots
    )
  })
  
  names(results) <- names(groups)
  
  results
}

################################################################################

results <- aggregate_distances(
  data = data_distance,
  x_axis = "year",
  groups = list("ethnicity",
                c("ethnicity", "region"))
)
names(results[["ethnicity"]]$plots)          # see everything that was built
results[["ethnicity"]]$plots[[8]] 
results[["ethnicity"]]$plots[[6]] # by position
results[["ethnicity x region"]]$plots[[7]]
################################################################################