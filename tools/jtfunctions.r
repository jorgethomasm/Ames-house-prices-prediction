# JT's functins:

jtggtheme <- theme_minimal() +
  theme(
        plot.title = element_text(size = 10,
                                  hjust = 0.5),
        axis.ticks = element_line(),
        # Y-axis:
        axis.title.y = element_text(angle = 0,
                                    size = 9,
                                    vjust = 0.5,
                                    face = "bold"),
        axis.text.y  = element_text(size = 8),
        # X-axis:
        axis.title.x = element_text(angle = 0,
                                    size = 9,
                                    vjust = 0.5,
                                    face = "bold"),
        axis.text.x  = element_text(angle = 0,
                                    size = 8))

count_na <- function(df) {
  n_rows <- nrow(df)
  df_na <- sapply(df, function(x) sum(is.na(x))) # "colSums(is.na(df))"
  if (any(df_na > 0)) {
    if ("tidyverse" %in% .packages()) {
      df_na |>
        tibble::as_tibble(rownames = c("Variable")) |>
        dplyr::rename(Count = value) |>
        dplyr::filter(Count > 0) |>
        dplyr::mutate(Percentage = round(100 * Count / n_rows, 2)) |>
        dplyr::arrange(dplyr::desc(Percentage))
    } else {
      df_na <- data.frame(Variable = names(df_na), Count = df_na)
      df_na <- subset(df_na, df_na$Count > 0)
      df_na$Percentage <- round(100 * df_na$Count / n_rows, 2)
      df_na <- df_na[order(df_na$Count, decreasing = TRUE)]
      rownames(df_na) <- NULL
      df_na
    }
  } else {
    print("No missing (NA) values found.")
  }
}

replace_na_with_median <- function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
}

inv_boxcox <- function(x, lambda) {
  if (lambda == 0) {
    exp(x)
  } else {
    (lambda * x + 1)^(1 / lambda)
  }
}

find_mode <- function(x) {
  unique_values <- unique(x)
  tab <- tabulate(match(x, unique_values))
  unique_values[tab == max(tab)]
}

# get mode function
# df_all |>
#   count(MasVnrType, name = "Count", sort = TRUE) |>
#   filter(Count == max(Count, na.rm = TRUE)) |>
#   pull(MasVnrType)