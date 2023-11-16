# JT's functins:

jthom_ggtheme <- theme_minimal() +
  theme(
        plot.title = element_text(size = 14,
                                  hjust = 0.5),
        axis.ticks = element_line(),
        # Y-axis:
        axis.title.y = element_text(angle = 0,
                                    size = 12,
                                    vjust = 0.5),
        axis.text.y  = element_text(size = 11),
        # X-axis:
        axis.title.x = element_text(angle = 0,
                                    size = 12,
                                    vjust = 0.5),
        axis.text.x  = element_text(angle = 0,
                                    size = 11))

count_na <- function(df) {
  n_rows <- nrow(df)
  df_na <- sapply(df, function(x) sum(is.na(x))) # or colSums(is.na(df))
  if (any(df_na > 0)) {
    if ("tidyverse" %in% .packages()) {
      df_na |>
        tibble::as_tibble(rownames = c("Variable")) |>
        dplyr::rename(NA_count = value) |>
        dplyr::filter(NA_count > 0) |>
        dplyr::mutate(Percent = round(100 * NA_count / n_rows, 2)) |>
        dplyr::arrange(dplyr::desc(Percent))
    } else {
      df_na <- data.frame(Variable = names(df_na), NA_count = df_na)
      df_na <- subset(df_na, df_na$NA_count > 0)
      df_na$Percent <- round(100 * df_na$NA_count / n_rows, 2)
      df_na <- df_na[order(df_na$NA_count, decreasing = TRUE)]
      rownames(df_na) <- NULL
      df_na
    }
  } else {
    print("No missing values (NA) found.")
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