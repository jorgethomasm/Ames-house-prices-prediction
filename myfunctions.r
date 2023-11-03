# Jorge's functions:

count_na <- function(df) {
  n_rows <- nrow(df)
  df_na <- sapply(df, function(x) sum(is.na(x)))

  # Add conditional if sum > 0

  if ("tidyverse" %in% .packages()) {
    df_na <- tibble::as_tibble(df_na, rownames = c("Variable")) |>
      dplyr::rename(Count = value) |>
      dplyr::filter(Count > 0) |>
      dplyr::mutate(Percentage = round(100 * Count / n_rows, 2)) |>
      dplyr::arrange(dplyr::desc(Percentage))
  } else {
    df_na <- as.data.frame(df_na)
    colnames(df_na) <- c("Variable", "Count")
    df_na <- subset(df_na, Count > 0)
    df_na$Percentage <- round(100 * Count / n_rows, 2)
    df_na <- df_na[order(df_na$Count, decreasing = TRUE)]
  }
  df_na

  # else {"No missing (NA) values found."}
}
