addPropCICols <- function(df, n_col = "n", total_col = "total",
                          interval_colnames = c("lwr", "upr"), ...) {

  conf_int = 0.95
  if ("conf.level" %in% names(list(...))) {
    conf_int <- list(...)[["conf.level"]]
  }

  stopifnot(
    "Error: Two column names must be provided to `interval_colnames`." =
      length(interval_colnames) == 2)

  output_df <- df
  output_df$prop <- output_df[, n_col] / output_df[, total_col]

  for (idx in 1:nrow(output_df)) {
    .x <- output_df[idx, n_col]
    .y <- output_df[idx, total_col]

    output_df[idx, interval_colnames[1]] <- propCI(.x, .y, bound = "lower", ci = conf_int)
    output_df[idx, interval_colnames[2]] <- propCI(.x, .y, bound = "upper", ci = conf_int)
  }

  return(output_df)


  # LEGACY TIDYR CODE
  #   ungroup() %>%
  #   mutate(prop = .[[{{ n_col }}]] / .[[{{ total_col }}]],
  #          '{ interval_colnames[1] }' := map2_dbl(.[[{{ n_col }}]], .[[{{ total_col }}]],
  #                                                 .f = ~propCI(.x, .y, bound = "lower", ci = conf_int)),
  #          '{ interval_colnames[2] }' := map2_dbl(.[[{{ n_col }}]], .[[{{ total_col }}]],
  #                                                 .f = ~propCI(.x, .y, bound = "upper", ci = conf_int)))
  #
  # return(output_df)

}

