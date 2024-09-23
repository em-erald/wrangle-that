naPaste <- function(..., delimiter = "/", arrange = 1) {

  y = (list(...))

  # stopifnot()

  x = (sapply(y, function(vec) {NA %in% vec}))

  if (sum(x) == 0) {
    return(paste(..., sep = delimiter))
  } else {
    output = paste(..., sep = delimiter)

    for (vec in y) {
      idx = which(is.na(vec))
      for (i in idx) {
        output[i] <- NA
      }
    }

    return(output)
  }


  # intermediate_data <- cbind(vec1, vec2, combined_column) %>%
  #   as.data.frame() %>%
  #   arrange(vec1, vec2)

  ## TODO: Implement custom ordering using order_by param
  # order_by = 2 (default) -> arrange(vec1, vec2)
  # order_by = 1 -> arrange(vec2, vec1)

}

