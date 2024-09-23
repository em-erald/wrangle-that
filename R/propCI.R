propCI <- function(count, total, bound = c("lower", "upper", "both"), ci = 0.95) {

  result <- prop.test(count, total, correct = FALSE, conf.level = ci)
  interval <- result$conf.int

  bnd <- match.arg(bound)

  idx <- switch(bnd,
                lower = 1,
                upper = 2,
                both = c(1, 2))

  return(interval[idx])
}
