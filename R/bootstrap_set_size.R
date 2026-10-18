# Summarize set sizes from the existing bootstrap replicates.
summarize_bootstrap_set_size <- function(successful, conf) {

  per_replicate <- data.frame(
    replicate = seq_along(successful),

    status = vapply(
      successful,
      function(result) {

        if (is.null(result)) {
          "calculation_failed"
        } else if (anyNA(result$selected)) {
          "selection_undefined"
        } else {
          "defined"
        }
      },
      character(1)
    ),

    k = vapply(
      successful,
      function(result) {

        if (
          is.null(result) ||
          anyNA(result$selected)
        ) {
          NA_integer_
        } else {
          as.integer(sum(result$selected))
        }
      },
      integer(1)
    ),

    stringsAsFactors = FALSE
  )

  k <- per_replicate$k[
    per_replicate$status == "defined"
  ]

  bounds <- c(NA_real_, NA_real_)

  if (length(k) > 0L) {

    bounds <- stats::quantile(
      k,
      probs = c(
        (1 - conf) / 2,
        1 - (1 - conf) / 2
      ),
      names = FALSE,
      type = 7
    )

    counts <- table(k)

    frequencies <- data.frame(
      k = as.integer(names(counts)),
      n_replicates = as.integer(counts),
      frequency = as.numeric(counts) / length(k)
    )

  } else {

    frequencies <- data.frame(
      k = integer(),
      n_replicates = integer(),
      frequency = numeric()
    )
  }

  summary_table <- data.frame(
    n_requested = length(successful),
    n_defined = length(k),
    n_excluded = length(successful) - length(k),

    k_mean = if (length(k) > 0L) {
      mean(k)
    } else {
      NA_real_
    },

    k_sd = if (length(k) > 1L) {
      stats::sd(k)
    } else {
      NA_real_
    },

    k_median = if (length(k) > 0L) {
      stats::median(k)
    } else {
      NA_real_
    },

    k_min = if (length(k) > 0L) {
      min(k)
    } else {
      NA_integer_
    },

    k_lower = bounds[1L],
    k_upper = bounds[2L],

    k_max = if (length(k) > 0L) {
      max(k)
    } else {
      NA_integer_
    }
  )

  list(
    replicates = per_replicate,
    frequencies = frequencies,
    summary = summary_table
  )
}
