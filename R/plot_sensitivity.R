#' @title Convert raw sensitivity output to a long data frame
#'
#' @description
#' Reshapes the result of \code{run_sensitivity(output_mode = "raw")} into one
#' long data frame (one row per parameter step x timestep) at a single depth,
#' ready for custom plotting or summaries. \code{plot_sensitivity()} uses it
#' internally.
#'
#' @param res The list returned by \code{run_sensitivity()} with
#'   \code{output_mode = "raw"}.
#' @param depth Numeric or \code{NULL}. Depth to extract. The nearest available
#'   output depth is used. If \code{NULL} (default), all depths are returned.
#' @param var Character or integer. Which variable to use when a step's
#'   \code{output} holds several (one data frame per entry of \code{vars}):
#'   its name or position. Default \code{1}.
#'
#' @return A data frame with columns \code{datetime}, \code{depth},
#'   \code{value}, \code{iteration} (position of the parameter step) and
#'   \code{param_value} (the parameter value used in that step). When
#'   \code{depth} is given, the depth actually used is stored in the
#'   \code{"depth_used"} attribute. Steps with no usable output are skipped
#'   with a warning.
#'
#' @examples
#' \dontrun{
#' res <- run_sensitivity("kc", calib_setup, yaml_file = "Output.yaml",
#'                        model_dir = "GOTM-Selmaprotbas", n_steps = 10,
#'                        model = "GOTM-Selmaprotbas", output_mode = "raw",
#'                        vars = "selmaprotbas_DO_mg")
#' long <- sensitivity_to_long(res, depth = 5)
#' }
#'
#' @export
sensitivity_to_long <- function(res, depth = NULL, var = 1) {
  if (!is.list(res) || length(res) == 0L) {
    stop("'res' must be the non-empty list returned by run_sensitivity().", call. = FALSE)
  }
  has_output <- vapply(res, function(step) !is.null(step[["output"]]), logical(1))
  if (!any(has_output)) {
    stop("'res' has no 'output' elements. sensitivity_to_long()/plot_sensitivity() ",
         "need run_sensitivity(..., output_mode = \"raw\", vars = ...).", call. = FALSE)
  }

  pieces <- lapply(seq_along(res), function(i) {
    df <- res[[i]][["output"]]
    if (is.list(df) && !is.data.frame(df)) df <- df[[var]]
    depth_cols <- if (is.data.frame(df)) grep("^Depth_", names(df), value = TRUE) else character(0)
    if (!is.data.frame(df) || nrow(df) == 0L || length(depth_cols) == 0L ||
        !"datetime" %in% names(df)) {
      warning("Step ", i, " (param_value = ", res[[i]][["param_value"]],
              ") has no usable output -- skipping.", call. = FALSE)
      return(NULL)
    }
    data.frame(
      datetime    = rep(df[["datetime"]], times = length(depth_cols)),
      depth       = rep(as.numeric(sub("^Depth_", "", depth_cols)), each = nrow(df)),
      value       = unlist(df[depth_cols], use.names = FALSE),
      iteration   = i,
      param_value = res[[i]][["param_value"]],
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, pieces)
  if (is.null(long)) {
    stop("None of the sensitivity steps produced usable output.", call. = FALSE)
  }

  if (!is.null(depth)) {
    available <- unique(long$depth)
    depth_used <- available[which.min(abs(available - depth))]
    long <- long[long$depth == depth_used, , drop = FALSE]
    attr(long, "depth_used") <- depth_used
  }
  rownames(long) <- NULL
  long
}

#' @title Plot a sensitivity envelope from run_sensitivity() output
#'
#' @description
#' Plots how a model variable responds to a parameter that
#' \code{run_sensitivity(output_mode = "raw")} stepped through its range: one
#' thin grey line per parameter value, a shaded band for the chosen quantile
#' range across steps, and the median line. A thin band means the parameter
#' has little effect on this variable at this depth and is a poor calibration
#' candidate.
#'
#' @inheritParams sensitivity_to_long
#' @param ylab,title Character or \code{NULL}. Axis label and plot title.
#'   Defaults to the variable name (when known) and \code{"Sensitivity at
#'   <depth> m"}.
#' @param quantiles Numeric length 2. Lower and upper quantile of the shaded
#'   band. Default \code{c(0.05, 0.95)}.
#' @param show_runs Logical. Draw the individual runs underneath. Default
#'   \code{TRUE}.
#'
#' @return A \code{ggplot} object. For the underlying numbers, summarise the
#'   output of \code{sensitivity_to_long()} yourself.
#'
#' @examples
#' \dontrun{
#' res <- run_sensitivity("kc", calib_setup, yaml_file = "Output.yaml",
#'                        model_dir = "GOTM-Selmaprotbas", n_steps = 10,
#'                        model = "GOTM-Selmaprotbas", output_mode = "raw",
#'                        vars = "selmaprotbas_DO_mg")
#' plot_sensitivity(res, depth = 5, ylab = "DO (mg/m3)")
#' }
#'
#' @export
plot_sensitivity <- function(res, depth = NULL, var = 1, ylab = NULL, title = NULL,
                             quantiles = c(0.05, 0.95), show_runs = TRUE) {
  if (!is.numeric(quantiles) || length(quantiles) != 2L || anyNA(quantiles) ||
      any(quantiles < 0 | quantiles > 1) || quantiles[1] >= quantiles[2]) {
    stop("'quantiles' must be two increasing numbers between 0 and 1.", call. = FALSE)
  }
  long <- sensitivity_to_long(res, depth = depth, var = var)

  # One row per timestep: quantile band + median across parameter steps.
  dt_u <- unique(long$datetime)
  grp  <- match(long$datetime, dt_u)
  qfun <- function(p) as.numeric(tapply(long$value, grp, stats::quantile, probs = p, na.rm = TRUE))
  env <- data.frame(
    datetime = dt_u,
    q_lo     = qfun(quantiles[1]),
    q_hi     = qfun(quantiles[2]),
    median   = qfun(0.5)
  )

  if (is.null(ylab)) {
    first_out <- res[[which(!vapply(res, function(s) is.null(s[["output"]]), logical(1)))[1]]][["output"]]
    ylab <- if (is.list(first_out) && !is.data.frame(first_out) &&
                !is.null(names(first_out)) && nzchar(names(first_out)[1])) {
      if (is.character(var)) var else names(first_out)[var]
    } else {
      "Value"
    }
  }
  if (is.null(title)) {
    depth_used <- attr(long, "depth_used")
    title <- if (!is.null(depth_used)) {
      paste0("Sensitivity at ", format(depth_used), " m")
    } else {
      "Sensitivity"
    }
  }

  p <- ggplot2::ggplot()
  if (isTRUE(show_runs)) {
    p <- p + ggplot2::geom_line(
      data = long, ggplot2::aes(datetime, value, group = iteration),
      color = "grey70", alpha = 0.25, linewidth = 0.3
    )
  }
  p <- p +
    ggplot2::geom_ribbon(data = env, ggplot2::aes(datetime, ymin = q_lo, ymax = q_hi),
                         fill = "steelblue", alpha = 0.25) +
    ggplot2::geom_line(data = env, ggplot2::aes(datetime, median),
                       color = "steelblue", linewidth = 0.8) +
    ggplot2::labs(
      x = NULL, y = ylab, title = title,
      caption = sprintf("Band: %g-%g%% quantile range across %d parameter values",
                        100 * quantiles[1], 100 * quantiles[2],
                        length(unique(long$iteration)))
    ) +
    ggplot2::theme_minimal()
  p
}

utils::globalVariables(c("datetime", "value", "iteration", "q_lo", "q_hi", "median"))
