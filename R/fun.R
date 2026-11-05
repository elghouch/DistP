#' Plot Standard Probability Densities with an Optional Shaded Area
#'
#' @description
#' `PlotDens()` draws the density of a normal distribution, a Student's t
#' distribution, or both overlaid. If the arguments `a` and `b` are supplied,
#' the area between them is shaded and the corresponding probability
#' is reported in the legend label.
#'
#' @param mean A _single numeric_ value giving the mean of the normal
#'   distribution. Defaults to `0`. Ignored when `dist = "t"`.
#' @param sd A _single positive numeric_ value giving the standard deviation
#'   of the normal distribution. Defaults to `1`. Ignored when `dist = "t"`.
#' @param df A _single positive numeric_ value giving the degrees of freedom
#'   of the Student's t distribution. Defaults to `1`. Ignored when
#'   `dist = "norm"`.
#' @param a Optional numeric value giving one bound of the interval over
#'   which the density is shaded. Must be supplied together with `b`;
#'   if either is `NULL`, no shading is performed. Values outside the
#'   plotted x-range are silently clamped to that range.
#' @param b Optional numeric value giving the other bound of the interval.
#'   Must be supplied together with `a`. The roles of `a` and `b` are
#'   symmetric: the interval is always `[min(a, b), max(a, b)]`, so the
#'   user does not need to order them.
#' @param dist A character string selecting the distribution to draw.
#'   One of:
#'   * `"norm"` (default) — normal distribution only;
#'   * `"t"` — Student's t distribution only;
#'   * `"both"` — the normal and the t distributions overlaid on the same
#'     panel, with distinct colours and a shared dashed boundary for the
#'     shaded interval.
#'
#' @return
#' A `ggplot` object. Because the return
#' value is a regular ggplot, it can be printed directly or further
#' modified, e.g. `PlotDens() + ggplot2::labs(title = "My plot")`.
#'
#' @details
#' The x-axis limits are chosen automatically so that the plotted density
#' is fully visible:
#' * for `dist = "norm"`, `mean ± 4 * sd`;
#' * for `dist = "t"`, `[-5, 5]` when `df <= 2` and `[-4, 4]` otherwise;
#' * for `dist = "both"`, the union of the two ranges above.
#'
#' @examples
#' # Normal distribution, no shading
#' PlotDens()
#'
#' # Normal distribution with a shaded central interval
#' PlotDens(a = -1, b = 1)
#'
#' # Student's t distribution
#' PlotDens(dist = "t", df = 5)
#'
#' # Student's t with a shaded area (bounds given in any order)
#' PlotDens(dist = "t", df = 5, a = 1, b = -1)
#'
#' # Both distributions overlaid, with a shared shaded interval
#' PlotDens(dist = "both", df = 5, a = -1, b = 1)
#'
#' # Shifted and scaled normal overlaid with a t distribution
#' PlotDens(mean = 2, sd = 0.5, df = 10, a = 1, b = 3, dist = "both")
#'
#' # The result is a regular ggplot object
#' PlotDens(a = -1, b = 1) + ggplot2::labs(title = "Standard normal")
#'
#' @importFrom ggplot2 ggplot stat_function scale_x_continuous geom_segment
#' @importFrom ggplot2 aes labs theme_minimal theme element_text unit
#' @importFrom stats dnorm dt pnorm pt qt
#'
#' @export

PlotDens <- function(mean = 0, sd = 1, df = 1, a = NULL, b = NULL, dist = "norm") {

  if (dist == "norm") {
    from <- mean - 4 * sd
    to <- mean + 4 * sd
    args <- list(mean = mean, sd = sd)
    lab <- paste("Normal ", "(mean = ", mean, ", sd = ", sd, ")", sep = "")
    dfun <- dnorm
    pfun <- pnorm
  }

  if (dist == "t") {
    from <- ifelse(df <= 2, -5, -4)
    to <- ifelse(df <= 2, 5, 4)
    args <- list(df = df)
    lab <- paste("Student's t-distribution ", "(df = ", df, ")", sep = "")
    dfun <- dt
    pfun <- pt
  }

  if (dist == "both") {
    from <- min(mean - 4 * sd,
      ifelse(df <= 2, -5, -4))
    to   <- max(mean + 4 * sd,
      ifelse(df <= 2,  5, 4))
    args_n <- list(mean = mean, sd = sd)
    args_t <- list(df = df)
    lab_n <- paste("Normal ", "(mean = ", mean, ", sd = ", sd, ")", sep = "")
    lab_t <- paste("Student's t-distribution ", "(df = ", df, ")", sep = "")
  }

  p <- ggplot() +
    scale_x_continuous(
      limits = c(from, to),
      breaks = seq(from, to, length.out = 10) |> pretty())  +
    labs(y = "density(x)", x = "x")

  if (is.null(a) || is.null(b)) {
    if (dist == "norm" || dist == "t") gg <- stat_function(fun = dfun, args = args, linewidth = 1, mapping = aes(color = lab))
    if (dist == "both")
      gg <- list(
        stat_function(fun = dnorm, args = args_n, linewidth = 1, mapping = aes(color = lab_n)),
        stat_function(fun = dt, args = args_t, linewidth = 1, mapping = aes(color = lab_t))
      )
  } else {

    if (a < from) a <- from
    if (a > to) a <- to
    if (b < from) b <- from
    if (b > to) b <- to

    l <- min(a, b)
    u <- max(a, b)

    if (dist == "norm" || dist == "t") {
      lab <- paste(lab, "\nProb. shaded area = ", round(do.call(pfun, c(q = u, args)) - do.call(pfun, c(q = l, args)), 3))
      gg <- list(
        stat_function(fun = dfun, args = args, linewidth = 1, mapping = aes(color = lab)),
        stat_function(fun = dfun, args = args, geom = "area", fill = "red", alpha = 0.2, xlim = c(l, u)),
        geom_segment(aes(x = c(l, u), y = 0, xend = c(l, u), yend = c(do.call(dfun, c(x = l, args)), do.call(dfun, c(x = u, args)))), linetype = "dashed", linewidth = 0.5)
      )
    }
    if (dist == "both") {
      lab_n <- paste(lab_n, "\nProb. shaded area = ", round(pnorm(u, mean = mean, sd = sd) - pnorm(l, mean = mean, sd = sd), 3))
      lab_t <- paste(lab_t, "\nProb. shaded area = ", round(pt(u, df = df) - pt(l, df = df), 3))
      gg <- list(
        stat_function(fun = dnorm, args = args_n, linewidth = 1, mapping = aes(color = lab_n)),
        stat_function(fun = dnorm, args = args_n, geom = "area", fill = "red", alpha = 0.2, xlim = c(l, u)),
        stat_function(fun = dt, args = args_t, linewidth = 1, mapping = aes(color = lab_t)),
        stat_function(fun = dt, args = args_t, geom = "area", fill = "blue", alpha = 0.2, xlim = c(l, u)),
        geom_segment(aes(x = c(l, u), y = 0, xend = c(l, u), yend = c(max(dnorm(l, mean = mean, sd = sd), dt(l, df = df)), max(dnorm(u, mean = mean, sd = sd), dt(u, df = df)))), linetype = "dashed", linewidth = 0.5)
      )
    }
  }

  p + gg +
    labs(color = NULL) +
    theme_minimal() +
    theme(legend.position = "top") +
    theme(legend.text = element_text(size = 12))  +
    theme(legend.key.width = unit(2, "cm"))
}
