library(ggplot2)

stat_boxplot_custom <- function(mapping = NULL, data = NULL,
                                geom = "boxplot", position = "dodge",
                                ...,
                                qs = c(.05, .25, 0.5, 0.75, 0.95),
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplotCustom,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      qs = qs,
      ...
    )
  )
}

StatBoxplotCustom <- ggproto("StatBoxplotCustom", Stat,
                             required_aes = c("x", "y"),
                             non_missing_aes = "weight",
                             
                             setup_params = function(data, params) {
                               params$width <- ggplot2:::"%||%"(params$width, (resolution(data$x) * 0.75))
                               
                               if (is.double(data$x) && !ggplot2:::has_groups(data) && any(data$x != data$x[1L])) {
                                 warning(
                                   "Continuous x aesthetic -- did you forget aes(group=...)?",
                                   call. = FALSE)
                               }
                               
                               params
                             },
                             
                             compute_group = function(data, scales, width = NULL, na.rm = FALSE, qs = c(.05, .25, 0.5, 0.75, 0.95)) {
                               
                               if (!is.null(data$weight)) {
                                 mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                                 stats <- as.numeric(stats::coef(mod))
                               } else {
                                 stats <- as.numeric(stats::quantile(data$y, qs))
                               }
                               names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                               iqr <- diff(stats[c(2, 4)])
                               
                               outliers <- (data$y < stats[1]) | (data$y > stats[5])
                               #if (any(outliers)) {
                               #  stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
                               #}
                               
                               if (length(unique(data$x)) > 1)
                                 width <- diff(range(data$x)) * 0.9
                               
                               df <- as.data.frame(as.list(stats))
                               df$outliers <- list(data$y[outliers])
                               
                               if (is.null(data$weight)) {
                                 n <- sum(!is.na(data$y))
                               } else {
                                 # Sum up weights for non-NA positions of y and weight
                                 n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                               }
                               
                               df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                               df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
                               
                               df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                               df$width <- width
                               df$relvarwidth <- sqrt(n)
                               df
                             }
)
