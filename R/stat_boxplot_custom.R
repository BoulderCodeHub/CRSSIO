# modified from
# https://github.com/tidyverse/ggplot2/blob/master/R/stat-boxplot.r
# now takes qs argument instead of coef to extend the whiskers to a specific 
# percentile

#' A box and whiskers plot that uses percentiles instead of IQR
#' 
#' `stat_boxplot_custom()` modifies [ggplot2::stat_boxplot()] so that it 
#' computes the extents of the whiskers based on specified percentiles, rather 
#' than a multiple of the IQR. 
#' 
#' The upper and lower whiskers extend to the first, and last entries of the 
#' `qs` parameter, respectively. Data beyond the whiskers are "outliers".
#' 
#' @param qs The percentiles that are used to create the lower whisker, lower
#'   hinge, middle bar, upper hinge, and upper whisker, respectively. The lower 
#'   and upper whiskers default to the 5th and 95th percentiles. The hinges 
#'   default to the 25th and 75th percentiles, and the middle bar defaults to 
#'   the median. These  values should be in increasing order, i.e., the whisker 
#'   should be a smaller percentile than the lower hinge, and can only span the 
#'   values from 0 to 1 (inclusive).
#'   
#' @inheritParams ggplot2::stat_boxplot
#' 
#' @examples 
#' 
#' library(ggplot2)
#' p <- ggplot(mpg, aes(class, hwy))
#' 
#' # show whiskers at 5th and 95th percentiles
#' p + stat_boxplot_custom(qs = c(.05, .25, .5, .75, .95))
#' 
#' # show whiskers at 10th and 90th percentiles
#' p + stat_boxplot_custom(qs = c(.1, .25, .5, .75, .9))
#' 
#' @export
stat_boxplot_custom <- function(mapping = NULL, data = NULL,
                                geom = "boxplot", position = "dodge2",
                                ...,
                                qs = c(.05, .25, 0.5, 0.75, 0.95),
                                na.rm = FALSE,
                                orientation = NA,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  assert_that(
    length(qs) == 5 && is.numeric(qs),
    msg = "`qs` should be a numeric vector with 5 values."
  )
  
  assert_that(
    all(qs == sort(qs)), 
    msg = "`qs` should be provided in ascending order."
  )
  
  assert_that(
    all(qs <= 1) && all(qs >= 0),
    msg = "`qs` should only span values [0, 1]."
  )
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplotCustom,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      orientation = orientation,
      qs = qs,
      ...
    )
  )
}

#' @rdname stat_boxplot_custom
#' @usage NULL
#' @format NULL
#' @export
StatBoxplotCustom <- ggplot2::ggproto("StatBoxplotCustom", ggplot2::Stat,
  required_aes = c("y|x"),
  non_missing_aes = "weight",
  # either the x or y aesthetic will get dropped during
  # statistical transformation, depending on the orientation
  dropped_aes = c("x", "y", "weight"),
  setup_data = function(self, data, params) {
    data <- ggplot2::flip_data(data, params$flipped_aes)
    data$x <- ggplot2:::"%||%"(data$x, 0)
    data <- ggplot2::remove_missing(
      data,
      na.rm = params$na.rm,
      vars = "x",
      name = "stat_boxplot_custom"
    )
    ggplot2::flip_data(data, params$flipped_aes)
  },
  
  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params, 
                                          main_is_orthogonal = TRUE,
                                          group_has_equal = TRUE,
                                          main_is_optional = TRUE)
    data <- ggplot2::flip_data(data, params$flipped_aes)
    
    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      abort("stat_boxplot() requires an x or y aesthetic.")
    }
    
    params$width <- ggplot2:::"%||%"(
      params$width, 
      (ggplot2::resolution(ggplot2:::"%||%"(data$x, 0) * 0.75))
    ) 
    
    if (!ggplot2:::is_mapped_discrete(data$x) && is.double(data$x) && 
        !ggplot2:::has_groups(data) && any(data$x != data$x[1L])) {
      rlang::warn(glue::glue(
        "Continuous {flipped_names(params$flipped_aes)$x} aesthetic -- did you forget aes(group=...)?"
      ))
    }
    
    params
  },
  
  extra_params = c("na.rm", "orientation"),
   
  compute_group = function(data, scales, width = NULL, na.rm = FALSE, 
                        qs = c(.05, .25, 0.5, 0.75, 0.95), flipped_aes = FALSE) {
    
    data <- ggplot2::flip_data(data, flipped_aes)
      
    if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
      stats <- as.numeric(stats::coef(mod))
    } else {
      stats <- as.numeric(stats::quantile(data$y, qs))
    }
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    iqr <- diff(stats[c(2, 4)])
    
    outliers <- (data$y < stats[1]) | (data$y > stats[5])

    if (vctrs::vec_unique_count(data$x) > 1)
      width <- diff(range(data$x)) * 0.9
    
    df <- ggplot2:::data_frame0(!!!as.list(stats))
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
    df$flipped_aes <- flipped_aes
    ggplot2::flip_data(df, flipped_aes)
  }
)
