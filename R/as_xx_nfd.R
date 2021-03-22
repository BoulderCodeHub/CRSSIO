#' Convert `nfd` objects to other classes
#' 
#' `as.data.frame` converts `nfd`, `crss_nf`, and `crssi` objects to 
#' `data.frame`s. 
#' 
#' @param x Object inheriting from `nfd`. 
#' 
#' @param wide When `TRUE`, the sites are included as columns. When `FALSE` the
#'   sites are converted to a `site` column, making the data.frame longer. 
#'   
#' @param ... Other parameters passed to [base::as.data.frame()]
#' 
#' @return `data.frame` with columns for `date`, `trace`, `time_step`, and 
#'   `flow_type`. When `wide == TRUE`, there is another column for each site. 
#'   When `wide == FALSE`, `site`, and `value` columns. If `x` is a `crssi` 
#'   object, than the returned data.frame will also include Sacramento Year Type
#'   Index data as another column (`wide == TRUE`) or as another entry into 
#'   `site` (`wide == FALSE`).
#' 
#' @export
#' @rdname nfd_to_class
as.data.frame.nfd <- function(x, wide = TRUE, ...)
{
  df <- data.frame()
  nt <- n_trace(x)
  
  if (has_monthly(x)) {
    if (has_intervening(x, "monthly")) {
      tmp_df <- lapply(
        seq_len(nt), 
        function(i) trace_to_df(x, i, "intervening", "monthly", ...)
      )
      
      df <- dplyr::bind_rows(df, dplyr::bind_rows(tmp_df))
    }
    
    if (has_total(x, "monthly")) {
      tmp_df <- lapply(
        seq_len(nt), 
        function(i) trace_to_df(x, i, "total", "monthly", ...)
      )
      
      df <- dplyr::bind_rows(df, dplyr::bind_rows(tmp_df))
    }
  }
  
  if (has_annual(x)) {
    if (has_intervening(x, "annual")) {
      tmp_df <- lapply(
        seq_len(nt), 
        function(i) trace_to_df(x, i, "intervening", "annual", ...)
      )
      
      df <- dplyr::bind_rows(df, dplyr::bind_rows(tmp_df))
    }
    
    if (has_total(x, "annual")) {
      tmp_df <- lapply(
        seq_len(nt), 
        function(i) trace_to_df(x, i, "total", "annual", ...)
      )
      
      df <- dplyr::bind_rows(df, dplyr::bind_rows(tmp_df))
    }
  }
  
  col_order <- c("date", "trace", "flow_space", "time_step")
  sites <- colnames(df)
  sites <- sites[!(sites %in% col_order)]

  # switch to LONG? and reorder columns
  if (wide) {
    df <- df[, c(col_order, sites)]
  } else {
    df <- tidyr::pivot_longer(df, sites, names_to = "site")
    
    df <- df[,c(col_order, "site", "value")]
  }
  
  df$date <- zoo::as.yearmon(df$date)
  
  df
}

#' @export
#' @rdname nfd_to_class
as.data.frame.crssi <- function(x, wide = TRUE, ...) {

  sac <- sac_xts_to_df(x$sac_year_type, wide = wide, ...)
  x <- suppressMessages(as_crss_nf(x))
  x <- as.data.frame(x, wide = wide, ...)
  x$date <- as.character(x$date)

  if (wide) {
    x <- dplyr::left_join(x, sac, by = c("date", "trace"))
  } else {
    x <- dplyr::bind_rows(x, sac)
  }
  
  x$date <- zoo::as.yearmon(x$date)
  
  x
}

sac_xts_to_df <- function(x, wide, ...) {
  x <- as.data.frame(x, ...)
  ym <- rownames(x)
  rownames(x) <- NULL
  
  colnames(x) <- 1:ncol(x)
  x$date <- ym
  
  if (wide) {
    vv <- "sac_year_type"
  } else {
    vv <- "value"
  }
  
  x <- tidyr::pivot_longer(x, !c("date"), names_to = "trace", values_to = vv)
  x$trace <- as.integer(x$trace)
  
  if (!wide) {
    x$site <- "sac_year_type"
  }
  
  x
}

trace_to_df <- function(x, trace, flow_space, time_step, ...) {
  r <- as.data.frame(x[[time_step]][[flow_space]][[trace]], ...)
  ym <- rownames(r)
  rownames(r) <- NULL
  
  # add variables (attributes)
  r$date <- ym
  r$flow_space <- flow_space
  r$trace <- trace
  r$time_step <- time_step
  
  r
}