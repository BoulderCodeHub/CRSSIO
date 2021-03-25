#' Convert `nfd` objects to other classes
#' 
#' `as.data.frame` converts `nfd`, `crss_nf`, and `crssi` objects to 
#' `data.frame`s. 
#' 
#' @param x Object inheriting from `nfd`. 
#' 
#' @inheritParams base::as.data.frame
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
#' @examples 
#' nf <- crss_nf(
#'   CoRiverNF::monthlyInt["2000/2002"],
#'   flow_space = "intervening",
#'   time_step = "monthly"
#'  )
#'  
#'  # keep sites as columns
#'  as.data.frame(nf)
#'  
#'  # moves sites to "site" variable
#'  as.data.frame(nf, wide = FALSE)
#' 
#' @export
#' @rdname nfd_to_class
as.data.frame.nfd <- function(x, row.names = NULL, optional = FALSE, ..., 
                              wide = TRUE)
{
  df <- data.frame()
  nt <- n_trace(x)
  
  if (has_monthly(x)) {
    if (has_intervening(x, "monthly")) {
      tmp_df <- lapply(
        seq_len(nt), 
        function(i) {
          trace_to_df(x, i, "intervening", "monthly", optional, ...)
        }
      )
      
      df <- dplyr::bind_rows(df, dplyr::bind_rows(tmp_df))
    }
    
    if (has_total(x, "monthly")) {
      tmp_df <- lapply(
        seq_len(nt), 
        function(i) {
          trace_to_df(x, i, "total", "monthly", optional, ...)
        }
      )
      
      df <- dplyr::bind_rows(df, dplyr::bind_rows(tmp_df))
    }
  }
  
  if (has_annual(x)) {
    if (has_intervening(x, "annual")) {
      tmp_df <- lapply(
        seq_len(nt), 
        function(i) {
          trace_to_df(x, i, "intervening", "annual", optional, ...)
        }
      )
      
      df <- dplyr::bind_rows(df, dplyr::bind_rows(tmp_df))
    }
    
    if (has_total(x, "annual")) {
      tmp_df <- lapply(
        seq_len(nt), 
        function(i) {
          trace_to_df(x, i, "total", "annual", optional, ...)
        }
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

  if (!missing(row.names) && !is.null(row.names)) {
    rownames(df) <- row.names
  }
  
  df
}

#' @examples 
#' # crssi objects have sacramento year type data in them, which is also 
#' # included in the data frame
#' 
#' sac <- sac_year_type_get(internal = TRUE)["2000/2002"]
#' in_data <- crssi(nf, sac, scen_number = 1.20002002)
#' 
#' as.data.frame(in_data)
#' 
#' @export
#' @rdname nfd_to_class
as.data.frame.crssi <- function(x, row.names = NULL, optional = FALSE, ..., 
                                wide = TRUE) {
  sac <- sac_xts_to_df(
    x$sac_year_type, optional = optional, wide = wide, 
    ...
  )
  x <- suppressMessages(as_crss_nf(x))
  x <- as.data.frame(x, row.names = row.names, optional = optional, 
                     wide = wide, ...)
  x$date <- as.character(x$date)

  if (wide) {
    x <- dplyr::left_join(x, sac, by = c("date", "trace"))
  } else {
    x <- dplyr::bind_rows(x, sac)
  }
  
  x$date <- zoo::as.yearmon(x$date)
  
  x
}

sac_xts_to_df <- function(x, optional, wide, ...) {
  x <- as.data.frame(x, optional = optional, ...)
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

trace_to_df <- function(x, trace, flow_space, time_step, optional, ...) {
  r <- as.data.frame(
    x[[time_step]][[flow_space]][[trace]], optional = optional, ...
  )
  ym <- rownames(r)
  rownames(r) <- NULL
  
  # add variables (attributes)
  r$date <- ym
  r$flow_space <- flow_space
  r$trace <- trace
  r$time_step <- time_step
  
  r
}