#' Extract subsets from nfd objects
#' 
#' Extract subsets from `nfd` (and `crss_nf`, and `crssi`) objects by time, 
#' trace, site, flow space, and/or timestep dimensions.
#' 
#' When calling `nfd_extract()` not all dimensions have to be specified. When 
#' dimensions are not specified, all data for that dimension are returned. 
#' 
#' Times (`i`) can be specified by row index or ISO-8601 style character ranges. 
#' For example: `"2000/2002"` would return all data for 2000-2002. See 
#' \code{\link[xts]{[.xts}} for more details.
#' 
#' When extracting from a [crssi] object, all sites must remain. Additionally, 
#' there must be intervening monthly flow remaining after the extraction is 
#' complete, or an error will post. For [crss_nf] objects, the extraction is
#' less strict, and will return a [nfd] object instead of erroring. 
#' 
#' @param x An object inheriting from [nfd].
#' 
#' @param i The times to extract. Can be numeric or ISO-8601 style character
#'   range. See details.
#' 
#' @param j Traces to extract. Numeric. 
#' 
#' @param k Sites to extract. Numeric or names of sites. Must be missing, or all
#'   sites if extracting from `crssi` objects.
#' 
#' @param l The flow space. "intervening", "total", or 
#'   `c("total", "intervening")`.
#' 
#' @param m The time step to get. "annual", "monthly", or 
#'   `c("annual", "monthly")`.
#' 
#' @return `nfd_extract()` returns an `nfd` object if `x` is an `nfd` object. 
#' If `x` is a `crss_nf` object, a `crss_nf` object is returned, unless the 
#' extraction results in an invalid `crss_nf` object, i.e., there are less than
#' the required 29 sites and/or the time_step or flow_space no longer includes
#' monthly intervening data.
#' 
#' @export
nfd_extract <- function(x, i, j, k, l, m) 
{
  UseMethod("nfd_extract")
}

#' @export
nfd_extract.nfd <- function(x, i, j, k, l, m) 
{
  assert_that(is_nfd(x))
  year_att <- attr(x, "year")
  
  # time -------------------------------
  if (missing(i)) {
    # TODO have to handle i_mon and i_ann
    # and specifying by row numbers: 1:40
    # or via xts: "1906/1999" or "1906-01/1999-12"
    i_mon <- ""
    i_ann <- ""
  } else {
    if (is.numeric(i)) {
      assert_that(
        !(has_annual(x) && has_monthly(x)), 
        msg = paste0(
          "nfd has annual and monthly data, so extracting time by row index is ambiguous.\n",
          "Try extracting by date element instead. See help('[.xts', package = 'xts')"  
        )
      )
      
      if (has_annual(x)) {
        assert_that(max(i) <= n_years(x))
        i_ann <- i
        i_mon <- NULL
      } else {
        assert_that(max(i) <= n_months(x))
        i_ann <- NULL
        i_mon <- i
      }
      
    } else {
      assert_that(is.character(i) && length(i) == 1)
      i_mon <- i
      i_ann <- i
    }
  }
  
  # traces ----------
  if (missing(j))
    j <- seq(n_trace(x))
  else 
    assert_that(is.numeric(j) && max(j) <= n_trace(x))
  
  # site ------------
  # can extract by column index, or by name
  if (missing(k)) {
    k <- seq(n_sites(x))
  } else if (is.character(k)) {
    k_miss <- k[!(k %in% sites(x))]
    
    if (length(k_miss) > 0)
      stop(
        "Invalid site(s) to extract by.\n", 
        "Site(s): ", paste(k_miss, collapse = ","), 
        "\ndo not exist in nfd object."
      )
    
  } else if (is.numeric(k)) {
    assert_that(
      all(k %in% seq(n_sites(x))), 
      msg = "all k values should be valid site indeces."
    )
  } else {
    stop("`k` should be either a numeric or character vector if specified.")
  }
  
  # flow_space ------------
  if (missing(l)) {
    l <- c()
    if (has_intervening(x) || has_intervening(x, "monthly")) 
      l <- c(l, "intervening")
    if (has_total(x) || has_total(x, "monthly"))
      l <- c(l, "total")
  } else 
    assert_that(all(l %in% c("intervening", "total")))
  
  # timestep --------------
  if (missing(m)) {
    m <- c()
    if (has_annual(x))
      m <- c(m, "annual")
    if (has_monthly(x))
      m <- c(m, "monthly")
  } else
    assert_that(all(m %in%  c("monthly", "annual")))
  
  # subset -------------  
  # build new nfd that only has specified monthly/annual and total/intervening 
  # data
  is_monthly <- "monthly" %in% m
  is_annual <- "annual" %in% m
  is_int <- "intervening" %in% l
  is_tot <- "total" %in% l
  
  # ensure that i_mon and i_ann are NULL if we dont' have annual/monthly data
  if (!is_monthly)
    i_mon <- NULL
  
  if (!is_annual)
    i_ann <- NULL
  
  mon_int <- mon_tot <- ann_int <- ann_tot <- NULL
  if (is_monthly && is_int)
    mon_int <- x[["monthly"]][["intervening"]]
  
  if (is_monthly && is_tot)
    mon_tot <- x[["monthly"]][["total"]]
  
  if (is_annual && is_int)
    ann_int <- x[["annual"]][["intervening"]]
  
  if (is_annual && is_tot)
    ann_tot <- x[["annual"]][["total"]]
  
  # then reduce number of traces while simultaneously selecting by time and site
  # unless subset wants all traces, all timesteps, all sites
  if (!keep_all_traces(j, n_trace(x)) || 
      !(keep_all_ts(i_mon, n_months(x)) && keep_all_ts(i_ann, n_years(x))) || 
      !keep_all_sites(k, n_sites(x))) {
    
    all_traces <- seq(n_trace(x))
    
    if (is_monthly && is_int) {
      #mon_int[[trace]] <- subset_trace(mon_int, i_mon, trace, k, keep_trace)
      mon_int <- lapply(all_traces, function(trace) {
        subset_trace(mon_int, i_mon, trace, k, trace %in% j)
      })
    }
    
    if (is_monthly && is_tot) {
      mon_tot <- lapply(all_traces, function(trace) {
        subset_trace(mon_tot, i_mon, trace, k, trace %in% j)
      })
    }
    
    if (is_annual && is_int) {
      ann_int <- lapply(all_traces, function(trace) {
        subset_trace(ann_int, i_ann, trace, k, trace %in% j)
      })
    }
    
    if (is_annual && is_tot) {
      ann_tot <- lapply(all_traces, function(trace) {
        subset_trace(ann_tot, i_ann, trace, k, trace %in% j)
      })
    }
    
  }
  
  out <- new_nfd(remove_traces(mon_int), remove_traces(mon_tot), 
                 remove_traces(ann_int), remove_traces(ann_tot), year_att)
  
  out
}

#' @export
nfd_extract.crss_nf <- function(x, i, j, k, l, m)
{
  x <- nfd_extract.nfd(x, i, j, k, l, m)
  
  x <- tryCatch(
    {
      tmp <- crss_nf_validate(x)
      class(tmp) <- c("crss_nf", "nfd")
      tmp
    }, 
    error = function(cond) {
      x
    },
    finally = NULL
  )
  
  x
}

#' @export
nfd_extract.crssi <- function(x, i, j, k, l, m)
{
  if (!missing(k)) {
    assert_that(
      identical(k, 1:29) || identical(k, nf_gage_abbrv()),
      msg = paste0(
        "Cannot extract sites from crssi object.\n",
        "Convert to a crss_nf object first."
      )
    )
  }
  
  if (!missing(l))
    assert_that(
      "intervening" %in% l, 
      msg = "Must keep the intervening flow space when extracting from a crssi object."
    )
  
  if (!missing(m))
    assert_that(
      "monthly" %in% m,
      msg = "Must keep monthly time step when extracting from a crssi object."
    )
  
  
  # save other data
  sac_yt <- x[["sac_year_type"]]
  scen_name <- x[["scen_name"]]
  scen_number <- x[["scen_number"]]
  orig_trace <- x[["n_trace"]]
  
  # extract the flow data using nfd_extract.crss_nf
  x <- nfd_extract.crss_nf(x, i, j, k, l, m)
  
  # then extract sac_yt data ----------------
  # time 
  if (missing(i)) {
    i <- ""
  } else {
    if (is.numeric(i)) {
      stop(
        "Extracting time from crssi objects by row index is ambiguous.\n",
        "Try extracting by date element instead. See help('[.xts', package = 'xts')"
      )
    } else {
      assert_that(is.character(i) && length(i) == 1)
    }
  }
  
  # trace
  if (missing(j)) {
    j <- seq(orig_trace)
  } else {
    assert_that(is.numeric(j) && max(j) <= orig_trace)
  }
  
  sac_yt <- sac_yt[i, j]
  
  # then recreate crssi
  x[["sac_year_type"]] <- sac_yt
  x[["scen_name"]] <- scen_name
  x[["scen_number"]] <- scen_number
  x[["n_trace"]] <- n_trace(x)
  
  class(x) <- c("crssi", class(x))
  
  crssi_validate(x)
}

#' @param x list of trace data. Presumably the list of annual intervening, 
#' annual total, monthly intervening, or monthly total data
#' @param i time
#' @param j trace
#' @param k site
#' @noRd
subset_trace <- function(x, i, j, k, keep_trace) 
{
  if (keep_trace)
    out <- x[[j]][i, k] 
  else 
    out <- NULL
  
  out
}

# checks list, and removes any entries that are NULL
remove_traces <- function(x)
{
  if (!is.null(x)) {
    x <- x[lengths(x) != 0]
  }
  
  x
}

keep_all_traces <- function(j, n_traces)
{
  all(seq(n_traces) %in% j)
}

keep_all_ts <- function(i, n_ts)
{
  i == "" || is.null(i) || all(seq(n_ts) %in% i)
}

keep_all_sites <- function(k, n_sites)
{
  all(seq(n_sites) %in% k)
}