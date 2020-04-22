# TODO: how to handle creation of flow_space = both; maybe that is a future 
# enhancement
# TODO: should this be an extension of xts? can you have 4-D xts? 
# would it simplify summation to annual?
# TODO: should this actually be a list with one entry for annual and one for monthly?
# TODO: should there be an attribute that specifies if the dates mean something,
# ex: dates mean something in historical data or in VIC data, but they are 
# arbitrary in ism data. 

#' Natural Flow Array (nfa)
#' 
#' `nfa()` creates a natural flow array from the specified `data`. Stores data
#' for multiple months and traces (sequences) for all 29 natural flow sites 
#' ([nf_gage_names()]).
#' 
#' *Array:* Arrays should be an m x t x 29 array, where m is the number of
#' months, and t is the number of 
#' traces. If there are rownames, then they must be in "yyyy-mm" format, 
#' otherwise an error will post. Rownames are not required, and if they are not
#' provided will be set starting with the specified `start_yearmon` or assuming
#' a starting date of January of the current year. The colnames are not required
#' but will be used in their provided form if they are specified. If they are 
#' not specified, they will be named Trace1 - TraceN. The names of the 3rd 
#' dimension should match [nf_gage_abbrv()] if provided. If they are not 
#' provided, then assume that the depths match the order in [nf_gage_abbrv()].
#' 
#' @param data If `NA` or of length 1, creates an object with dimensions based
#'   on `dim`. Otherwise, creates an object based on the provided data. Data 
#'   should be a matrix, array, list, data.frame, or `[xts]` object. 
#'   
#' @param dim `NA` or a length 2 numeric vector. If `NA` and `data` is `NA`, 
#'   then creates an object with one month and one trace of data with the values. 
#'   If `data` is only one value, then creates based on the user specified 
#'   number of months and number of traces. `c(months, traces)`. Should be `NA`
#'   if data is not NA or length of 1. 
#'   
#' @param flow_space Data are intervening or total flow.
#' 
#' @param start_yearmon Start year and month of data. If `NA`, then assumes the
#'   data begins in January of the current year. Should be a [zoo::yearmon] 
#'   object or coercable to one. 
#' 
#' @export
nfa <- function(data = NA, dim = NA, flow_space = c("intervening", "total"),
                 start_yearmon = NA)
{
  flow_space <- match.arg(flow_space, c("intervening", "total"))
  assert_that(length(start_yearmon) == 1)

  if (is.na(start_yearmon)) {
    start_yearmon <- default_yearmon()
  } else {
    start_yearmon <- zoo::as.yearmon(start_yearmon)
  }
  
  if (isTRUE(is.na(data)) || (length(data) == 1 && is.numeric(data))) {
    # initialize an empty nfa
    if (isTRUE(is.na(dim))) {
      # just one time dimension, and one trace dimension
      x <- array(
        data = data, 
        dim = c(1, 1, 29), 
        dimnames = list(
          ym_to_yyyymm(start_yearmon), "Trace1", nf_gage_abbrv()
        )
      )
    } else {
      # if dim is not NA, then it should be length 2
      assert_that(is.numeric(dim) && length(dim) == 2)
      
      mm <- ym_to_yyyymm(ym_seq(start_yearmon, dim[1]))
      tt <- trace_names(dim[2])
      
      x <- array(
        data = data, 
        dim = c(dim, 29), 
        dimnames = list(mm, tt, nf_gage_abbrv())
      )
    }
    
    x <- structure(x, "flow_space" = flow_space, class = c("nfa", "array"))
    
  } else {
    # dim should be NA (assumed based on data)
    x <- as_nfa(data, flow_space, start_yearmon = start_yearmon)
  }
  
  x
}

#' @export
as_nfa <- function(x, flow_space = c("intervening", "total"), 
                   start_yearmon = NA, ...)
{
  UseMethod("as_nfa")
}

as_nfa.default <- function(x, ...)
{
  stop("as_nfa() is not implemented for an object of class: ", class(x),
       "\nSee ?nfa for details.")
}

as_nfa.array <- function(x, flow_space = c("intervening", "total"), 
                         start_yearmon = NA, ...)
{
  flow_space <- match.arg(flow_space, c("intervening", "total"))
  
  assert_that(length(dim(x)) == 3)
  assert_that(dim(x)[3] == 29)

  # are there rownames? -----------
  # if so...try to use them
  rr <- rownames(x)
  if (is.null(rr)) {
    if (is.na(start_yearmon)) {
      # use defaults
      rownames(x) <- ym_to_yyyymm(ym_seq(default_yearmon(), dim(x)[1]))
    } else {
      # use sequence starting from start_yearmon
      rownames(x) <- ym_to_yyyymm(ym_seq(start_yearmon, dim(x)[1]))
    }
  } else {
    
    # Be strict - if rownames exist, they have to be yyyy-mm format
    assertthat(
      all(grepl("\\d{4}-\\d{2}", rr)),
      msg = "The provided rownames are not all in yyyy-mm format."
    )
    
    # check if they are increasing; if not, guess we should sort them
    if (!are_increasing(rr)) {
      # sort the array
      rr_sort <- sort.int(rr, index.return = TRUE)$ix
      x <- x[rr_sort, ,]
    }
  }
  
  # are there colnames? ---------
  # if so use them
  # colnames are less strict, use what there is if there is something,
  # otherwise name them Trace1 - TraceN
  if (is.null(colnames(x))) {
    colnames(x) <- trace_names(dim(x)[2])
  }
  
  # are there depth names? ---------------
  # if so try to use them; they have to be nf_gage_names
  # if they aren't assume that the gages are in correct order
  dd <- dimnames(x)[[3]]
  if (is.null(dd)) {
    dimnames(x)[[3]] <- nf_gage_abbrv()
  } else {
    assert_that(all(dd %in% nf_gage_abbrv()) && all(nf_gage_abbrv()) %in% dd)
    
    # check to see if we need to reorder the depth dimension
    nf_names <- match(dd, nf_gage_abbrv())
    if (!all(nf_names == 1:29)) {
      # they need to be sorted 
      nf_i <- sort.int(nf_names, index.return = TRUE)$ix
      x <- x[, , nf_i]
    }
  }
  
  structure(x, "flow_space" = flow_space, class = c("nfa"))
}

as_nfa.matrix <- function(x, flow_space = c("intervening", "total"), 
                          start_yearmon = NA, ...)
{
  # will assume that the matrix only has one trace of data; format it as an 
  # array, and then call as_nfa()
  assert_that(ncol(x) == 29)
  x <- array(
    x, 
    dim = c(nrow(x), 1, 29), 
    dimnames = list(rownames(x), "Trace1", colnames(x))
  )
  
  as_nfa(x, flow_space = flow_space, start_yearmon = start_yearmon, ...)
}

# TODO
as_nfa.data.frame <- function(x, ...)
{ 
  # what are required column names? trace may not be required, can assume 1 
  # trace if not specified. should this work for long and wide format?
  stop("not implemented")
}

# TODO
as_nfa.list <- function(x, ...)
{
  # what shape to work for? each list entry is a site? each list entry is a 
  # trace? require list to be named? maybe that's the best route, then could 
  # work with either shape
  stop("not implemented")
}

# TODO
as_nfa.xts <- function(x, ...)
{
  stop("not implemented")
}

# takes a vector of yearmons and converts them to strings in yyyy-mm format
ym_to_yyyymm <- function(x)
{
  format(x, "%Y-%m")
}

# takes a yearmon `x`, and creates a vector yearmons starting with x and a 
# specified length
ym_seq <- function(x, length)
{
  x + 0:(length - 1)/12
}

default_yearmon <- function()
{
  zoo::as.yearmon(paste0(format(Sys.Date(), "%Y"), "-01"))
}

# x should be coercible to yearmon; then determine if they are increasing
are_increasing <- function(x) 
{
  x <- zoo::as.yearmon(x)
  i <- length(x)
  
  all(x[2:i] >= x[1:(i-1)])
}

# returns vector from Trace1 to Trace[n]
trace_names <- function(n)
{
  paste0("Trace", 1:n)
}

# TODO: add site argument? That then resolve to the correct k dimension?
#' @export
`[.nfa` <- function(x, i, j, ...) {
  attrs <- attributes(x)

  out <- unclass(x)
  out <- out[i, j, ..., drop = FALSE]

  attr(out, "flow_space") <- attrs$flow_space
  class(out) <- "nfa"

  out
}

#' @export
`[<-.nfa` <- function(x, i, j, ...) {
  .Class <- "array"
  NextMethod(.Generic)
}

#' @export
is_nfa <- function(x) inherits(x, "nfa")

ntrace <- function(x)
{
  assert_that(is_nfa(x))
  dim(x)[2]
}

#' @export
print.nfa <- function(x, ...)
{
  cat(
    "CRSSIO::nfa - natural flow array\n",
    " - flow_space:  ", attr(x, "flow_space"), "\n",
    " - # traces:    ", ntrace(x), "\n",
    " - time period: ", rownames(x)[1], " to ", tail(rownames(x), 1), "\n",
    "Trace 1:\n"
  )
  print(head(x, 3))
  cat("\n...\n")
  
  invisible(x)
}

#' @export
head.nfa <- function(x, n = 6L, trace = 1, ...)
{
  assert_that(trace <= ncol(x))
  assert_that(length(n) == 1 && is.numeric(n))
  
  if (n > 0) {
    n <- min(n, nrow(x))
    n <- 1:n
  } else {
    n <- min(nrow(x), abs(n))
    if (n == nrow(x)) {
      n <- 0
    } else {
      n <- 1:(nrow(x) - n)
    }
  }
  
  as.matrix(x[n, trace, ])
}

#' @export
tail.nfa <- function(x, n = 6L, trace = 1, ...)
{
  assert_that(trace <= ncol(x))
  assert_that(length(n) == 1 && is.numeric(n))

  if (n > 0) {
    n <- min(n, nrow(x))
    n <- (nrow(x) - n + 1):nrow(x)
  } else {
    n <- min(nrow(x), abs(n))
    if (n == nrow(x)) {
      n <- 0
    } else {
      n <- (n + 1):nrow(x)
    }
  }
  
  as.matrix(x[n, trace, ])
}

#' @export
as.array.nfa <- function(x, ...)
{
  unclass(x)
}

#' @export
as.matrix.nfa <- function(x, ...)
{
  assert_that(
    ncol(x) == 1,
    msg = "Can only convert nfa to matrix if there is only one trace of data."
  )
  
  unclass(x)[,,, drop = TRUE]
}

# TODO: everything below here ---------
as.data.frame.nfa <- function(x, long = TRUE, ...)
{
  # create column names for trace and yearmon. return in long format which has
  # site column. return in wide format which has one column for each site
  stop("not implemented")
}

rbind.nfa <- function(..., deparse.level = 1)
{
  # complicated - will check that the time ranges are not overlapping or sparse
  stop("not impelmented")
}

cbind.nfa <- function(..., deparse.level = 1)
{
  # will combine by trace, but need to check that time ranges are identical
  stop("not implemented")
}

c.nfa <- function(...)
{
  # combine based on 4th dimension once we are storing intervening and total flow
  stop("not implemented")
}
