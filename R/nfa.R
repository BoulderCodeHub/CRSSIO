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
#' *Array:* Arrays should be an m x t x 29 array, where m is a number of months
#' for a regular number of years (divisible by 12), and t is the number of 
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
    start_yearmon <- default_yearmon
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
      assert_that(dim[1] %% 12 == 0)
      
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
    x <- as_nfa(data, flow_space)
  }
  
  x
}

#' @export
as_nfa <- function(x, flow_space, ...)
{
  UseMethod("as_nfa")
}

# Maybe don't need this one??
as_nfa.default <- function(x, ...)
{
  stop("as_nfa() is not implemented for object of class: ", class(x),
       "\nSee ?nfa for details.")
}

as_nfa.matrix <- function(x, ...)
{
  
}

as_nfa.array <- function(x, flow_space, start_yearmon = NA, ...)
{
  flow_space <- match.arg(flow_space, c("intervening", "total"))
  
  assert_that(length(dim(x)) == 3)
  assert_that(dim(x)[3] == 29)
  assert_that(dim(x)[1] %% 12 == 0)
  
  # are there rownames? -----------
  # if so...try to use them
  rr <- rownames(x)
  if (is.null(rr)) {
    if (is.na(start_yearmon)) {
      # use defaults
      rownames(x) <- ym_to_yyyymm(ym_seq(default_yearmon, dim(x)[1]))
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
  
  structure(x, "flow_space" = flow_space, class = c("nfa", "array"))
}

as_nfa.data.frame <- function(x, ...)
{
  
}

as_nfa.list <- function(x, ...)
{
  
}

as_nfa.xts <- function(x, ...)
{
  
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

default_yearmon <- zoo::as.yearmon(paste0(format(Sys.Date(), "%Y"), "-01"))

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
