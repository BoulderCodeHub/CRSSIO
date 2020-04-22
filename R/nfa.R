# TODO: how to handle creation of flow_space = both; maybe that is a future 
# enhancement
# TODO: should this be an extension of xts? can you have 4-D xts? 
# would it simplify summation to annual?
# TODO: should this actually be a list with one entry for annual and one for monthly?
#' Natural Flow Array (nfa)
#' 
#' `nfa()` creates a natural flow array from the specified `data`. Stores data
#' for multiple months and traces (sequences) for all 29 natural flow sites 
#' ([nf_gage_names()]).
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
#' @param start_year Start year to use for monthly data if `data` is length of 1.
#' 
#' @export
nfa <- function(data = NA, dim = NA, flow_space = c("intervening", "total"),
                 start_year = NA)
{
  flow_space <- match.arg(flow_space, c("intervening", "total"))
  assert_that(
    is.na(start_year) || (is.numeric(start_year) && length(start_year) == 1)
  )
  if (is.na(start_year))
    start_year <- format(Sys.Date(), "%Y")
  
  if (is.na(data) || (length(data) == 1 && is.numeric(data))) {
    # initialize an empty nfa
    if (is.na(dim)) {
      # just one time dimension, and one trace dimension
      x <- array(
        data = data, 
        dim = c(1, 1, 29), 
        dimnames = list(
          paste0(start_year, '-01'), "Trace1", nf_gage_abbrv()
        )
      )
    } else {
      # if dim is not NA, then it should be length 2
      assert_that(is.numeric(dim) && length(dim) == 2)
      assert_that(dim[1] %% 12 == 0)
      
      # TODO: add years to mm
      mm <- sprintf("%02d", rep(1:12, dim[1] / 12))
      tt <- paste0("Trace", 1:dim[2])
      
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
}

#' @export
as_nfa <- function(x, flow_space, ...)
{
  UseMethod("as_nfa")
}

# Maybe don't need this one??
as_nfa.default <- function(x, ...)
{
  
}

as_nfa.matrix <- function(x, ...)
{
  
}

as_nfa.array <- function(x, ...)
{
  
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
