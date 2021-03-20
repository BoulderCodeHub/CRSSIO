#' Convert between intervening and total flow
#' 
#' `nf_to_total()` converts intervening flow to total flow for the Colorado 
#' River natural flow network, while `nf_to_intervening()` converts total flow
#' to intervening flow. The functions only work if the objects have the
#' 29 Colorado River natural flow sites, and that dimension's names match
#' [nf_gage_abbrv()]. `xts`, `nfd`, `crss_nf`, and `crssi` objects
#' can all be converted between total and intervening flow. For 
#' `xts` objects, the flow is converted without any additional checks, i.e.,
#' it is assumed the user passed in intervening flow if `nf_to_total()` is 
#' called. For the other three types
#' of objects, they must have the required existing data already, i.e., 
#' intervening data must exist when calling `nf_to_total()`, and total data 
#' must exist when calling `nf_to_intervening()`. If the objects have
#' both annual and monthly data, both will be converted. 
#' 
#' @param x An object inheriting from [xts::xts], [nfd], [crss_nf], or [crssi].
#' 
#' @param ... Other parameters passed to subsequent methods.
#' 
#' @return An object of the same class as `x`, except when `x` is either a 
#'   `crss_nf` obect or `crssi` object and `keep_intervening` is `FALSE`. See 
#'   **Details**.
#'   
#' @examples 
#' # start with an nfd object with intervening annual data
#' nf_int <- nfd(
#'   CoRiverNF::cyAnnInt, 
#'   n_sites = 29, 
#'   flow_space = "intervening", 
#'   time_step = "annual"
#' )
#' 
#' # convert to total
#' nf <- nf_to_total(nf_int, keep_intervening = FALSE)
#' 
#' # It matches the total natural flow that exists in the CoRiverNF package 
#' # (except it doesn't have the sheet name attribute)
#' all.equal(
#'   zoo::coredata(CoRiverNF::cyAnnTot),
#'   zoo::coredata(
#'     nfd_get_trace(nf, 1, flow_space = "total", time_step = "annual")
#'   )
#' )
#' 
#' # converting back will result in original data
#' nf2 <- nf_to_intervening(nf, keep_total = FALSE)
#' all.equal(nf2, nf_int)
#' 
#' @export
nf_to_total <- function(x, ...)
{
  UseMethod("nf_to_total")
}

#' @details 
#' When `keep_intervening` is `FALSE` and `x` is either a `crss_nf` object or
#' a `crssi` object, then the returned object will be an `nfd` object, and a 
#' warning will post. This is because `crss_nf` and `crssi` objects must have 
#' monthly intervening natural flow stored in them.
#' 
#' @param keep_intervening When `FALSE`, the intervening flow are removed from
#'   the object. Otherwise those data remain in the returned object.
#'   
#' @param recompute If `x` already has total flow, the function will fail with
#'   an error when `FALSE`. If `TRUE`, then the total flow will be recomputed
#'   and the existing data will be overwritten.
#' @export 
#' @rdname nf_to_total
nf_to_total.crss_nf <- function(x, keep_intervening = TRUE, recompute = FALSE, 
                                ...) {
  if (!keep_intervening) {
    warning(paste(
      "Returned object will be of class `nfd`.", 
      "`crss_nf` objects must have intervening flow, but `keep_intervening` was set to FALSE",
      sep = "\n"
    ))
    
    x <- as_nfd(x)
  }
  
  nf_to_total.nfd(x, keep_intervening, recompute, ...)
}
  
#' @export
#' @rdname nf_to_total
nf_to_total.nfd <- function(x, keep_intervening = TRUE, recompute = FALSE, ...)
{
  assert_that(
    all(sites(x) %in% nf_gage_abbrv()) && all(nf_gage_abbrv() %in% sites(x)),  
    msg = paste(
      "The sites in `x` must exactly match those in `nf_gage_abbrv()`.",
      "The conversion to total natural flow is only known for those specific sites.",
      sep = "\n"
    )
  )
  
  # check that intervening exists for annual and/or monthly
  has_ann <- has_intervening(x, "annual")
  has_mon <- has_intervening(x, "monthly")
  
  assert_that(
    has_ann || has_mon, 
    msg = "`x` does not have any intervening flow data."
  )
  
  # check for recompute if x already has total
  if ((has_ann && has_total(x, "annual")) || 
      (has_mon && has_total(x, "monthly"))) {
    assert_that(
      recompute, 
      msg = paste(
        "`x` already has total natural flow.",
        "To recompute/update the total natural flow, `recompute` must be `TRUE`.",
        sep = "\n"
      )
    )
  }
  
  if (has_ann) {
    ann_tot <- lapply(seq_len(n_trace(x)), function(i) {
      compute_total_nf(x$annual$intervening[[i]])
    })
    
    x$annual$total <- ann_tot
    
    if (!keep_intervening) {
      x$annual <- list("intervening" = NULL, "total" = ann_tot)
    }
  }
  
  if (has_mon) {
    mon_tot <- lapply(seq_len(n_trace(x)), function(i) {
      compute_total_nf(x$monthly$intervening[[i]])
    })
    
    x$monthly$total <- mon_tot
    
    if (!keep_intervening) {
      x$monthly <- list("intervening" = NULL, "total" = mon_tot)
    }
  }
  
  x
}

#' @export
#' @rdname nf_to_total
nf_to_total.xts <- function(x, ...) {
  assert_that(
    all(colnames(x) %in% nf_gage_abbrv()) && 
      all(nf_gage_abbrv() %in% colnames(x)),  
    msg = paste(
      "The colnames in `x` must exactly match those in `nf_gage_abbrv()`.",
      "The conversion to total natural flow is only known for those specific sites.",
      sep = "\n"
    )
  )
  
  compute_total_nf(x)
}

# x is an xts object
compute_total_nf <- function(x) {
  # ignore most upstream nodes as intervening and total are the same
  # then start adding to other total natural flows upstream to downstream
  
  # Upper Basin
  x$Cameo <- x$GlenwoodSprings + x$Cameo
  x$BlueMesa <- x$TaylorPark + x$BlueMesa
  x$Crystal <- x$Crystal + x$BlueMesa
  x$GrandJunction <- x$GrandJunction + x$Crystal
  x$CiscoColorado <- x$CiscoColorado + x$Cameo + x$CiscoDolores + 
    x$GrandJunction
  x$GreenRiverWY <- x$GreenRiverWY + x$Fontenelle
  x$Greendale <- x$Greendale + x$GreenRiverWY
  x$GreenRiverUTGreen <- x$GreenRiverUTGreen + x$Greendale + x$Maybell + 
    x$Lily + x$Watson + x$Randlett
  x$Bluff <- x$Bluff + x$Archuleta
  x$LeesFerry <- x$LeesFerry + x$Bluff + x$CiscoColorado + x$GreenRiverUTGreen + 
    x$GreenRiverUTSanRafael
  
  # Lower Basin
  x$GrandCanyon <- x$GrandCanyon + x$Cameron + x$LeesFerryParia + x$LeesFerry
  x$Hoover <- x$Hoover + x$Littlefield + x$GrandCanyon
  x$Davis <- x$Davis + x$Hoover
  x$Parker <- x$Parker + x$Davis + x$Alamo
  x$Imperial <- x$Imperial + x$Parker
  
  x
}
