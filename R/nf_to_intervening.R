#' @export
#' @rdname nf_to_total
nf_to_intervening <- function(x, ...)
{
  UseMethod("nf_to_intervening")
}

#' @export
#' @rdname nf_to_total
nf_to_intervening.crss_nf <- function(x, keep_total = TRUE, recompute = FALSE, 
                                ...) {
  assert_that(
    recompute,
    msg = "`recompute` must be TRUE for computing intervening natural flow for `crss_nf` objects."
  )
  
  NextMethod()
}

#' @export
#' @rdname nf_to_total
nf_to_intervening.nfd <- function(x, keep_total = TRUE, recompute = FALSE, 
                                  ...)
{
  assert_that(
    all(sites(x) %in% nf_gage_abbrv()) && all(nf_gage_abbrv() %in% sites(x)),  
    msg = paste(
      "The sites in `x` must exactly match those in `nf_gage_abbrv()`.",
      "The conversion to total intervening flow is only known for those specific sites.",
      sep = "\n"
    )
  )
  
  # check that total exists for annual and/or monthly
  has_ann <- has_total(x, "annual")
  has_mon <- has_total(x, "monthly")
  
  assert_that(
    has_ann || has_mon, 
    msg = "`x` does not have any total flow data."
  )
  
  # check for recompute if x already has total
  if ((has_ann && has_intervening(x, "annual")) || 
      (has_mon && has_intervening(x, "monthly"))) {
    assert_that(
      recompute, 
      msg = paste(
        "`x` already has intervening natural flow.",
        "To recompute/update the intervening natural flow, `recompute` must be `TRUE`.",
        sep = "\n"
      )
    )
  }
  
  if (has_ann) {
    ann_int <- lapply(seq_len(n_trace(x)), function(i) {
      compute_intervening_nf(x$annual$total[[i]])
    })
    
    if (!keep_total) {
      x$annual <- list("intervening" = ann_int, "total" = NULL)
    } else {
      x$annual$intervening <- ann_int 
    }
  }
  
  if (has_mon) {
    mon_int <- lapply(seq_len(n_trace(x)), function(i) {
      compute_intervening_nf(x$monthly$total[[i]])
    })
    
    if (!keep_total) {
      x$monthly <- list("intervening" = mon_int, "total" = NULL)
    } else {
      x$monthly$intervening <- mon_int
    }
  }
  
  x
}

#' @export
#' @rdname nf_to_total
nf_to_intervening.xts <- function(x, ...) {
  assert_that(
    all(colnames(x) %in% nf_gage_abbrv()) && 
      all(nf_gage_abbrv() %in% colnames(x)),  
    msg = paste(
      "The colnames in `x` must exactly match those in `nf_gage_abbrv()`.",
      "The conversion to total natural flow is only known for those specific sites.",
      sep = "\n"
    )
  )
  
  compute_intervening_nf(x)
}

# x is an xts object
compute_intervening_nf <- function(x) {
  # ignore most upstream nodes as intervening and total are the same
  # then start subtracting to other intervening natural flows downstream to 
  # upstream
  
  # Lower Basin
  x$Imperial <- x$Imperial - x$Parker
  x$Parker <- x$Parker - x$Davis - x$Alamo
  x$Davis <- x$Davis - x$Hoover
  x$Hoover <- x$Hoover - x$GrandCanyon - x$Littlefield
  x$GrandCanyon <- x$GrandCanyon - x$Cameron - x$LeesFerryParia - x$LeesFerry
  
  # Upper Basin
  x$LeesFerry <- x$LeesFerry - x$Bluff - x$CiscoColorado - x$GreenRiverUTGreen - 
    x$GreenRiverUTSanRafael
  x$Bluff <- x$Bluff - x$Archuleta
  x$GreenRiverUTGreen <- x$GreenRiverUTGreen - x$Greendale - x$Maybell - 
    x$Lily - x$Watson - x$Randlett
  x$Greendale <- x$Greendale - x$GreenRiverWY
  x$GreenRiverWY <- x$GreenRiverWY - x$Fontenelle
  x$CiscoColorado <- x$CiscoColorado - x$Cameo - x$CiscoDolores - 
    x$GrandJunction
  x$GrandJunction <- x$GrandJunction - x$Crystal
  x$Crystal <- x$Crystal - x$BlueMesa
  x$BlueMesa <- x$BlueMesa - x$TaylorPark
  x$Cameo <- x$Cameo - x$GlenwoodSprings
  
  x
}
