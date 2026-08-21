#' Compute St. Vrain Natural Flow for CRSS
#' 
#' `st_vrain_nf_calc()` estimates annual (calendar year) natural flow in  
#' the St. Vrain river in Lyons, CO as a function of total natural flow 
#' at Glenwood Springs. 
#' 
#' CRSS uses St. Vrain natural flow to estimate the volume of transmountain 
#' diversions (see CRSS object 'TMD East Slope Supply').
#' 
#' St. Vrain natural flow can be generated from the Colorado River natural flow 
#' data since these must always be available for use in CRSS. St. Vrain natural
#' natural flow is estimated using a linear regression created by Reclamation  
#' and total natural flow at Glenwood Springs. See
#' https://github.com/BoulderCodeHub/CRSSIO/tree/master/data-raw/StVrain_NaturalFlow_Documentation 
#' for details on the linear regression.
#' 
#' @param cy_vol xts object that has annual 
#' (calendar year), total natural flow for GlenwoodSprings. Columns are assumed
#' to be traces and rows are years.
#' 
#' @param co_tot_nf [nfd] object that has annual, total data and has the
#' "cy" year type. It also must have at least Glenwood Springs volumes. 
#'  Volumes at other locations are ignored. 
#'  
#' @return xts object containing calendar year total natural flow for the St. 
#' Vrain. Columns are traces and rows are years.
#'  
#' @examples
#' st_vrain_nf_calc(
#'   xts::xts(
#'     matrix(sample(CoRiverNF::cyAnnTot$GlenwoodSprings,15), nrow = 3, ncol = 5), 
#'     order.by = zoo::as.yearmon("Dec 2000") + 0:2
#'   )
#' )
#' 
#' nf <- nfd(CoRiverNF::cyAnnTot, flow_space = "total", 
#'   time_step = "annual",
#'   year = "cy")
#'
#' st_vrain_nf_calc(co_tot_nf = nf)
#' 
#' @export

st_vrain_nf_calc <- function(cy_vol = NULL, co_tot_nf=NULL) {

  # Intercept and slope of linear regression
  int=2.045355e+04
  slope=4.670764e-02 
 
  assert_that(
    xor(missing(cy_vol), missing(co_tot_nf)),
    msg = "Only one of `cy_vol` or `co_tot_nf` should be specified."
  )
  
  if (missing(co_tot_nf)) {
    # use cy_vol. cy_vol should be xts object
    assert_that(
      xts::is.xts(cy_vol) && xts::periodicity(cy_vol)$label == "year",
      msg = "`cy_vol` must be a yearly xts object."
    )
    
    st_vrain = cy_vol*slope+int

    # rename columns
    names(st_vrain) <- paste0('TraceNumber', 1:ncol(st_vrain))
    
  } else {
    assert_that(
      is_nfd(co_tot_nf) && has_total(co_tot_nf) && 
        has_annual(co_tot_nf) && attr(co_tot_nf, "year") == "cy",
      msg = "`co_tot_nf` must be a nfd object with annual (CY) intervening data."
    )
    
    assert_that(
      'GlenwoodSprings' %in% sites(co_tot_nf), 
      msg = "`co_int_nf` must have named sites that match `nf_gage_abbrv()`"
    )
    
    st_vrain=lapply(co_tot_nf$annual$total, function(x){
      
      x$GlenwoodSprings*slope+int
      
    })
    
    st_vrain <- do.call(cbind, st_vrain)
    names(st_vrain) <- paste0('TraceNumber', 1:ncol(st_vrain))
    st_vrain <- xts::xts(st_vrain, zoo::index(co_tot_nf$annual$total[[1]]))
    
  }
  
  # remove attributes. If there were any, they were inherited from the input.
  xtsAttributes(st_vrain) <- NULL
  
  st_vrain
}
