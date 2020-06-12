
# variable names after getting data from rdf
slotNames <- function()
{
	r <- c(
	  "SummaryOutputData.LBNormalCondition",
		"SummaryOutputData.MidElevationReleaseAt823",
		"SummaryOutputData.LBShortageConditions",
		"SummaryOutputData.UpperBalancingBelow823",
		"SummaryOutputData.LBSurplusConditions",
		"SummaryOutputData.EqualizationAt823",
		"SummaryOutputData.UpperBalancingAbove823",
		"SummaryOutputData.UpperBalancingAt823",
		"SummaryOutputData.MidElevationReleaseAt748",
		"SummaryOutputData.EqualizationAbove823",
		"SummaryOutputData.LowerBalancingAbove823",
		"SummaryOutputData.LowerBalancingBelow823",
		"SummaryOutputData.LowerBalancingAt823",
		"SummaryOutputData.LBFloodControlSurplus",
		"SummaryOutputData.LBShortageStep1",
		"SummaryOutputData.LBShortageStep2",
		"SummaryOutputData.LBShortageStep3"
	)
	
	r
}

# description want to use in the output file, ordered in the same name as vNames
vDescAll <- function()
{
	r <- c(
	  'Normal Year or ICS Surplus Condition (Mead < 1,145 and > 1,075 ft)', 
	  'Mid-Elevation Release - annual release = 8.23 maf',
		'Shortage Condition - any amount (Mead <= 1,075 ft)',
		'Upper Elevation Balancing - annual release < 8.23 maf',
		'Surplus Condition - any amount (Mead >= 1,145 ft)',
		'Equalization - annual release = 8.23 maf',
		'Upper Elevation Balancing - annual release > 8.23 maf',
		'Upper Elevation Balancing - annual release = 8.23 maf',
		'Mid-Elevation Release - annual release = 7.48 maf',
		'Equalization - annual release > 8.23 maf',
		'Lower Elevation Balancing - annual release > 8.23 maf',
		'Lower Elevation Balancing - annual release < 8.23 maf',
		'Lower Elevation Balancing - annual release = 8.23 maf',
		'Surplus - Flood Control',
		'Shortage - 1st Level (Mead <= 1,075 and >= 1,050 ft)',
		'Shortage - 2nd Level (Mead < 1,050 and >= 1,025 ft)',
		'Shortage - 3rd Level (Mead < 1,025 ft)',
		'Equalization Tier (Powell >= Equalization [EQ] Elevation)',
		'Upper Elevation Balancing Tier (Powell < EQ Elevation and >= 3,575 ft)',
		'Mid-Elevation Release Tier (Powell < 3,575 and >= 3,525 ft)',
		'Lower Elevation Balancing Tier (Powell < 3,525 ft)'
	)
	r
}

vShort <- function()
{
	r <- c('lbNormal','mer823','lbShortage','uebLt823','lbSurplus','eq823',
	       'uebGt823','ueb823','mer748','eq','lebGt823','lebLt823','leb823',
	       'lbFcSurplus','lbShortageStep1','lbShortageStep2','lbShortageStep3')
	r
}

vShortAll <- function()
{
	r <- c('lbNormal','mer823','lbShortage','uebLt823','lbSurplus','eq823',
	       'uebGt823','ueb823', 'mer748','eq','lebGt823','lebLt823','leb823',
	       'lbFcSurplus','lbShortageStep1', 'lbShortageStep2','lbShortageStep3',
	       'eqAll','uebAll','merAll','lebAll')
	r
}

shortOrderFull <- function()
{
	r <- c(
	  'eqAll','eq','eq823','uebAll','uebGt823','ueb823','uebLt823','merAll',
	  'mer823','mer748', 'lebAll','lebGt823','leb823','lebLt823',
	  'lbSurplus','lbFcSurplus', 'lbNormal', 'lbShortage', 'lbShortageStep1',
	  'lbShortageStep2', 'lbShortageStep3'
	)
	r
}

shortOrderLimit <- function()
{
	r <- c(
	  'eqAll','eq','eq823','uebAll','uebGt823','ueb823','uebLt823','merAll',
	  'mer823','mer748', 'lebAll','lbSurplus','lbFcSurplus', 'lbNormal',
	  'lbShortage','lbShortageStep1', 'lbShortageStep2','lbShortageStep3'
	)
	r
}

#' Create standard CRSS system conditions table
#' 
#' Create the standard system conditions table (`crsso_get_sys_cond_table()`) 
#' using the prespecified set of CRSS slots (`sys_cond_rwa()`)
#' 
#' @details
#' `crsso_get_sys_cond_table()` creates the standard system conditions table 
#' that is commonly created from CRSS results, e.g., slide 6 at
#' \url{https://www.usbr.gov/lc/region/g4000/crss-5year.pdf}. The table reports 
#' the percent of traces that simulate various system conditions, e.g., Lake 
#' Powell operating tiers, through time.
#' 
#' @param zz Full data for all years/traces necessary for creating System 
#'   Conditions table. `zz` should be a data frame returned from 
#'   [RWDataPlyr::getDataForAllScens()] that contains all
#'   of the 17 variables necessary to create the system conditions table.
#' @param yrs Vector of years to provide the system conditions for. 
#'   Ex: `2017:2020`
#' 
#' @return `crsso_get_sys_cond_table()` returns a named list with two matrices, 
#'   i.e., system condition tables. The first matrix (`fullTable`) 
#'   includes the system conditions for the specified years including the 
#'   breakout of Lower Elevation Balancing releases.  The second matrix 
#'   (`limitedTable`) includes the system conditions without the Lower 
#'   Elevation Balancing breakout.
#' 
#' @examples
#' # use RWDataPlyr package to get the data for the system conditions table
#' rwa <- sys_cond_rwa()
#' scenFolder <- "ISM1988_2014,2007Dems,IG,Most"
#' scenName <- "scenA"
#' scenPath <- system.file('extdata','Scenario/',package = 'RWDataPlyr')
#' sysData <- RWDataPlyr::rdf_aggregate(
#'   rwa,
#'   rdf_dir = file.path(scenPath, scenFolder),
#'   scenario = scenName
#' )
#'
#' sysCondTable <- crsso_get_sys_cond_table(sysData, 2018:2022)
#' 
#' # print out the limited table
#' sysCondTable$limitedTable
#' 
#' @export
#' @rdname sys_cond_table
crsso_get_sys_cond_table <- function(zz, yrs)
{
  # if there there is a "Scenario" dimension and there are more than 1 
  # scenarios, then post a warning message that the scenarios will be averaged 
  # together for creating the table
  if(!is.null(zz$Scenario) & length(levels(as.factor(zz$Scenario))) > 1){
    warning(paste(
      'There are', 
      length(levels(as.factor(zz$Scenario))),
      "Scenarios in the data.\n",
      "Please note, these scenarios will be averaged together when creating the system conditions table."
    ))
  }
  
  # check that all of the necesary variables are present
  if(!all(vShort() %in% as.character(levels(as.factor(zz$Variable))))) {
    tmp <- vShort()[!(vShort() %in% as.character(levels(as.factor(zz$Variable))))]
    stop(
      "The following variables are not found in the data frame passed to crsso_get_sys_cond_table():\n",
      paste(tmp, collapse = ", ")
    )
  }
  
  if(!all(yrs %in% zz$Year)){
    yrs <- yrs[yrs %in% zz$Year]
    # if none of the years exist then throw an error, otherwise warn the user 
    # and will use a subset
    if(length(yrs) == 0)
      stop("None of the yrs exist in the data")
    
    warning(
      "All years (yrs) are not in the data frame passed to crsso_get_sys_cond_table()\n",
      "Will only evaluate for the years that are in the data frame"
    )
   
  }
  
  zz <- zz %>% 
    dplyr::filter(Year %in% yrs) %>%
    # multiply mean by 100 to create % of traces.
    dplyr::group_by(Year, Variable) %>%
    dplyr::summarise(Value = mean(Value)*100) %>%
    tidyr::spread(Variable, Value) %>%
    # change names and arange in the correct order
    dplyr::mutate(
      eqAll = .data$eq + .data$eq823,
      uebAll = .data$uebGt823 + .data$ueb823 + .data$uebLt823,
      merAll = .data$mer823 + .data$mer748,
      lebAll = .data$lebGt823 + .data$leb823 + .data$lebLt823
    ) %>%
    dplyr::arrange(Year)

  yrsLab <- zz$Year
  zz <- subset(zz,select = shortOrderFull())
  zzLimit <- subset(zz, select = shortOrderLimit())
  
  # change to full descriptions and transpose the matrix
  rr <- names(zz)
  ii <- match(rr, vShortAll())
  rr <- vDescAll()[ii]
  names(zz) <- rr
  zz <- t(zz)
  colnames(zz) <- yrs
  
  rr <- names(zzLimit)
  ii <- match(rr, vShortAll())
  rr <- vDescAll()[ii]
  names(zzLimit) <- rr
  zzLimit <- t(zzLimit)
  colnames(zzLimit) <- yrsLab
  
  rr <- list('fullTable' = zz, 'limitedTable' = zzLimit)
  
  rr
}

#' @details
#' `sys_cond_matrix()` is included for use with RWDataPlyr <= v0.5.0. It
#' is a convenience function to save the user from having to 
#' routinely recreate the information to pass to 
#' [RWDataPlyr::createSlotAggList()] when creating the system conditions table.
#' The matrix returned by `sys_cond_matrix()` contains all of the slots and
#' their corresponding variable names that are expected in 
#' `crsso_get_sys_cond_table()`. This matrix should be passed to
#' [RWDataPlyr::createSlotAggList()] to create the necessary
#' slot aggregation list that [RWDataPlyr::getDataForAllScens()]
#' uses. Since `crsso_get_sys_cond_table()` expects a specific set of variable 
#' names, this function ensures the slots from CRSS are correctly mapped to 
#' those expected variables. 
#' 
#' @return `sys_cond_matrix()` returns a 17x5 character matrix.
#' 
#' @export
#' @rdname sys_cond_table

sys_cond_matrix <- function()
{
  n <- length(slotNames())
  r <- cbind(rep('SystemConditions.rdf',n), slotNames(), rep('AnnualRaw',n), 
             rep(NA, n), vShort())
  r
}

#' @details
#' `sys_cond_rwa()` is a convenience function to save the user from having to 
#' routinely recreate the [RWDataPlyr::rwd_agg] object that is necessary to 
#' create the standard CRSS system conditions table. 
#' The object returned by `sys_cond_rwa()` contains all of the slots and
#' their corresponding variable names that are expected in 
#' `crsso_get_sys_cond_table()`. This [RWDataPlyr::rwd_agg] object should be
#' passed to [RWDataPlyr::rdf_aggregate()] to aggregate the necessary data for
#' `crsso_get_sys_cond_table()`, which expects a specific set of variable 
#' names. This function ensures the slots from CRSS are correctly mapped to 
#' those expected variables. 
#' 
#' @return `sys_cond_rwa()` returns a [RWDataPlyr::rwd_agg] object.
#' 
#' @export
#' @rdname sys_cond_table
sys_cond_rwa <- function()
{
  n <- length(slotNames())
  
  RWDataPlyr::rwd_agg(data.frame(
    file = rep("SystemConditions.rdf", n),
    slot = slotNames(),
    period = rep("eocy", n),
    summary = rep(NA, n),
    eval = rep(NA, n),
    t_s = rep(NA, n),
    variable = vShort(),
    stringsAsFactors = FALSE
  ))
}
