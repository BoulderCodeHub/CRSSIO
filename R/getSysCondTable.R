#source(paste(Sys.getenv('GEN_CODE'),'/getDataFromRdf.R',sep = ''))


# variable names after getting data from rdf
slotNames <- function()
{
	r <- c("SummaryOutputData.LBNormalCondition",
		"SummaryOutputData.MidElevationReleaseAt823","SummaryOutputData.LBShortageConditions",
		"SummaryOutputData.UpperBalancingBelow823","SummaryOutputData.LBSurplusConditions",
		"SummaryOutputData.EqualizationAt823","SummaryOutputData.UpperBalancingAbove823",
		"SummaryOutputData.UpperBalancingAt823","SummaryOutputData.MidElevationReleaseAt748",
		"SummaryOutputData.EqualizationAbove823","SummaryOutputData.LowerBalancingAbove823",
		"SummaryOutputData.LowerBalancingBelow823","SummaryOutputData.LowerBalancingAt823",
		"SummaryOutputData.LBFloodControlSurplus",
		"SummaryOutputData.LBShortageStep1","SummaryOutputData.LBShortageStep2",
		"SummaryOutputData.LBShortageStep3")
	
	r
}

# description want to use in the output file, ordered in the same name as vNames
vDescAll <- function()
{
	r <- c('Normal Year or ICS Surplus Condition', 'Mid-Elevation Balancing - annual release = 8.23 maf',
		'Shortage Condition - any amount (Mead <= 1,075 ft)',
		'Upper Elevation Balancing - annual release < 8.23 maf',
		'Surplus Condition - any amount (Mead>= 1,145 ft)',
		'Equalization - annual release = 8.23 maf',
		'Upper Elevation Balancing - annual release > 8.23 maf',
		'Upper Elevation Balancing - annual release = 8.23 maf',
		'Mid-Elevation Release Tier - annual release = 7.48 maf',
		'Equalization - annual release > 8.23 maf',
		'Lower Elevation Balancing - annual release > 8.23 maf',
		'Lower Elevation Balancing - annual release < 8.23 maf',
		'Lower Elevation Balancing - annual release = 8.23 maf',
		'Surplus - Flood Control','Shortage - 1st Level (Mead <= 1,075 and >= 1,050',
		'Shortage - 2nd Level (Mead < 1,050 and >= 1,025',
		'Shortage - 3rd Level (Mead < 1,025)','Equalization Tier','Upper Elevation Balancing Tier',
		'Mid-Elevation Release Tier','Lower Elevation Balancing Tier')
	r
}

vShort <- function()
{
	r <- c('lbNormal','mer823','lbShortage','uebLt823','lbSurplus','eq823','uebGt823','ueb823',
         'mer748','eq','lebGt823','lebLt823','leb823','lbFcSurplus','lbShortageStep1',
         'lbShortageStep2','lbShortageStep3')
	r
}

vShortAll <- function()
{
	r <- c('lbNormal','mer823','lbShortage','uebLt823','lbSurplus','eq823','uebGt823','ueb823',
         'mer748','eq','lebGt823','lebLt823','leb823','lbFcSurplus','lbShortageStep1',
         'lbShortageStep2','lbShortageStep3','eqAll','uebAll','merAll','lebAll')
	r
}

shortOrderFull <- function()
{
	r <- c('eqAll','eq','eq823','uebAll','uebGt823','ueb823','uebLt823','merAll','mer823','mer748',
		'lebAll','lebGt823','leb823','lebLt823','lbShortage','lbShortageStep1','lbShortageStep2',
    'lbShortageStep3','lbSurplus','lbFcSurplus','lbNormal')
	r
}

shortOrderLimit <- function()
{
	r <- c('eqAll','eq','eq823','uebAll','uebGt823','ueb823','uebLt823','merAll','mer823','mer748',
		'lebAll','lbShortage','lbShortageStep1','lbShortageStep2','lbShortageStep3','lbSurplus',
    'lbFcSurplus','lbNormal')
	r
}

#' Matrix for slot aggregation list for system conditions
#' 
#' \code{sysCondSALMatrix} returns a matrix for use in creating the slot aggregation
#' list to get the variables necessary to create the system conditions table.
#' 
#' The matrix returned by \code{sysCondSALMatrix} contains all of the slots and
#' their corresponding variable names that are expected in 
#' \code{\link{createSysCondTable}}. This matrix should be passed to
#' \code{RWDataPlyr::\link[RWDataPlyr]{createSlotAggList}} to create the necessary
#' slot aggregation list that \code{RWDataPlyr::\link[RWDataPlyr]{getDataForAllScens}}
#' uses. See the example in \code{\link{createSysCondTable}} for an example of
#' using all of these functions together. 
#' 
#' This is a convenience function to save the user from having to routinely 
#' recreate the information to pass to \code{RWDataPlyr::createSlotAggList} for
#' the system conditions table. Additionally, since \code{createSysCondTable}
#' expects a specific set of variable names, this ensures the slots from CRSS
#' are correctly mapped to these variables. 
#' 
#' @return 17x5 character matrix
#' @seealso \code{\link{createSysCondTable}}
#' @export

sysCondSALMatrix <- function()
{
  n <- length(slotNames())
  r <- cbind(rep('SystemConditions.rdf',n), slotNames(), rep('AnnualRaw',n), 
             rep(NA, n), vShort())
  r
}

#' Create standard CRSS system conditions table
#' 
#' \code{createSysCondTable} creates the standard system conditions table that 
#' is commonly created from CRSS results, e.g., slide 6 at
#' \url{https://www.usbr.gov/lc/region/g4000/crss-5year.pdf}. The table reports 
#' the percent of traces that simulate various system conditions, e.g., Lake 
#' Powell operating tiers, through time.
#' 
#' @param zz Full data for all years/traces necessary for creating System Conditions 
#' table. \code{zz} should be a data frame returned from 
#' \code{RWDataPlot::\link[RWDataPlyr]{getDataForAllScens}} that contains all of 
#' the 17 variables necessary to create the system conditions table.
#' @param yrs Vector of years to provide the system conditions for. Ex: \code{2017:2020}
#' @return Named list with two matrices. The first matrix (\code{'fullTable'}) 
#' includes the system conditions for the specified years including the breakout 
#' of Lower Elevation Balancing releases.  The second matrix (\code{'limitedTable'}) 
#' includes the system conditions without the Lower Elevation Balancing breakout.
#' 
#' @examples
#' # use RWDataPlyr package to get the data to create the system conditions table
#' require(RWDataPlyr)
#' slotAggList <- RWDataPlyr::createSlotAggList(CRSSIO::sysCondSALMatrix())
#' scenFolder <- 'ISM1988_2014,2007Dems,IG,Most'
#' scenName <- 'DNF Hydrology'
#' scenPath <- system.file('extdata','Scenario/',package = 'RWDataPlyr')
#' sysData <- RWDataPlyr::getDataForAllScens(scenFolder, scenName, slotAggList,
#'                                           scenPath, 'tmp.feather', TRUE)
#' sysCondTable <- createSysCondTable(sysData, 2018:2022)
#' sysCondTable[['limitedTable']]
#' 
#' @seealso \code{\link{sysCondSALMatrix}}
#' @importFrom magrittr "%>%"
#' @export
createSysCondTable <- function(zz, yrs)
{
  # if there there is a "Scenario" dimension and there are more than 1 scenarios, 
  # then post a warning message that the scenarios will be averaged together 
  # for creating the table
  if(!is.null(zz$Scenario) & length(levels(as.factor(zz$Scenario))) > 1){
    warning(paste('There are',length(levels(as.factor(zz$Scenario))),
                  'Scenarios in the data. Please note, these scenarios will be averaged together when creating the system conditions table.'))
  }
  
  zz2 <- dplyr::filter(zz, Year %in% yrs)

  # multiply mean by 100 to create % of traces.
  zz2 <- zz2 %>% 
    dplyr::group_by(Year, Variable) %>%
    dplyr::summarise(mean = mean(Value)*100)

  zz <- reshape2::dcast(zz2, Year~Variable, value.var = 'mean')

  # change names and arange in the correct order
  yrsLab <- zz$Year
  zz$eqAll <- zz$eq + zz$eq823
  zz$uebAll <- zz$uebGt823 + zz$ueb823 + zz$uebLt823
  zz$merAll <- zz$mer823 + zz$mer748
  zz$lebAll <- zz$lebGt823 + zz$leb823 + zz$lebLt823
  zz <- subset(zz,select = shortOrderFull())
  zzLimit <- subset(zz, select = shortOrderLimit())
  
  # change to full descriptions and transpose the matrix
  rr <- names(zz)
  ii <- match(rr,vShortAll())
  rr <- vDescAll()[ii]
  names(zz) <- rr
  zz <- t(zz)
  colnames(zz) <- yrs
  
  rr <- names(zzLimit)
  ii <- match(rr,vShortAll())
  rr <- vDescAll()[ii]
  names(zzLimit) <- rr
  zzLimit <- t(zzLimit)
  colnames(zzLimit) <- yrsLab
  
  rr <- list('fullTable' = zz, 'limitedTable' = zzLimit)
  
  rr
}


