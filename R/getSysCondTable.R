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

# ********
# start here by documenting this function and then adding to the examples of 
# createSysCondTable
# *********
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
#' @return Named list with two data frames. The first data frame (\code{'fullTable'}) 
#' includes the system conditions for the specified years including the breakout 
#' of Lower Elevation Balancing releases.  The second data frame (\code{'limitedTable'}) 
#' includes the system conditions without the Lower Elevation Balancing breakout.
#' 
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


