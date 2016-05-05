#source(paste(Sys.getenv('GEN_CODE'),'/getDataFromRdf.R',sep = ''))


# variable names after getting data from rdf
vNames <- function()
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
	r <- paste(r, '_AnnualRaw_100',sep = '')
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

get5YearTable <- function(scenPath, scen,oPath,nYrs)
{
	warning('get5YearTable will be depreciated in a future release.\nPlease use RWDataPlot::getDataForAllScens and CRSSIO::createSysCondTable instead.')
  
  srA = list()
	srA[[1]] <- list()
	srA[[1]]$slots <- c("SummaryOutputData.LBNormalCondition",
		"SummaryOutputData.MidElevationReleaseAt823","SummaryOutputData.LBShortageConditions",
		"SummaryOutputData.UpperBalancingBelow823","SummaryOutputData.LBSurplusConditions",
		"SummaryOutputData.EqualizationAt823","SummaryOutputData.UpperBalancingAbove823",
		"SummaryOutputData.UpperBalancingAt823","SummaryOutputData.MidElevationReleaseAt748",
		"SummaryOutputData.EqualizationAbove823","SummaryOutputData.LowerBalancingAbove823",
		"SummaryOutputData.LowerBalancingBelow823","SummaryOutputData.LowerBalancingAt823",
		"SummaryOutputData.LBFloodControlSurplus",
		"SummaryOutputData.LBShortageStep1","SummaryOutputData.LBShortageStep2",
		"SummaryOutputData.LBShortageStep3")
## TO DO
## change to use createSlotAggList
## and to use the abbreviations or the createSysCondTable code will not work.
## Do not multiply by 100. That's taken care of in the createSysCondTable code.
  srA[[1]]$annualize <- matrix(c(rep('AnnualRaw',length(srA[[1]]$slots)),rep('100',
		length(srA[[1]]$slots))),ncol = length(srA[[1]]$slots), byrow = T)
	srA[[1]]$rdf <- c('SystemConditions.rdf')

	getDataForAllScens(scen, scen, srA, scenPath, paste(oPath,'tmpData.txt',sep = ''))
	
	zz <- read.table(paste(oPath,'tmpData.txt',sep = ''),header = T)
	
	yr <- min(zz$Year)
	yr <- yr:(yr+(nYrs-1)) # N year window from the first year
  
  zz <- createSysCondTable(zz, yr)
  write.csv(zz[['fullTable']], paste(oPath,'/5YearTable_Full.csv',sep = ''), row.names = T)
	write.csv(zz[['limitedTable']], paste(oPath,'/5YearTable_Limited.csv',sep = ''), row.names = T)
}

#' \code{createSysCondTable}
#' 
#' \code{createSysCondTable} creates the standard System Conditions table that is commonly created
#' from CRSS results. The table reports the percent of traces that that simulate various
#' system conditions, e.g., Lake Powell operating tiers, through time.
#' 
#' @param zz Full data for all years/traces necessary for creating System Conditions table
#' @param yrs Vector of years to process all of the System Conditions 
#' @return List with two Data frames: one with the System Conditions for the specified 
#' years including the breakout of Lower Elevation Balancing releases and the other without
#' the Lower Elevation Balancing breakout
createSysCondTable <- function(zz, yrs)
{
  # if there there is a "Scenario" dimension and there are more than 1 scenarios, then 
  # post a warning message that the scenarios will be averaged together for crating the table
  if(!is.null(zz$Scenario) & length(levels(as.factor(zz$Scenario))) > 1){
    warning(paste('There are',length(levels(as.factor(zz$Scenario))),
                  'Scenarios in the data. Please note, these scenarios will be averaged together when creating the system conditions table.'))
  }
  
  zz2 <- dplyr::filter(zz, Year %in% yrs)
  
  ## for removal
  ## zz2 <- ddply(zz, .(Year,Variable), summarize,mean = mean(Value))
  
  # multiply mean by 100 to create % of traces.
  zz2 <- zz2 %>% 
    dplyr::group_by(Year, Variable) %>%
    dplyr::summarise(mean = mean(Value)*100)
  
  zz <- reshape2::dcast(zz2, Year~Variable, value.var = 'mean')

  # change names and arange in the correct order
  ## TO DO
  ## Remove the following after using a new create slot agg list function in above code
  # rr <- names(zz)[2:ncol(zz)]
  # rr[match(vNames(),rr)] <- vShort()
  # names(zz)[2:ncol(zz)] <- rr
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


