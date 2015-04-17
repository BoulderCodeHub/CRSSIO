#source(paste(Sys.getenv('GEN_CODE'),'/getDataFromRdf.R',sep = ''))
library(plyr)

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
		"Surplus.Flood Control Surplus Flag",
		"Shortage.Step 1 Shortage Flag","Shortage.Step 2 Shortage Flag",
		"Shortage.Step 3 Shortage Flag")
	r <- paste(r, '_EOCY_100',sep = '')
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
		'Mid-Elevation Balancing - annual release = 7.48 maf',
		'Equalization - annual release > 8.23 maf',
		'Lower Elevation Balancing - annual release > 8.23 maf',
		'Lower Elevation Balancing - annual release < 8.23 maf',
		'Lower Elevation Balancing - annual release = 8.23 maf',
		'Surplus - Flood Control','Shortage - 1st Level (Mead <= 1,075 and >= 1,050',
		'Shortage - 2nd Level (Mead < 1,050 and >= 1,025',
		'Shortage - 3rd Level (Mead < 1,025)','Equalization Tier','Upper Elevation Balancing Tier',
		'Mid-Elevation Balancing Tier','Lower Elevation Balancing Tier')
	r
}

vShort <- function()
{
	r <- c('norm','mer823','short','uebLT823','surplus','eq823','uebGT823','ueb823','mer748',
		'eq','lebGT823','lebLT823','leb823','fc','s1','s2','s3')
	r
}

vShortAll <- function()
{
	r <- c('norm','mer823','short','uebLT823','surplus','eq823','uebGT823','ueb823','mer748',
		'eq','lebGT823','lebLT823','leb823','fc','s1','s2','s3','eqAll','uebAll','merAll',
		'lebAll')
	r
}

shortOrderFull <- function()
{
	r <- c('eqAll','eq','eq823','uebAll','uebGT823','ueb823','uebLT823','merAll','mer823','mer748',
		'lebAll','lebGT823','leb823','lebLT823','short','s1','s2','s3','surplus','fc','norm')
	r
}

shortOrderLimit <- function()
{
	r <- c('eqAll','eq','eq823','uebAll','uebGT823','ueb823','uebLT823','merAll','mer823','mer748',
		'lebAll','short','s1','s2','s3','surplus','fc','norm')
	r
}

get5YearTable <- function(scenPath, scen,oPath,nYrs)
{
	srA = list()
	srA[[1]] <- list()
	srA[[1]]$slots <- c("SummaryOutputData.LBNormalCondition",
		"SummaryOutputData.MidElevationReleaseAt823","SummaryOutputData.LBShortageConditions",
		"SummaryOutputData.UpperBalancingBelow823","SummaryOutputData.LBSurplusConditions",
		"SummaryOutputData.EqualizationAt823","SummaryOutputData.UpperBalancingAbove823",
		"SummaryOutputData.UpperBalancingAt823","SummaryOutputData.MidElevationReleaseAt748",
		"SummaryOutputData.EqualizationAbove823","SummaryOutputData.LowerBalancingAbove823",
		"SummaryOutputData.LowerBalancingBelow823","SummaryOutputData.LowerBalancingAt823",
		"Surplus.Flood Control Surplus Flag",
		"Shortage.Step 1 Shortage Flag","Shortage.Step 2 Shortage Flag",
		"Shortage.Step 3 Shortage Flag")
	srA[[1]]$annualize <- matrix(c(rep('EOCY',length(srA[[1]]$slots)),rep('100',
		length(srA[[1]]$slots))),ncol = length(srA[[1]]$slots), byrow = T)
	srA[[1]]$rdf <- c('SystemConditions.rdf')

	getDataForAllScens(scen, scen, srA, scenPath, paste(oPath,'tmpData.txt',sep = ''))
	
	zz <- read.table(paste(oPath,'tmpData.txt',sep = ''),header = T)
	zz2 <- ddply(zz, .(Year,Variable), summarize,mean = mean(Value))
	yr <- min(zz2$Year)
	yr <- yr:(yr+(nYrs-1)) # 5 year window from the first year
	zz2 <- zz2[zz2$Year %in% yr,]
	zz <- dcast(zz2, Year~Variable, value.var = 'mean')
	
	# change names and arange in the correct order
	rr <- names(zz)[2:ncol(zz)]
	rr[match(vNames(),rr)] <- vShort()
	names(zz)[2:ncol(zz)] <- rr
	yrs <- zz$Year
	zz$eqAll <- zz$eq + zz$eq823
	zz$uebAll <- zz$uebGT823 + zz$ueb823 + zz$uebLT823
	zz$merAll <- zz$mer823 + zz$mer748
	zz$lebAll <- zz$lebGT823 + zz$leb823 + zz$lebLT823
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
	colnames(zzLimit) <- yrs
	
	write.csv(zz, paste(oPath,'/5YearTable_Full.csv',sep = ''), row.names = T)
	write.csv(zzLimit, paste(oPath,'/5YearTable_Limited.csv',sep = ''), row.names = T)
}




