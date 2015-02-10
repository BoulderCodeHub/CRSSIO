# *********
# Added this file to CRSSIO package
# *********


# creates the CRSS flow input files from the standard excel workbook that is used to publish the
# natural flow data
# iFile is the relative or absolute path to the excel workbook
# oFolder is the location to write out all trace files
# startDate is the start date to be listed in each trace file; should be in 2014-1-31 format
# simYrs is the number of years data each file should contain.  Could always include the same
# number of years as there are historical years, but might as well trim it to save file size

library('xlsx')
source('C:/alan/NaturalFlow/code/NFInputNames.R')

createCRSSDNFInputFiles <- function(iFile, oFolder, startDate, simYrs, oFiles = CRSSOldNFInput())
{
	nf = read.xlsx(iFile, sheetName = 'Intervening Natural Flow')
	# going to take a lot of trimming, etc. to get rid of all the labels we don't need for
	# the flow matrix
	
	# trim off extraneous data
	nf = as.matrix(nf[8:(nrow(nf)-1),2:31])
	# remove gap column
	nf = matrix(as.numeric(nf[,c(1:20,22:30)]),ncol = 29, byrow = F)
	
	nT = nrow(nf)/12
	
	# repeat all data, so can sample the traces that wrap around time
	nf = rbind(nf, nf)
	
	# number of months in each trace
	nM = simYrs * 12
	headerInfo = paste('start_date: ',startDate, ' 24:00\nunits: acre-ft/month',sep = '')
	
	# loop over each trace and file
	for(i in 1:nT){
		print(paste('Starting trace',i))
		flush.console()
		fold = paste(oFolder,'/trace',i,sep = '')
		dir.create(fold)
		
		for(j in 1:length(oFiles)){
			tmp = nf[(12*i-11):(12*i-12+nM),j]
			# add necessary header info
			tmp = matrix(tmp, ncol = 1)
			colnames(tmp) = headerInfo
			write.table(tmp, file = paste(fold,'/',oFiles[j],sep = ''),quote = F, row.names = F)
		}
	}
	
}