#' Get vector of CRSS natural inflow file names
#' 
#' \code{CRSSNFInputNames} returns a vector with the CRSS natural inflow file
#' names in the correct node order.
#' 
#' This function returns a vector with the file names used by CRSS to import
#' the natural flows. The order matches the node order used by Reclamation and
#' CRSS and thus should not be modified. As the structure to CRSS changes, 
#' the file names also may change. If this occurs in the future, the function
#' will be modified to return different values for different versions of CRSS.
#' 
#' This is the default function used by \code{\link{createCRSSDNFInputFiles}} 
#' for creating the input files for CRSS.
#' 
#' @return Vector of characters (file names).
#' 
#' @param version The CRSS version number. Current version of CRSS is 2. Valid 
#' versions are 1 or 2.
#' 
#' @examples
#' fileNames <- CRSSNFInputNames()
#' \dontrun{
#' iFiles <- 'NaturalFlows1906-2012_withExtensions_1.8.15.xlsx'
#' createCRSSDNFInputFiles(iFile,'NFSinput/','2015-1-31',50,fileNames)
#' }
#' @seealso
#' \code{\link{createCRSSDNFInputFiles}}, \code{\link{CRSSNatSaltInputNames}}
#' 
#' @export
CRSSNFInputNames <- function(version=2)
{
	if(version ==2){
	  return(c('UpperColoradoReach.Inflow',
		'UpperColoradoAboveCameo_GainsAboveCameo.Local_Inflow',
		'TaylorPark.Inflow',
		'TaylorAboveBlueMesa_GainsAboveBlueMesa.Local_Inflow',
		'GunnisonRiverAboveCrystal_GainsAboveCrystal.Local_Inflow',
		'GunnisonRiverAboveGrandJunction_GainsGunnisonRiverAbvGrandJunction.Local_Inflow',
		'DoloresRiver.Inflow',
		'UpperColoradoCameoToGunnison_GainsAboveCisco.Local_Inflow',
		'GreenRAboveFontenelle.Inflow',
		'GreenRAboveGreenRiverWY_GainsAboveGRWY.Local_Inflow',
		'GreenRAboveFlamingGorge_GainsAboveGreendale.Local_Inflow',
		'YampaRiver.Inflow',
		'LittleSnakeRiver.Inflow',
		'DuchesneAboveStarv.Inflow',
		'WhiteRiverAboveWatson.Inflow',
		'GreenRWhiteToSanRafael_GainsAboveGreenRiverUT.Local_Inflow',
		'SanRafaelRiver.Inflow',
		'SJAboveNavajo.Inflow',
		'SanJuanTributaries.Inflow',
		'SanJuanPowell_GainsAboveLeesFerry.Local_Inflow',
		'PariaRiver.Inflow',
		'LittleColoradoRiver.Inflow',
		'CoRivLittleCOToVirgin_GainsAboveGC.Local_Inflow',
		'VirginRiver.Inflow',
		'CoRivVirginToMead_GainsAboveHoover.Local_Inflow',
		'CoRivMeadToMohave_GainsAboveDavis.Local_Inflow',
		'CoRivMohaveToHavasu_BillWilliamsRiver.Local_Inflow',
		'CoRivMohaveToHavasu_GainsAboveParker.Local_Inflow',
		'AboveImperialDamColoradoR_GainsOnColoRAboveImperialDam.Local_Inflow'))
	} else if(version == 1){
	  return(c('UpperColoradoReach.Inflow',
	  'UpperColoradoReach_GainsAboveCameo.Local_Inflow',
	  'TaylorPark.Inflow',                              
	  'TaylorAboveBlueMesa_GainsAboveBlueMesa.Local_Inflow',
	  'GunnisonRiverAboveCrystal_GainsAboveCrystal.Local_Inflow',
	  'GunnisonRiverAboveGrandJunction_GainsGunnisonRiverAbvGrandJunction.Local_Inflow',
	  'DoloresRiver.Inflow',
	  'UpperColoradoReach_GainsAboveCisco.Local_Inflow',
	  'GreenRAboveFontenelle.Inflow',          
	  'FontToFlamingGorge_GainsAboveGRWY.Local_Inflow',                                
	  'FontToFlamingGorge_GainsAboveGreendale.Local_Inflow',                            
	  'YampaRiver.Inflow',                                                              
	  'YampaRiver_LittleSnakeRInflow.Local_Inflow',                                     
	  'DuchesneAboveStarv.Inflow',                                                      
	  'WhiteRiver.Inflow',                                                              
	  'GreenRWhiteToSanRafael_GainsAboveGreenRiverUT.Local_Inflow',                     
	  'SanRafaelRiver.Inflow',                                                          
	  'SJAboveNavajo.Inflow',                                                           
	  'SJBelowNavajo_GainsAboveBluff.Local_Inflow',                                     
	  'SanJuanPowell_GainsAboveLeesFerry.Local_Inflow',                                 
	  'CoRivPowellToVirgin_PariaGains.Local_Inflow',                                    
	  'CoRivPowellToVirgin_LittleCoR.Local_Inflow',                                     
	  'CoRivPowellToVirgin_GainsAboveGC.Local_Inflow',                                  
	  'VirginRiver.Inflow',                                                             
	  'CoRivVirginToMead_GainsAboveHoover.Local_Inflow',                                
	  'CoRivMeadToMohave_GainsAboveDavis.Local_Inflow',                                 
	  'CoRivMohaveToHavasu_BillWilliamsRiver.Local_Inflow',                             
	  'CoRivMohaveToHavasu_GainsAboveParker.Local_Inflow',                              
	  'AboveImperialDamColoradoR_GainsOnColoRAboveImperialDam.Local_Inflow')) 
	} else{
	  stop('Invalid version number in CRSSNFInputNames')
	}
}


#' Get vector of CRSS natural salt input file names
#' 
#' \code{CRSSNatSaltInputNames} returns a vector with the CRSS natural salt file
#' names in the correct node order.
#' 
#' This function returns a vector with the file names used by CRSS to import
#' the natural salt. The order matches the node order used by Reclamation and
#' CRSS and thus should not be modified. As the structure to CRSS changes, 
#' the file names also may change. If this occurs in the future, the function
#' will be modified to return different values for different versions of CRSS.
#' 
#' @return Vector of characters (file names).
#' 
#' @param version The CRSS version number. Current version of CRSS is 2. Valid 
#' versions are 1 or 2.
#' 
#' @examples
#' fileNames <- CRSSNatSaltInputNames(2)
#' @seealso
#' \code{\link{CRSSNFInputNames}}
#' 
#' @export
CRSSNatSaltInputNames <- function(version=2)
{
	if(version == 2){
	  return(c('UpperColoradoReach.Inflow_Salt_Concentration',
      'UpperColoradoAboveCameo_GainsAboveCameo.Local_Inflow_Salt_Concentration',
      'TaylorPark.Inflow_Salt_Concentration',
      'TaylorAboveBlueMesa_GainsAboveBlueMesa.Local_Inflow_Salt_Concentration',
      'GunnisonRiverAboveCrystal_GainsAboveCrystal.Local_Inflow_Salt_Concentration',
      'GunnisonRiverAboveGrandJunction_GainsGunnisonRiverAbvGrandJunction.Local_Inflow_Salt_Concentration',
      'DoloresRiver.Inflow_Salt_Concentration',
      'UpperColoradoCameoToGunnison_GainsAboveCisco.Local_Inflow_Salt_Concentration',
      'GreenRAboveFontenelle.Inflow_Salt_Concentration',
      'GreenRAboveGreenRiverWY_GainsAboveGRWY.Local_Inflow_Salt_Concentration',
      'GreenRAboveFlamingGorge_GainsAboveGreendale.Local_Inflow_Salt_Concentration',
      'YampaRiver.Inflow_Salt_Concentration','LittleSnakeRiver.Inflow_Salt_Concentration',
      'DuchesneAboveStarv.Inflow_Salt_Concentration',
      'WhiteRiverAboveWatson.Inflow_Salt_Concentration',
      'GreenRWhiteToSanRafael_GainsAboveGreenRiverUT.Local_Inflow_Salt_Concentration',
      'SanRafaelRiver.Inflow_Salt_Concentration',
      'SJAboveNavajo.Inflow_Salt_Concentration',
      'SanJuanTributaries.Inflow_Salt_Concentration',
      'SanJuanPowell_GainsAboveLeesFerry.Local_Inflow_Salt_Concentration',
      'PariaRiver.Inflow_Salt_Concentration',
      'LittleColoradoRiver.Inflow_Salt_Concentration',
      'CoRivLittleCOToVirgin_GainsAboveGC.Local_Inflow_Salt_Concentration',
      'VirginRiver.Inflow_Salt_Concentration',
      'CoRivVirginToMead_GainsAboveHoover.Local_Inflow_Salt_Concentration',
      'CoRivMeadToMohave_GainsAboveDavis.Local_Inflow_Salt_Concentration',
      'CoRivMohaveToHavasu_BillWilliamsRiver.Local_Inflow_Salt_Concentration',
      'CoRivMohaveToHavasu_GainsAboveParker.Local_Inflow_Salt_Concentration',
      'AboveImperialDamColoradoR_GainsOnColoRAboveImperialDam.Local_Inflow_Salt_Concentration'))
	} else if(version == 1){
  return(c('UpperColoradoReach.Inflow_Salt_Concentration',
		'UpperColoradoReach_GainsAboveCameo.Local_Inflow_Salt_Concentration',
		'TaylorPark.Inflow_Salt_Concentration',
		'TaylorAboveBlueMesa_GainsAboveBlueMesa.Local_Inflow_Salt_Concentration',
		'GunnisonRiverAboveCrystal_GainsAboveCrystal.Local_Inflow_Salt_Concentration',
		'GunnisonRiverAboveGrandJunction_GainsGunnisonRiverAbvGrandJunction.Local_Inflow_Salt_Concentration',
		'DoloresRiver.Inflow_Salt_Concentration',
		'UpperColoradoReach_GainsAboveCisco.Local_Inflow_Salt_Concentration',
		'GreenRAboveFontenelle.Inflow_Salt_Concentration',
		'FontToFlamingGorge_GainsAboveGRWY.Local_Inflow_Salt_Concentration',
		'FontToFlamingGorge_GainsAboveGreendale.Local_Inflow_Salt_Concentration',
		'YampaRiver.Inflow_Salt_Concentration',
		'YampaRiver_LittleSnakeRInflow.Local_Inflow_Salt_Concentration',
		'DuchesneAboveStarv.Inflow_Salt_Concentration',
		'WhiteRiver.Inflow_Salt_Concentration',
		'GreenRWhiteToSanRafael_GainsAboveGreenRiverUT.Local_Inflow_Salt_Concentration',
		'SanRafaelRiver.Inflow_Salt_Concentration',
		'SJAboveNavajo.Inflow_Salt_Concentration',
		'SJBelowNavajo_GainsAboveBluff.Local_Inflow_Salt_Concentration',
		'SanJuanPowell_GainsAboveLeesFerry.Local_Inflow_Salt_Concentration',
		'CoRivPowellToVirgin_PariaGains.Local_Inflow_Salt_Concentration',
		'CoRivPowellToVirgin_LittleCoR.Local_Inflow_Salt_Concentration',
		'CoRivPowellToVirgin_GainsAboveGC.Local_Inflow_Salt_Concentration',
		'VirginRiver.Inflow_Salt_Concentration',
		'CoRivVirginToMead_GainsAboveHoover.Local_Inflow_Salt_Concentration',
		'CoRivMeadToMohave_GainsAboveDavis.Local_Inflow_Salt_Concentration',
		'CoRivMohaveToHavasu_BillWilliamsRiver.Local_Inflow_Salt_Concentration',
		'CoRivMohaveToHavasu_GainsAboveParker.Local_Inflow_Salt_Concentration',
		'AboveImperialDamColoradoR_GainsOnColoRAboveImperialDam.Local_Inflow_Salt_Concentration'))
	} else{
	  stop('Invalid version passed to CRSSNatSaltInputNames.')
	}

}

#' Get vector of CRSS natural inflow USGS gage names
#' 
#' \code{nfGageNames} returns a vector with the CRSS natural flow node USGS gage
#' names in the correct node order.
#' 
#' This function returns a vector with the USGS gage names used by CRSS 
#' corresponding to the natural flow basins. The order matches the node order 
#' used by Reclamation and CRSS and thus should not be modified.
#' 
#' The gage names returned here, match the CRSS natural inflow slot names 
#' returned by \code{\link{CRSSNFInputNames}}
#' 
#' @return Vector of characters (file names).
#' @examples
#' # get the gage name for node 20 
#' nfGageNames()[20]
#' # and then see the CRSS natural inflow slot name corresponding to this gage
#' CRSSNFInputNames()[20]
#' @seealso
#' \code{\link{CRSSNFInputNames}}
#' 
#' @export
nfGageNames <- function()
{
  c("Colorado River At Glenwood Springs, CO", 
    "Colorado River Near Cameo, CO",
    "Taylor River Below Taylor Park Reservoir, CO", 
    "Gunnision River Above Blue Mesa Reservoir,CO",
    "Gunnison River At Crystal Reservoir,CO", 
    "Gunnison River Near Grand Junction, CO",
    "Dolores River Near Cisco, UT", 
    "Colorado River Near Cisco UT",
    "Green R Bel Fontenelle Res WY", 
    "Green R. Nr Green River, WY",
    "Green River Near Greendale, UT", 
    "Yampa River Near Maybell, CO",
    "Little Snake River Near Lily, CO", 
    "Duchesne River Near Randlett, UT",
    "White River Near Watson, UT", 
    "Green River At Green River, UT",
    "San Rafael River Near Green River, UT", 
    "San Juan River Near Archuleta,NM",
    "San Juan River Near Bluff, UT", 
    "Colorado R At Lees Ferry, AZ",
    "Paria R At Lees Ferry, AZ", 
    "Little Colorado River Near Cameron, AZ",
    "Colorado River Near Grand Canyon, AZ", 
    "Virgin River At Littlefield, AZ",
    "Colorado River Below Hoover Dam, AZ-NV", 
    "Colorado River Below Davis Dam, AZ-NV",
    "Bill Williams River Below Alamo Dam, AZ", 
    "Colorado River Below Parker Dam, AZ-CA",
    "Colorado River Above Imperial Dam, AZ"
  )
}

#' Get vector of CRSS natural inflow shorthand names
#' 
#' \code{nfShortNames} returns a vector with the CRSS natural flow node 
#' shorthand names in the correct node order.
#' 
#' For many purposes, it is desirable to have shorthand names for the natural 
#' flow nodes used in CRSS. This function returns a vector with the shorthand 
#' names used by CRSS corresponding to the the natural flow basins. The order 
#' matches the node order used by Reclamation and CRSS and thus should not be 
#' modified.
#' 
#' The shorthand names returned here, match the CRSS natural inflow slot names 
#' returned by \code{\link{CRSSNFInputNames}}
#' 
#' @return Vector of characters (file names).
#' @examples
#' # get the gage name for node 20 
#' nfGageNames()[20]
#' # and its shorthand name
#' nfShortNames()[20]
#' # and then see the CRSS natural inflow slot name corresponding to this gage
#' CRSSNFInputNames()[20]
#' @seealso
#' \code{\link{CRSSNFInputNames}}, \code{\link{nfGageNames}}
#' 
#' @export
nfShortNames <- function()
{
  c("GlenwoodSprings", "Cameo", "TaylorPark", "BlueMesa", "Crystal", 
    "GrandJunction", "CiscoDolores", "CiscoColorado", "Fontenelle", 
    "GreenRiverWY", "Greendale", "Maybell", "Lily", "Randlett", "Watson", 
    "GreenRiverUTGreen", "GreenRiverUTSanRafael", "Archuleta", "Bluff", 
    "LeesFerry", "LeesFerryParia", "Cameron", "GrandCanyon", "Littlefield",
    "Hoover", "Davis", "Alamo", "Parker", "Imperial")
}
