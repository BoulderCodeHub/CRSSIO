#' Get vector of natural flow and salt gage names, filenames, and abbreviations
#' 
#' These functions return the natural flow and salt file names 
#' (`nf_file_names()` and `natsalt_file_names()`), the natural flow gage names 
#' (`nf_gage_names()`), and the abbreviations (`nf_gage_abbrv()`) of the natural 
#' flow gage names in the standard node order used by Reclamation and expected 
#' by CRSS. 
#' 
#' @details 
#' `nf_file_names()` and `natsalt_file_names()` return file names that
#' CRSS is expecting to read in for natural flow and salt input data.
#' 
#' @param version The CRSS version number. Current version of CRSS is 6. Valid 
#'   versions are 1-6.
#'   
#' @return Vector of characters with 29 entries (file names, gage names, gage 
#'   abbreviations).
#' 
#' @examples
#' fileNames <- nf_file_names()
#' \dontrun{
#' iFiles <- 'NaturalFlows1906-2012_withExtensions_1.8.15.xlsx'
#' crssi_create_dnf_files(iFile,'NFSinput/','2015-1-31',50,fileNames)
#' }
#' @seealso
#' \code{\link{crssi_create_dnf_files}}
#' 
#' @export
#' @rdname nf_natsalt_names
nf_file_names <- function(version = 5)
{
	if (missing(version)) {
	  message(paste(
	    "`nf_file_names()` called without specifiying `version`.",
	    "  Default value of `version` will be removed in the next release.",
	    sep = "\n"
	  ))
	}
  
  if (version %in% 5:6) {
	  ff <- paste0(nf_gage_abbrv(), "NF.Inflow")
	  return (ff)
	} else if (version %in% 2:4 ) {
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
	  stop('Invalid version number in nf_file_names')
	}
}

#' @export
#' @rdname nf_natsalt_names
natsalt_file_names <- function(version = 5)
{
  if (missing(version)) {
    message(paste(
      "`natsalt_file_names()` called without specifiying `version`.",
      "  Default value of `version` will be removed in the next release.",
      sep = "\n"
    ))
  }
  
  if (version == 5:6) {
	  ff <- paste0(nf_gage_abbrv(), "NF.Inflow_Salt_Concentration")
	  return(ff)
	  
	} else if (version %in% 2:4) {
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
	} else if (version == 1) {
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
	  stop('Invalid version passed to natsalt_file_names().')
	}

}

#' @details
#' `nf_gage_names()` returns a vector with the USGS gage names used by 
#' CRSS  corresponding to the natural flow basins. The order matches the node 
#' order used by Reclamation and CRSS and thus should not be modified.
#' 
#' @export
#' @rdname nf_natsalt_names
nf_gage_names <- function()
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

#' @details 
#' `nf_gage_abbrv()` returns an abbreviated shorthand name for the natural flow
#' gages/nodes, because it is it is desirable to have shorthand names for many 
#' purposes, i.e., variable names.
#' 
#' @examples
#' 
#' # get the gage name for node 20 
#' nf_gage_names()[20]
#' # and its shorthand name
#' nf_gage_abbrv()[20]
#' # and then see the CRSS natural inflow file name corresponding to this gage
#' nf_file_names()[20]
#' 
#' @export
#' @rdname nf_natsalt_names
nf_gage_abbrv <- function()
{
  c("GlenwoodSprings", "Cameo", "TaylorPark", "BlueMesa", "Crystal", 
    "GrandJunction", "CiscoDolores", "CiscoColorado", "Fontenelle", 
    "GreenRiverWY", "Greendale", "Maybell", "Lily", "Randlett", "Watson", 
    "GreenRiverUTGreen", "GreenRiverUTSanRafael", "Archuleta", "Bluff", 
    "LeesFerry", "LeesFerryParia", "Cameron", "GrandCanyon", "Littlefield",
    "Hoover", "Davis", "Alamo", "Parker", "Imperial")
}
