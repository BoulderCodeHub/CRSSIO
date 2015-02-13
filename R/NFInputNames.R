#' Get vector of CRSS natural inflow file names.
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
#' @examples
#' fileNames <- CRSSNFInputNames()
#' createCRSSDNFInputFiles('NaturalFlows1906-2012_withExtensions_1.8.15.xlsx','NFSinput/','2015-1-31',50,fileNames)
#' @seealso
#' \code{\link{createCRSSDNFInputFiles}}
#' 
CRSSNFInputNames <- function()
{
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
}


# not sure what the salt files are for
CRSSOldNatSaltInput <- function()
{
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

}