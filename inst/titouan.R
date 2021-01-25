library(antaresRead)
library(AdequacyPatch)
library(antaresEditObject)
library(data.table)
library(fs)
library(plyr)
library(doParallel)
library(progressr)
library(progress)
#library(antaresFlowbased)

opts <- setSimulationPath("C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/etude/new/BP19_FB18_2021_60mcAcc/output/20200910-1433eco-calcul pour adequacy patch")


areas = c("fr", "at", "be", "de", "nl", "es", "ukgb", "ch", "ie", "itn", "zz_flowbased")

virtual_areas = getAreas(select = "_", regexpSelect = TRUE, exclude = c("zz_flowbased"), regexpExclude = FALSE)

run_adq(opts = opts,
		areas = areas,
		virtual_areas = virtual_areas,
		mcYears = "all",
		antaresfbzone = "model_description_fb",
		ext = "_adq",
		nbcl = 4)



library(AdequacyPatch)
library(fbAntares)
library(antaresRead)

##Load data before ADQ
opts <- setSimulationPath("D:/Users/fauduetale/Desktop/BP20_relance_final_15092020_2024_DE8.5_UK3_ITN3_ITCS3/output/20200930-2331eco")
before = readAntares(areas=areas, mcYears = "all")


dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl", "at"),
								links = c("be - de","be - fr","be - nl",
										  "de - fr","de - nl"),
								select = c("LOLD", "UNSP. ENRG", "DTG MRG",
										   "UNSP. ENRG", "BALANCE", "FLOW LIN."),
								mcYears = 1:10, opts = opts)



###Load data after adq
opts <- setSimulationPath("D:/Users/fauduetale/Desktop/BP20_relance_final_15092020_2024_DE8.5_UK3_ITN3_ITCS3/output/20200930-2331eco_adq")
after = readAntares(areas=areas, mcYears = "all")
odta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl", "at"),
								 links = c("be - de","be - fr","be - nl",
								 		  "de - fr","de - nl"),
								 select = c("LOLD", "UNSP. ENRG", "DTG MRG",
								 		   "UNSP. ENRG", "BALANCE", "FLOW LIN.", "PSP"),
								 mcYears = 1:10, opts = opts)



 idC <- c(antaresRead::getIdCols(dta$areas))
 idC <- idC[idC!="area"]
 LOLD <- dta$areas[,lapply(.SD, sum), by = idC, .SDcols = "LOLD"]
 LOLD <- LOLD[LOLD!=0]
 LOLD[,LOLD := NULL]

 # Merge to filter data
 dta$areas <- merge(dta$areas, LOLD, by =  idC)
 odta$areas <- merge(odta$areas, LOLD, by =  idC)

plotNetPositionFB(data = dta,
				  dayType = 6,
				  country1 = 'FR',
				  country2 = "DE",
				  hour = 19,
				  areaName = "cwe", odata = odta,
				  filteringEmptyDomains = TRUE)


