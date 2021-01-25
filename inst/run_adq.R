library(antaresRead)
library(AdequacyPatch)
library(antaresEditObject)
library(data.table)
library(fs)
library(plyr)
library(doParallel)
library(progressr)
library(progress)
library(pipeR)
#library(antaresFlowbased)

args <- c("C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/etude/new/BP19_FB18_2021_60mcAcc/output/20200910-1433eco-calcul pour adequacy patch",
		  "-adq")

if(length(args)==0){
	stop("You specify a directory of a study")
}

if(length(args)==1){
	invisible(readline(prompt="You will overwrite actual study result, press [enter] to continue"))
	args <- c(args, NA)
}

old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = paste(old_path, "C:/rtools40/mingw64/bin", sep = ";"))
Sys.setenv(PATH = paste(old_path, "C:/Program Files/RTE/Antares/7.0.0/bin", sep = ";"))

opts <- setSimulationPath(args[1])

areas <- c("fr", "at", "be", "de", "nl", "es", "ukgb", "ch", "ie", "itn", "zz_flowbased")
virtual_areas = getAreas(select = "_", regexpSelect = TRUE, exclude = c("zz_flowbased"), regexpExclude = FALSE)


system.time(run_adq(opts = opts,
					areas = areas,
					virtual_areas = virtual_areas,
					mcYears = "all",
					antaresfbzone = "zz_flowbased",
					ext = args[2],
					nbcl = 8, thresholdFilter = 100))

