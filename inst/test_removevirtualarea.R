library(antaresRead)


opts <- setSimulationPath("C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/etude/new/BP19_FB18_2021_60mcAcc/output/20200910-1433eco-calcul pour adequacy patch")


areas = c("fr", "lu", "de", "cz", "pl", "ch", "at", "itn", "nl", "be", "es", "non", "se1", "model_description_fb_adq")
virtual_areas = getAreas(select = "_", regexpSelect = TRUE, exclude = c("model_description_fb"), regexpExclude = FALSE)
all_areas = antaresRead::getAreas(opts=opts)
unused_areas = setdiff(all_areas, union(areas, virtual_areas))

dta <- readAntares(areas = areas, links = 'all', mcYears = 1)
dta <- removeVirtualAreas(dta, storageFlexibility = c(unused_areas, virtual_areas) ,  newCols=FALSE)

dta2 <- readAntares(areas = areas, links = 'all', mcYears = 1)
dta2 <- removeVirtualAreas(dta2, storageFlexibility = c(unused_areas, virtual_areas) ,  newCols=FALSE)
fsetequal(dta$areas, dta2$areas[area %in% unique(dta$areas$area)])

##Test 1 charger les zone qui sont pas dans areas -> inutil on peut les enlver de l'étude

dta2 <- readAntares(areas = areas, links = opts$linksDef[from %in% areas | to %in% areas]$link, mcYears = 1)
dta2 <- removeVirtualAreas(dta2, storageFlexibility = c(virtual_areas) ,  newCols=FALSE)
fsetequal(dta$areas, dta2$areas[area %in% unique(dta$areas$area)])
fsetequal(dta$links, dta2$links[link %in% unique(dta$links$link)])
data.table::fsetdiff(dta$areas, dta2$areas[area %in% unique(dta$areas$area)])
