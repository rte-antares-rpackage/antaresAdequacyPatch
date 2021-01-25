opts <- setSimulationPath("C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/etude/new/BP19_FB18_2021_60mcAcc/output/20200910-1433eco-calcul pour adequacy patch")

aggregateResult(opts)

readAntares(clusters = "all")
verbose = 1
#Version which readAntares

linkTable <- .tableAggregateload(opts)
##All areas
areas <- getAreas(opts = opts)

linkTableArea <- linkTable[Folder == "area"]
linkTableLink <- linkTable[Folder == "link"]

sapply(areas, function(W, opts){
	print(W)
	dta <- readAntares(areas = W, mcYears = "all", showProgress = FALSE)
	dta <- .updateData(opts, dta, linkTableArea)
	antaresEditObject::writeOutputValues(dta, opts)
}, opts = opts)


link <- getLinks(opts = opts)
sapply(link, function(W, opts){
	print(W)
	dta <- readAntares(links = W, mcYears = "all", showProgress = FALSE)
	dta <- .updateData(opts, dta, linkTable)
	antaresEditObject::writeOutputValues(dta, opts)
}, opts = opts)


##Cluster
dta <- readAntares(clusters = areas[1], mcYears = "all", showProgress = FALSE)
#dta <- .updateData(opts, dta)
dtaE <- readAntares(clusters = areas[1], showProgress = FALSE)



