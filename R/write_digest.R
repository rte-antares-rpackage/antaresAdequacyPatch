#' @noRd
.add_csv_digest <- function(opts){

	#Write digest
	digetsWrite <- try({
		oldw <- getOption("warn")
		diges <- data.table::fread(system.file("format_output/digest.csv", package = "AdequacyPatch"))
		areas <- antaresRead::readAntares(timeStep = "annual", showProgress = FALSE, opts = opts)
		areas <- areas[, .SD, .SDcols = c(1:3,which(names(areas)%in%diges$Variable))]
		allNam <- names(areas)[-c(1:3)]
		areas[, c("timeId", "time"):= NULL]
		for (col in allNam) set(areas, j = col, value = as.numeric(areas[[col]]))
		allStats <- diges$CalcBuYear
		for(i in 1:length(allNam))
		{
			var <- allNam[i]
			fct <- allStats[i]
			areas[, c(var) := .(do.call(fct, args = list(get(var)))), by = area]
		}
		areas <- unique(areas)
		for (col in allNam) set(areas, j = col, value = as.character(areas[[col]], 0))
		coltoKeep <- match(names(areas)[-1], diges$Variable)
		unitKeep <- diges$Unit[coltoKeep]
		StatsKeep <- diges$Stats[coltoKeep]
		rentam <- names(areas)
		areas <- rbindlist(list(data.table(t(c("area", unitKeep))),
								data.table(t(c("area", StatsKeep))),
								areas), fill = FALSE)
		names(areas) <- rentam
		digets <- paste0(opts$simDataPath, "/mc-all/grid")
		if(!dir.exists(digets))dir.create(digets)

		write.table(areas, paste0(digets, "/digest.csv"), row.names = FALSE, sep = ";", quote = FALSE)
	}, silent = TRUE)

}
