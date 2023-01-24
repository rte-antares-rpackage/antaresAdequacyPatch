#' @noRd
.complete <- function(areas_data, areas_new)
{
	keys <- c("area", "mcYear", "timeId")
	areas_new$LOLD <- 0
	areas_new$LOLD[areas_new$`UNSP. ENRG` > 1] <- 1
	areas_new$LOLP <- areas_new$LOLD * 100

	setkeyv(areas_new, keys)
	setkeyv(areas_data, keys)

	setnames(areas_data, "SPIL. ENRG", "DTM")
	setnames(areas_new, "SPIL. ENRG", "DTM")

	setnames(areas_data, "UNSP. ENRG", "NSPE")
	setnames(areas_new, "UNSP. ENRG", "NSPE")

	setnames(areas_data, "BALANCE", "BAL")
	setnames(areas_new, "BALANCE", "BAL")
	areas_data[areas_new, `:=`( DTM = i.DTM, NSPE = i.NSPE, BAL = i.BAL, LOLD = i.LOLD, PSP = i.PSP)]
	setnames(areas_data,  "DTM", "SPIL. ENRG")
	setnames(areas_data,  "NSPE", "UNSP. ENRG")
	setnames(areas_data,  "BAL", "BALANCE")
}

#' @noRd
.write_adq_area <- function(opts, areas_data, output, links_data, areaslist){

	#output$areas = removeAreas(areaslist, output$areas, links_data, add=TRUE, sim_opts=opts)

	.complete(areas_data, output$areas)

	antaresEditObject::writeOutputValues(areas_data, opts)
}

