#' @noRd
.updateStudyName <- function(opts, ext)
{
	iniPath <- file.path(opts$simPath, "info.antares-output")
	infosIni <- readIniFile(iniPath)
	infosIni$general$name <- paste0(infosIni$general$name, ext)
	writeIni(listData = infosIni, pathIni =  iniPath, overwrite = TRUE)
	NULL
}


#' @noRd
.transformOutput <- function(out, antaresfbzone){
	out[, c("patch.Supply", "patch.Load", "patch.net_position", "patch.ENS",
			"patch.MRG", "patch.DENS", "patch.Date"):=NULL]
	setnames(out, "post_patch.MRG", "SPIL. ENRG")
	setnames(out, "post_patch.ENS", "UNSP. ENRG")
	setnames(out, "post_patch.net_position", "BALANCE")
	names(out) <- gsub("patch.", "", names(out))
	areas <- out[, .SD, .SDcols = c("area", "mcYear", "timeId", "PSP", "SPIL. ENRG", "UNSP. ENRG", "BALANCE")]
	links <- out[, .SD, .SDcols = c("area", "mcYear", "timeId", names(out)[names(out)%in% opts$linkList])]

	links <- melt.data.table(links, id.vars = c("area","mcYear", "timeId"))
	links <- links[!is.na(value)]
	links$variable <- as.character(links$variable)
	res <- unlist(strsplit(links$variable, " - "))
	res <- res[1:length(res)%%2==1]
	links <- links[res==links$area]
	setnames(links, "variable", "link")
	setnames(links, "value", "FLOW LIN.")
	links$area <- NULL
	links$`FLOW LIN.` <- round(links$`FLOW LIN.`, 0)

	setnames(out, "CWE", antaresfbzone)
	links2 <- melt.data.table(out, id.vars = c("area","mcYear", "timeId"), measure.vars = antaresfbzone)
	links2 <- links2[!is.na(links2$value)]

	links2$variable <- paste0(links2$area," - ", links2$variable)
	links2$area <- NULL
	setnames(links2, "variable", "link")
	links2$value <- round(links2$value, 0)
	setnames(links2, "value", "FLOW LIN.")
	links <- rbindlist(list(links, links2))
	list(areas = areas, links = links)
}


.getLinkExtremities = function(link_name){
	from_to = strsplit(as.character(link_name), " - ")[[1]]

	from = tolower(from_to[[1]])
	to = tolower(from_to[[2]])

	list(from, to)
}

removeAreas = function(patch_areas, areas_data,
					   links_data, add = FALSE,
					   sim_opts=antaresRead::simOptions()) {

	unused_areas = setdiff(getAreas(opts=sim_opts), patch_areas)

	interesting_links_data = data.table::copy(links_data)[
		,
		c("time", "day", "month", "hour") := NULL
	]
	interesting_links_data = interesting_links_data[
		,
		c("from", "to") := .getLinkExtremities(link),
		by=link
	]
	interesting_links_data = interesting_links_data[  # Only selects links between a zone to remove and one to keep
		(from %in% patch_areas & to %in% unused_areas) | (from %in% unused_areas & to %in% patch_areas)
	]
	interesting_links_data = interesting_links_data[  # Makes sure that the zones to keep are the destinations of all links
		from %in% patch_areas,
		c(
			"from",
			"to",
			"FLOW LIN."
		) := .(
			to,
			from,
			-`FLOW LIN.`
		)
	]
	interesting_links_data = interesting_links_data[
		,
		.(`FLOW LIN.` = sum(`FLOW LIN.`)),
		by = .(mcYear, timeId, to)
	]

	if (add) {
		interesting_links_data[, `FLOW LIN.` := -`FLOW LIN.`]
	}

	output = merge(
		areas_data[area %in% patch_areas],
		interesting_links_data,
		by.x=c("mcYear", "timeId", "area"),
		by.y=c("mcYear", "timeId", "to"),
		all.x = TRUE
	)
	output = output[
		is.na(`FLOW LIN.`),
		`FLOW LIN.` := 0
	]
	output = output[  # Reassigns flows from removed areas to production of kept areas and adjusts their BALANCE
		,
		c(
			"PSP",
			"BALANCE"
		) := .(
			PSP + `FLOW LIN.`,
			BALANCE + `FLOW LIN.`
		)
	]
	output = output[
		,
		`FLOW LIN.` := NULL
	]

	output
}
