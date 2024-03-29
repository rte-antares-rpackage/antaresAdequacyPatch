#' Computes the positive part of a numeric.
#'
#' The positive part is defined as follows:
#'	.pos(x) = x if x >= 0
#'	.pos(x) = 0 otherwise
#'
#' @param x (numeric)
#'
#' @return (numeric) the positive part of x
#' @export
#'
#' @examples
#' .pos(3)  # 3
#' .pos(-5)  # 5
.pos <- function(x) {
	if (x >= 0)
		x
	else
		0
}


#' Extracts the data relevant for the Adequacy patch for a simulation output.
#'
#' It selects the time-steps in an ANtares study when at least one country is in
#' loss of load.
#'
#' @param areas (string or vector of strings) what areas the patch should
#' be applied on. Default: ""
#' @param virtual_areas (string or vector of strings) Virtual areas of the study,
#' excluded from the patch. Default: NULL
#' @param mcYears (numeric or vector of numeric) The Monte-Carlo years to
#' extract from. The special value "all" extracts all Monte-Carlo Years.
#' Default: "all"
#' @param sim_opts (list) Simulation options as given by antaresRead::setSimulationPath
#'
#' @return (data.table) Table containing, for each mcYear, time-step and country,
#'	the DENS (domestic Energy Not Served) and DMRG (Domestic Margin).
#' @export
#'
#' @examples
#' \dontrun{
#' sim_opts = antaresRead::setSimulationPath("path/to/my/simulation")
#' areas = antaresRead::getAreas()
#'
#' patch_data = extract_patch(areas=areas, mcYears=c(1, 3), sim_opts=sim_opts)
#' }
#'
#'
extract_patch = function(areas, virtual_areas, mcYears = "all",
						 sim_opts=antaresRead::simOptions()) {


	links = antaresRead::getLinks(areas, opts=sim_opts)

	patch_data = antaresRead::readAntares(
		areas=areas,
		links=links,
		opts=sim_opts,
		mcYears=mcYears,
		showProgress = FALSE
	)

	patch_data = removeAreas(
		areas,
		patch_data$areas,
		patch_data$links,
		sim_opts=sim_opts
	)

	#remove areas who are not in study domain
	# patch_data = antaresRead::removeVirtualAreas(patch_data,
	# 											 storageFlexibility = virtual_areas,
	# 											 newCols=FALSE)

	# patch_data = patch_data$areas[area %in% areas]
	patch_data = patch_data[area %in% areas]
	
	patch_data = patch_data[
		patch_data[
			,
			patch := max(`UNSP. ENRG`) > 0,
			by=.(mcYear, timeId)
		][, patch]
	][
	  , `ALL MISC. DTG` := rowSums(.SD), .SDcols = grep("MISC. DTG", colnames(patch_data), value = T)
	][
	  , `RENEWABLES` := rowSums(.SD), .SDcols = if ("WIND" %in% colnames(patch_data)) {c("WIND", "SOLAR")} else
	    {c("WIND OFFSHORE", "WIND ONSHORE", "SOLAR CONCRT.","SOLAR PV", "SOLAR ROOFT", "RENW. 1", "RENW. 2", "RENW. 3", "RENW. 4")}
	][
		,
		.(
			mcYear,
			timeId,
			Date = as.character(lapply(time, substr, start=1, stop=10)),

			area,

			PSP,

			Supply = PSP + `MISC. NDG`
			+ `H. ROR` + `RENEWABLES`
			+ NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `ALL MISC. DTG`
			+ `H. STOR` - `H. PUMP` + `ROW BAL.`,

			Load = LOAD,

			net_position = BALANCE,

			ENS = `UNSP. ENRG`,

			MRG = `DTG MRG`
		)
	][
		,
		c("DENS", "D_available_energy") := .(
			# as.numeric(sapply(Load - Supply - MRG, .pos)),
			# as.numeric(sapply(Supply + MRG - Load, .pos))

			as.numeric(sapply(Load - Supply, .pos)),
			as.numeric(Supply - Load)
		)
	]

	data.table::setnames(patch_data, function(colname){paste("patch", colname, sep=".")})

	patch_data
}


