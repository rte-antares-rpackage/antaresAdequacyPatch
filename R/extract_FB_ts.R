#' Extracts the flow-based time-series from an Antares study
#'
#' @param sim_opts (list) Simulation options as given by antaresRead::setSimulationPath
#'
#' @return (data.table) Table containing the typical day for each day in each
#'	Monte-Carlo year of the simulation.
#' @export
#'
#' @examples
#' \dontrun{
#' sim_opts = antaresRead::setSimulationPath("path/to/my/simulation")
#'
#' ts_FB_data = extract_FB_ts(sim_opts=sim_opts)
#' }
extract_FB_ts = function(sim_opts=antaresRead::simOptions()) {
	ts = data.table::fread(
		paste(sim_opts$studyPath, "user/flowbased/ts.txt", sep="/"),
		header=TRUE
	)

	ts_FB_data = data.table::melt(
		ts, id.vars="Date", variable.name="mcYear", value.name="typical_day"
	)[
		,
		.(mcYear = as.numeric(mcYear), Date, typical_day)
	]
	data.table::setnames(ts_FB_data, function(colname){paste("ts", colname, sep=".")})

	ts_FB_data
}
