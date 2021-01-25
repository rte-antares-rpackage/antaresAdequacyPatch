#' Extracts the maximum transfer capacity on each CB for the flow-based domain of a study
#'
#' @param sim_opts (list) Simulation options as given by antaresRead::setSimulationPath
#'
#' @return (data.table) Table containing the limit capacity for each critical
#'	branch on each typical day and for each hour.
#' @export
#'
#' @examples
#' \dontrun{
#' sim_opts = antaresRead::setSimulationPath("path/to/my/simulation")
#'
#' capacity_FB_data = extract_FB_capacity(sim_opts=sim_opts)
#' }
extract_FB_capacity = function(sim_opts=antaresRead::simOptions()) {

	capacity_FB_data = data.table::fread(
		paste(sim_opts$studyPath, "user/flowbased/second_member.txt", sep="/")
	)[
		,
		.(typical_day = Id_day, Id_hour, zone = "CWE", CB = Name, capacity = vect_b)
	]
	data.table::setnames(capacity_FB_data, function(colname){paste("capacity", colname, sep=".")})

	capacity_FB_data
}
