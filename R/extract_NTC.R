#' Extracts the NTC links data from a study and formats the like the flow-based data
#'
#' @param areas (string or vector of strings) Areas between which we want to
#'	extract the links.
#' @param sim_opts (list) Simulation options as given by antaresRead::setSimulationPath
#' @param mcYears (integer) vector of years to read
#'
#' @return (list) such that $capacity is a data.table containing the maximum
#'	transfer capacity for each link (divided in Direct and Indirect)
#'	and $ptdf is a data.table containing, for each link, a PTDF of 1 for the
#'	origin country of the link if it is direct, or for the destination country
#'	if it is indirect.
#' @export
#'
#' @examples
#' \dontrun{
#' sim_opts = antaresRead::setSimulationPath("path/to/my/simulation")
#' areas = antaresRead::getAreas()
#'
#' links_NTC_data = extract_NTC_links(areas=areas, sim_opts=sim_opts)
#' }
extract_NTC_links = function(areas=NULL, sim_opts=antaresRead::simOptions(), mcYears = NULL) {

	links = antaresRead::getLinks(areas, internalOnly=TRUE, withTransmission=T, namesOnly=F)[
	  ,
	  .(link, transmission)
	]
	
	links_data = antaresRead::readAntares(linkCapacity=TRUE, links=links$link, mcYear = mcYears, opts=sim_opts)[
	  ,
	  .(mcYear, timeId, zone = link, transCapacityDirect, transCapacityIndirect)
	]

	links_data = merge(links_data, links, by.x="zone", by.y="link")
  links_data[transmission == "ignore", c("transCapacityDirect", "transCapacityIndirect") := 0]
  links_data[transmission == "infinite",  c("transCapacityDirect", "transCapacityIndirect") := 99999]
	
	links_data = data.table::melt(
		links_data,
		id.vars=c("mcYear", "timeId", "zone"),
		measure.vars=c("transCapacityDirect", "transCapacityIndirect"),
		variable.name="CB",
		value.name="capacity"
	)[
		CB == "transCapacityDirect",
		CB := paste(zone, "Direct", sep=":")
	][
		CB == "transCapacityIndirect",
		CB := paste(zone, "Indirect", sep=":")
	][
		,
		CB := as.character(CB)
	]

	capacity_NTC_data = links_data

	ptdf_NTC_data = links_data[
		,
		.(
			country = ifelse(
				strsplit(CB, split=":", fixed=TRUE)[[1]][[2]] == "Direct",
				strsplit(zone, split=" - ", fixed=TRUE)[[1]][[1]],
				strsplit(zone, split=" - ", fixed=TRUE)[[1]][[2]]
			),
			PTDF = 1
		),
		by=.(zone, CB)
	]


	data.table::setnames(capacity_NTC_data, function(colname){paste("capacity", colname, sep=".")})

	data.table::setnames(ptdf_NTC_data, function(colname){paste("ptdf", colname, sep=".")})

	list("capacity"=capacity_NTC_data, "ptdf"=ptdf_NTC_data)
}

