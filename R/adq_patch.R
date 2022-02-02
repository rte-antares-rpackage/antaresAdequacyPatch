#' Applies the Adequacy patch on given DENS and DMRG for countries and
#' constrained by the flow_based and NTC data.
#'
#' The Adequacy patch is a post-processing phase on an Antares study simulation,
#' applying the local-matching and curtailment sharing rules as defined by the
#' EUPHEMIA to correct situations with at least one country in loss of load.
#'
#' This function does not solve anything itself, it sets up and transfers the
#' relevant data to a python file that builds and solves the problem using OR-TOOLS.
#'
#' @param patch_data (data.table) DENS and DMRG for each country at each time-step
#' @param ts_FB_data (data.table) typical day for each day
#' @param capacity_FB_data (data.table) Capacity on each critical branch in the
#'	flow-based domain depeding on the typical day
#' @param capacity_NTC_data (data.table) Maximum transfer capacity of each NTC
#'	border, mimicking capacity_FB_data
#' @param ptdf_FB_data (data.table) PTDF for each country on each critical branch
#' @param ptdf_NTC_data (data.table) Mimics ptdf_FB_data for each border
#' @param sim_opts (string) Simulation options, as returned by antaresRead::setSimulationPath
#' @param core_ahc (boolean) TRUE for using CORE area, FALSE for using CWE area
#' @param log_detail (boolean) TRUE for more details
#'
#' @return (data.table) Table giving the MRG, ENS and net-position for each country
#'	at each time-step
#' @export
#'
#' @examples
#' \dontrun{
#' sim_opts = antaresRead::setSimulationPath("path/to/my/simulation")
#' areas = antaresRead::getAreas()
#'
#' patch_data = extract_patch(areas=areas, mcYears=c(1, 3), sim_opts=sim_opts)
#' ts_FB_data = extract_FB_ts(sim_opts=sim_opts)
#' capacity_FB_data = extract_FB_capacity(sim_opts=sim_opts)
#' ptdf_FB_data = extract_FB_ptdf(sim_opts=sim_opts)
#' links_NTC_data = extract_NTC_links(areas=areas, sim_opts=sim_opts)
#'
#' output = adq_patch(
#'	patch_data,
#'	ts_FB_data,
#'	capacity_FB_data, links_NTC_data$capacity,
#'	ptdf_FB_data, links_NTC_data$ptdf,
#'  sim_opts, core_ahc=FALSE, log_detail=FALSE
#' )
#' }
adq_patch = function(patch_data, ts_FB_data,
					 capacity_FB_data, capacity_NTC_data,
					 ptdf_FB_data, ptdf_NTC_data, sim_opts, core_ahc, log_detail) {

	# Remove NTC links already in FB
	used_link = function(link_name, FB_countries) {
		from_to = strsplit(link_name, " - ")[[1]]
		from = from_to[[1]]
		to = from_to[[2]]

		from %in% FB_countries & to %in% FB_countries
	}

	FB_links <- getLinks(areas = "zz_flowbased", opts = sim_opts)
	ptdf_NTC_data = ptdf_NTC_data[!ptdf.zone %in% FB_links]

	# capacity_NTC_data = capacity_NTC_data[!(sapply(capacity.zone,
	# 											   function(link_name){
	# 											   	used_link(link_name, FB_countries)}))]

	# Combine all ptdf
	ptdf_data = rbind(ptdf_FB_data, ptdf_NTC_data)

	# Create ids for string fields (because of rAMPL setData)
	zones_id = ptdf_data[, .(zone.id = .GRP), by=ptdf.zone]
	cb_id = merge(ptdf_data, zones_id, by = "ptdf.zone")[, .(zone.id = zone.id[[1]], cb.id = .GRP), by=ptdf.CB]

	countries_id = ptdf_data[, .(country.id = .GRP), by=ptdf.country]

	# Replaces zones, CB and countries names by ids
	if(core_ahc){
		ptdf = merge(
			merge(
				merge(
					zones_id,
					ptdf_data,
					by="ptdf.zone"
				),
				countries_id,
				by=c("ptdf.country")
			),
			cb_id[, .(ptdf.CB, cb.id)],
			by=c("ptdf.CB")
		)[
			,
			.(
				zone.id, cb.id, country.id,  # AMPL sets : FB_zones, CB[zone], countries[zone]
				PTDF = ptdf.PTDF  # AMPL parameter : PTDF
			)
		]
	} else {
		ptdf = merge(
			merge(
				merge(
					zones_id,
					ptdf_data,
					by="ptdf.zone"
				),
				countries_id,
				by=c("ptdf.country")
			),
			cb_id,
			by=c("zone.id", "ptdf.CB")
		)[
			,
			.(
				zone.id, cb.id, country.id,  # AMPL sets : FB_zones, CB[zone], countries[zone]
				PTDF = ptdf.PTDF  # AMPL parameter : PTDF
			)
		]
	}

	# Replaces zones and CB names by ids
	capacity_FB = merge(
		merge(
			zones_id,
			capacity_FB_data,
			by.x="ptdf.zone",
			by.y="capacity.zone"
		),
		cb_id,
		by.x=c("zone.id", "capacity.CB"),
		by.y=c("zone.id", "ptdf.CB")
	)[
		,
		.(
			capacity.typical_day, capacity.Id_hour,
			zone.id, cb.id,  # AMPL sets : FB_zones, CB[zone]
			capacity = capacity.capacity  # AMPL parameter : capacity
		)
	]
	if (core_ahc){
		capacity_NTC = merge(
			merge(
				zones_id,
				capacity_NTC_data,
				by.x="ptdf.zone",
				by.y="capacity.zone"
			),
			cb_id,
			by.x=c("zone.id", "capacity.CB"),
			by.y=c("zone.id", "ptdf.CB")
		)[
			,
			.(
				capacity.timeId,
				cb.id,  # AMPL sets : FB_zones, CB[zone]
				capacity = capacity.capacity  # AMPL parameter : capacity
			)
		]
	} else {
		capacity_NTC = merge(
			merge(
				zones_id,
				capacity_NTC_data,
				by.x="ptdf.zone",
				by.y="capacity.zone"
			),
			cb_id,
			by.x=c("zone.id", "capacity.CB"),
			by.y=c("zone.id", "ptdf.CB")
		)[
			,
			.(
				capacity.timeId,
				zone.id, cb.id,  # AMPL sets : FB_zones, CB[zone]
				capacity = capacity.capacity  # AMPL parameter : capacity
			)
		]
	}

	# Replaces countries names by ids
	patch = merge(
		patch_data,
		countries_id,
		by.x="patch.area",
		by.y="ptdf.country"
	)[
		,
		.(
			patch.mcYear, patch.Date, patch.timeId,  # Time step
			country.id,  # AMPL set : all_countries
			# DENS = patch.DENS, DMRG = patch.DMRG, global_net_position_init = patch.net_position  # AMPL parameters : DENS, DMRG and global_net_position_init
			DENS = patch.DENS, D_available_energy = patch.D_available_energy, global_net_position_init = patch.net_position  # AMPL parameters : DENS, DMRG and global_net_position_init
		)
	]

	# Short work on sets before handing them to python
	# Just passing the data needed
	zones = zones_id[, zone.id]
	if (core_ahc) {
		# IF core then cbs are not linked to zones
		cbs = cb_id[, cb.id]
	}
	else {
		# if NOT core (CWE) then cbs are linked to zones
		cbs = cb_id[, c("zone.id", "cb.id")]
	}
	countries = merge(zones_id, merge(countries_id,
									  ptdf_data,
									  by="ptdf.country"), by="ptdf.zone")[, c("zone.id", "country.id")]

	# Time-step by time-step resolution
	if (core_ahc){
	  output = patch[,
	                 .single_time_step(patch.mcYear, patch.Date, patch.timeId, 
	                                   zones, cbs, countries, .SD, ptdf,
	                                   rbind(
	                                     capacity_FB[  # For each CB, selects the right capacity depending on typical day and hour
	                                       capacity.typical_day ==
	                                         ts_FB_data[
	                                           ts.mcYear == ((patch.mcYear - 1) %% length(unique(ts_FB_data$ts.mcYear))) + 1 & ts.Date == patch.Date,
	                                           ts.typical_day
	                                         ]
	                                       
	                                       & capacity.Id_hour == (patch.timeId - 1) %% 24 + 1,
	                                       .(
	                                         cb.id,
	                                         capacity
	                                       )
	                                     ],
	                                     capacity_NTC[
	                                       capacity.timeId == patch.timeId,
	                                       .(
	                                         cb.id,
	                                         capacity
	                                       )
	                                     ]
	                                   ), core_ahc, log_detail
	                 ),
	                 by=.(patch.mcYear, patch.Date, patch.timeId)
	  ]
	} else {
	  output = patch[,
	                 .single_time_step(patch.mcYear, patch.Date, patch.timeId, 
	                                   zones, cbs, countries, .SD, ptdf,
	                                   rbind(
	                                     capacity_FB[  # For each CB, selects the right capacity depending on typical day and hour
	                                       capacity.typical_day ==
	                                         ts_FB_data[
	                                           ts.mcYear == ((patch.mcYear - 1) %% length(unique(ts_FB_data$ts.mcYear))) + 1 & ts.Date == patch.Date,
	                                           ts.typical_day
	                                         ]
	                                       
	                                       & capacity.Id_hour == (patch.timeId - 1) %% 24 + 1,
	                                       .(
	                                         zone.id, cb.id,
	                                         capacity
	                                       )
	                                     ],
	                                     capacity_NTC[
	                                       capacity.timeId == patch.timeId,
	                                       .(
	                                         zone.id, cb.id,
	                                         capacity
	                                       )
	                                     ]
	                                   ), core_ahc, log_detail
	                 ),
	                 by=.(patch.mcYear, patch.Date, patch.timeId)
	  ]
	}


	output = merge(countries_id, output, by.x="country.id", by.y="index0")[
		,
		c(
			"patch.area",
			"post_patch.MRG",
			"post_patch.ENS",
			"post_patch.net_position",


			"ptdf.country",
			"MRG.country.",
			"ENS.country.",
			"global_net_position.country.",
			"country.id"
		) := .(
			ptdf.country,
			round(MRG.country., digits=0),
			round(ENS.country., digits=0),
			round(global_net_position.country., digits=0),


			NULL,
			NULL,
			NULL,
			NULL,
			NULL
		)
	][
		,
		min_MRG_ENS := pmin(post_patch.MRG, post_patch.ENS)
	][
		,
		c(
			"post_patch.MRG",
			"post_patch.ENS",

			"min_MRG_ENS"
		) := .(
			post_patch.MRG - min_MRG_ENS,
			post_patch.ENS - min_MRG_ENS,

			NULL
		)
	]
	data.table::setnames(
		output,
		function(colnames) {
			newnames = c()
			for (colname in colnames) {
				if (!(colname %in% c("patch.mcYear", "patch.Date", "patch.timeId", "patch.area", "post_patch.MRG", "post_patch.ENS", "post_patch.net_position")))
					newnames = c(newnames, zones_id[zone.id == colname, ptdf.zone])
				else
					newnames = c(newnames, colname)
			}
		newnames
		}
	)
	output <<- merge(patch_data, output, by=c("patch.mcYear", "patch.timeId", "patch.Date", "patch.area"))
	merge(patch_data, output, by=c("patch.mcYear", "patch.timeId", "patch.Date", "patch.area"))
}


#' Calls the Python Adequacy patch at each time-step
#'
#' @param mcYear (numeric) Monte Carlo year, for logging purposes
#' @param Date (character) Date, for logging purposes
#' @param timeId (numeric) timeId, for logging purposes
#' @param zones (list) zones as a list of integers
#' @param cbs (data.table) critical branches, a data.table containing a column for the zone and a column for the cb id
#' @param countries (data.table) countries, a data.table containing a column for the zone and a column for the country id
#' @param grouped_patch (data.table) The patch data.table grouped by mcYear, Date & timeId
#' @param ptdf (data.table) the ptdf coefficients of the network
#' @param capacity (data.table) Limit capacity on each CB, containing both flow-based
#'	and NTC data
#' @param core_ahc (boolean) TRUE for using CORE area, FALSE for using CWE area
#' @param log_detail (boolean) TRUE for more details
#'
#' @return (data.table) Table giving the MRG, ENS and net-position for each country
#'	at this time-step
#' @export
.single_time_step = function(mcYear, Date, timeId, zones, cbs, countries,
                             grouped_patch, ptdf, capacity, core_ahc, log_detail) {
  # Setting up python script for or-tools use
  library(reticulate)
  source_python(system.file("python", "adq_patch_python.py", package = "AdequacyPatch"))
  python_output = solve_single_time_step(mcYear, Date, timeId, zones, cbs, countries, grouped_patch, ptdf, capacity,
										 core_ahc, log_detail)
  if (is.null(python_output)){
    # In case the solver didn't manage to reach a solution
    return(NULL)
  }
  # Turning the python pandas dataframes into R data.frames
  areas = py_to_r(python_output$areas)
  # Replacing pandas' NaNs by R NAs
  areas[areas =="NaN"] <- NA_real_
  links = py_to_r(python_output$links)
  links[links =="NaN"] <- NA_real_

  links = data.table::dcast(data.table::setDT(links), index1 ~ index0, value.var="net_position.zone.country.")
  merge(areas, links, by.x="index0", by.y="index1")
}
