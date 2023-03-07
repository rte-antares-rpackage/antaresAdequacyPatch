#' Applies the Adequacy patch on given DENS and DMRG for countries and
#' constrained by the flow_based and NTC data.
#'
#' The Adequacy patch is a post-processing phase on an Antares study simulation,
#' applying the local-matching and curtailment sharing rules as defined by the
#' EUPHEMIA to correct situations with at least one country in loss of load.
#'
#' This function does not solve anything itself, it sets up and transfers the
#' relevant data to an AMPL model which then solves it using XPRESS.
#'
#' @param patch_data (data.table) DENS and DMRG for each country at each time-step
#' @param ts_FB_data (data.table) typical day for each day
#' @param capacity_FB_data (data.table) Capacity on each critical branch in the
#'	flow-based domain depeding on the typical day
#' @param capacity_NTC_data (data.table) Maximum transfer capacity of each NTC
#'	border, mimicking capacity_FB_data
#' @param ptdf_FB_data (data.table) PTDF for each country on each critical branch
#' @param ptdf_NTC_data (data.table) Mimics ptdf_FB_data for each border
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
#'	ptdf_FB_data, links_NTC_data$ptdf
#' )
#' }
adq_patch_core = function(areas,
                          patch_data, ts_FB_data,
                          capacity_FB_data, capacity_NTC_data,
                          ptdf_FB_data, ptdf_NTC_data, sim_opts) {
  
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
  
  # Mark countries in the fb domain but out of the patch
  countries_out <- ptdf_data[
    ,
    .(out = sapply(!(ptdf.country %in% areas), as.integer)),
    by=ptdf.country
  ]

  # Create ids for string fields (because of rAMPL setData)
  zones_id = ptdf_data[, .(zone.id = .GRP), by=ptdf.zone]
  cb_id = merge(ptdf_data, zones_id, by = "ptdf.zone")[, .(zone.id = zone.id[[1]], cb.id = .GRP), by=ptdf.CB]
  
  countries_id = ptdf_data[, .(country.id = .GRP), by=ptdf.country]
  
  
  # Sets up AMPL
  ampl = new(rAMPL::AMPL)
  
  ampl$setOption("solver_msg", -1)
  
  ampl$setOption("solver", "amplxpress")
  ampl$setOption("presolve", 1)
  
  ampl$read(system.file("ampl/adq_patch_core.mod", package = "AdequacyPatch"))
  
  # Sends data to AMPL sets
  ampl$getSet("FB_zones")$setValues(zones_id[, zone.id])
  
  ampl$getSet("CB")$setValues(cb_id[, cb.id])
  
  countries_set = ampl$getSet("countries")
  
  countries_per_zone <- unique(merge(zones_id, merge(countries_id,
                                              ptdf_data,
                                              by="ptdf.country"), by="ptdf.zone")[, .(zone.id, country.id)])
  
  for(zone in countries_per_zone$zone.id){
    countries_set$get(zone)$setValues(unique(countries_per_zone[zone.id == zone, country.id]))
  }
  
  # Sends data to AMPL parameters
  
  # Replaces zones, CB and countries names by ids
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
      capacity.mcYear, #V8.2
      capacity.timeId,
      cb.id,  # AMPL sets : FB_zones, CB[zone]
      capacity = capacity.capacity  # AMPL parameter : capacity
    )
  ]
  
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
  
  countries_out = merge(
    countries_id,
    countries_out,
    by=c("ptdf.country")
  )[
    ,
    .(
      country.id,  # AMPL set : all_countries
      out  # AMPL parameter : out
    )
  ]
  
  
  # AMPL data independent from time-step
  ampl$setData(countries_out, 1, "")
  ampl$setData(ptdf, 3, "")
  
  # Time-step by time-step resolution
  output = patch[
    , .single_time_step_core(ampl,
                             .SD,   # For each CB, selects the right capacity depending on typical day and hour
                             rbind(capacity_FB[capacity.typical_day ==
                                                 ts_FB_data[
                                                   ts.mcYear == ((patch.mcYear - 1) %% length(unique(ts_FB_data$ts.mcYear))) + 1 & ts.Date == patch.timeId,
                                                   ts.typical_day
                                                 ]
                                               
                                               & capacity.Id_hour == (patch.timeId - 1) %% 24 + 1,
                                               .(
                                                 cb.id,
                                                 capacity
                                               )],
                                   
                                   capacity_NTC[capacity.timeId == patch.timeId & capacity.mcYear == patch.mcYear, #V8.2
                                                .(
                                                  cb.id,
                                                  capacity
                                                )
                                   ])), 
    by=.(patch.mcYear, patch.Date, patch.timeId)]
  
  ampl$close()
  
  # Replaces countries ids by names
  # output = merge(countries_id, output, by.x="country.id", by.y="index0")[
  # 	,
  # 	.(
  # 		patch.mcYear, patch.timeId, patch.Date, patch.area = ptdf.country,
  # 		post_patch.MRG = round(MRG.country., digits=0),
  # 		post_patch.ENS = round(ENS.country., digits=0),
  # 		post_patch.net_pos = round(net_pos.country., digits=0)
  # 	)
  # ]
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
  output <- merge(patch_data, output, by=c("patch.mcYear", "patch.timeId", "patch.Date", "patch.area"))

  output <- output[
    ,
    delta := min(.SD[
      patch.ENS > 0,
      .(abs(patch.net_position - post_patch.net_position))
    ]),
    by=c("patch.mcYear", "patch.timeId")
  ][
    delta > 5
  ][
    , delta := NULL
  ]
  optim_out <<- copy(output)

  output
}


#' Calls the AMPL Adequacy patch at each time-step
#'
#' @param ampl (rAMPL::AMPL) AMPL object, with model and PTDF already loaded
#' @param patch (data.table) Relevant data form the simulation for this time-step
#' @param capacity (data.table) Limit capacity on each CB, containing both flow-based
#'	and NTC data
#'
#' @return (data.table) Table giving the MRG, ENS and net-position for each country
#'	at this time-step
#' @export
.single_time_step_core = function(ampl, patch, capacity) {
  
  ampl$setData(patch, 1, "")
  ampl$setData(capacity, 1, "") # A MODIFIER ?
  
  ampl$solve()
  
  status <- ampl$getData("solve_result_num")
  
  if(status < 100) {
    areas = ampl$getData("
  		{country in all_countries} (
  			ENS[country],
  			MRG[country],
  			global_net_position[country]
  		)
  	")
      
    links = ampl$getData("
  		{zone in FB_zones, country in countries[zone]} (
  			net_position[zone, country]
  		)
  	")
    links = data.table::dcast(data.table::setDT(links), index1 ~ index0, value.var="net_position.zone.country.")
    
    output = merge(areas, links, by.x="index0", by.y="index1")
  }
  else {
    output = NULL
  }
  output
}
