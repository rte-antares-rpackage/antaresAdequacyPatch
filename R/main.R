# To use data.table:::`[.data.table`
.datatable.aware = TRUE


#' Applies the Adequacy Patch on a given simulation
#'
#' The Adequacy patch is a post-processing phase on an Antares study simulation,
#' applying the local-matching and curtailment sharing rules as defined by the
#' EUPHEMIA to correct situations with at least one country in loss of load.
#'
#' @param sim_opts (string) Simulation options, as returned by antaresRead::setSimulationPath
#' @param areas (string or vector of strings) what areas the patch should
#' be applied on. Default: ""
#' @param virtual_areas (string or vector of strings) Virtual areas of the study,
#' excluded from the patch. Default: NULL
#' @param mcYears (numeric or vector of numeric) The Monte-Carlo years to
#' extract from. The special value "all" extracts all Monte-Carlo Years.
#' Default: "all"
#' @param ptdf_FB_data ptdf_FB_data
#' @param capacity_FB_data capacity_FB_data
#' @param ts_FB_data ts_FB_data
#'
#' @return (data.table) Table giving the MRG, ENS and net-position for each country
#'	at each time-step
#'
#' @export
apply_adq_patch = function(sim_opts=antaresRead::simOptions(),
                           areas="all",
                           virtual_areas=NULL,
                           mcYears="all",
                           ptdf_FB_data = NULL,
                           capacity_FB_data = NULL,
                           ts_FB_data = NULL,
                           core_ahc) {
  
  assign("current_mcYear_profiling", mcYears, envir = .GlobalEnv)
  
  areas_fb = union(areas, union(unique(ptdf_FB_data$ptdf.country), unique(ptdf_FB_data$ptdf.to)))
  ptdf_FB_data[, ptdf.to := NULL]
  
  # cat("Import NTC \n")
  links_NTC_data = extract_NTC_links(areas=areas_fb, sim_opts=opts, mcYears=mcYears)
  
  patch_data <- extract_patch(areas = areas_fb,
                              virtual_areas = virtual_areas,
                              sim_opts = sim_opts,
                              mcYears = mcYears)
  
  # patch_data = patch_data
  # ts_FB_data = ts_FB_data
  # capacity_FB_data = capacity_FB_data
  # capacity_NTC_data = links_NTC_data$capacity
  # ptdf_FB_data = ptdf_FB_data
  # ptdf_NTC_data = links_NTC_data$ptdf
  
  if(nrow(patch_data)>0){
    
    if(core_ahc){
      
      out <- adq_patch_core(
        areas = areas,
        patch_data = patch_data,
        ts_FB_data = ts_FB_data,
        capacity_FB_data = capacity_FB_data,
        capacity_NTC_data = links_NTC_data$capacity,
        ptdf_FB_data = ptdf_FB_data,
        ptdf_NTC_data = links_NTC_data$ptdf,
        sim_opts = sim_opts
      )
      
    }else {
      
      out <- adq_patch(
        patch_data = patch_data,
        ts_FB_data = ts_FB_data,
        capacity_FB_data = capacity_FB_data,
        capacity_NTC_data = links_NTC_data$capacity,
        ptdf_FB_data = ptdf_FB_data,
        ptdf_NTC_data = links_NTC_data$ptdf,
        sim_opts = sim_opts
      )
      
    }
    
    
    
    
  }else{
    return(NULL)
  }
  out
  
}




#' Applies the Adequacy Patch on a study
#'
#'
#' @param opts Simulation options, as returned by antaresRead::setSimulationPath
#' @param areas (string or vector of strings) what areas the patch should
#' be applied on. Default: ""
#' @param virtual_areas (string or vector of strings) Virtual areas of the study,
#' excluded from the patch. Default: NULL
#' @param mcYears (numeric or vector of numeric) The Monte-Carlo years to
#' extract from. The special value "all" extracts all Monte-Carlo Years.
#' Default: "all"
#' @param ext name extand for output study.
#' @param nbcl numeric, number of process in cluster
#' @param antaresfbzone antares names of flowbased zone
#' @param showProgress show progress
#' @param thresholdFilter filtering to important modification
#'
#'
#' @export
#'
#' @import doParallel plyr antaresEditObject fs antaresRead pipeR logger data.table
#'
#' @examples
#' \dontrun{
#' opts <- setSimulationPath("path", 4)
#'
#' areas = c("fr", "lu", "de", "cz", "pl", "ch", "at", "itn", "nl", "be", "es", "non", "se1", "model_description_fb_adq")
#' virtual_areas = getAreas(select = "_", regexpSelect = TRUE, exclude = c("model_description_fb", "x_open_turb", "x_open_pump"), regexpExclude = FALSE)
#' run_adq(opts, areas, virtual_areas, 1)
#' }
#'
#' @export
run_adq <- function(opts, areas,
                    virtual_areas,
                    mcYears,
                    ext = NULL,
                    nbcl = 10,
                    antaresfbzone = "model_description_fb",
                    showProgress = TRUE,
                    thresholdFilter = 1000000,
                    core_ahc = F,
                    calculate_mc_all = TRUE){
  
  logger <- layout_glue_generator(format = '[{format(time, \"%Y-%m-%d %H:%M:%S\")}][adq][{level}] {msg}')
  log_layout(logger)
  log_info("ADEQUACY PATCH Starting Treatment")
  
  startTime <- Sys.time()
  
  if(!is.null(ext)){
    if(is.na(ext)){
      ext <- NULL
    }
  }
  # progress <- 0
  # pbp <- txtProgressBar()
  # setTxtProgressBar(pbp, progress, title = 'toto', label = 'titi')
  
  if(showProgress)cat("Verify function inputs\n")
  
  if(!dir.exists(file.path(opts$studyPath, "user"))){
    stop("You wust run run_adq on a flow-based antares studie, folder user missing")
  }
  
  if(!antaresfbzone%in%getAreas(opts = opts)){
    stop("antaresfbzone not in areas")
  }
  if(length(mcYears)==1){
    if(mcYears == "all" | mcYears == "All"){
      mcYears <- opts$mcYears
    }
    
  }
  
  if(!is.null(ext)){
    
    if(showProgress)cat(sprintf("Copy study from %s to %s \n", opts$simPath, paste0(opts$simPath, ext)))
    opts <- antaresEditObject::copyOutput(opts, ext)
  }
  
  
  # cat("Import NTC \n")
  # links_NTC_data = extract_NTC_links(areas=areas, sim_opts=opts)
  log_info("Import PTDF")
  if(core_ahc){
    ptdf_FB_data = extract_ptdf_core(sim_opts=opts)
  } else {
    ptdf_FB_data = extract_FB_ptdf(sim_opts=opts)
  }
  areas_fb = union(areas, union(unique(ptdf_FB_data$ptdf.country), unique(ptdf_FB_data$ptdf.to)))
  log_info("End of import ptdf")
  
  log_info("Import FB capacity")
  capacity_FB_data = extract_FB_capacity(sim_opts=opts)
  log_info("End of FB capacity import")
  
  log_info("Import FB time series")
  ts_FB_data = extract_FB_ts(sim_opts=opts)
  log_info("End of FB ts")
  
  log_info("Compute ADQ")
  
  if(nbcl>1){
    parallel <- TRUE
  }else{
    parallel <- FALSE
  }
  
  
  if(parallel){
    cl <- makeCluster(nbcl)
    clusterExport(cl, c("ptdf_FB_data", "capacity_FB_data", "ts_FB_data",
                        "areas", "opts", "virtual_areas", "antaresfbzone", "core_ahc", "thresholdFilter"), envir = environment())
    
    clusterEvalQ(cl, {
      library(antaresRead)
      library(AdequacyPatch)
      library(antaresEditObject)
      library(data.table)
      library(fs)
      
      opts <- setSimulationPath(opts$simPath)
    })
    registerDoParallel(cl)
    
  }
  log_info("Start write study by mc year")
  llply(mcYears,
        .fun = function(mcYear){
          adq_write(sim_opts = opts,
                    areas = areas,
                    virtual_areas = virtual_areas,
                    ptdf_FB_data = ptdf_FB_data,
                    capacity_FB_data = capacity_FB_data,
                    ts_FB_data = ts_FB_data,
                    mcYears = mcYear,
                    antaresfbzone = antaresfbzone,
                    thresholdFilter = thresholdFilter,
                    core_ahc = core_ahc)
          
        },
        .parallel = parallel,
        .paropts = list(.options.snow = list(preschedule = TRUE)),
        .progress = "text"
  )
  log_info(paste0(length(mcYears), " years in adqp with llply"))
  
  
  if(parallel){
    stopCluster(cl)
  }
  
  log_info("prepare to recalculate only selected nodes/links")
  # prepare to recalculate only selected nodes/links
  selected <- list(areas = areas, links = getLinks(areas_fb, internalOnly = T))
  
  filteringData <- getGeographicTrimming(areas_fb)
  
  areasFiltering <- rbindlist(filteringData$areas, idcol = T)
  linksFiltering <- rbindlist(filteringData$links, idcol = T)
  
  mc_all_timesteps <- unique(areasFiltering$`filter-synthesis`)
  if (length(mc_all_timesteps) > 1){
    areasFiltering <- sapply(unique(areasFiltering$`filter-synthesis`), 
                             function(x){areasFiltering[`filter-synthesis` == x]$.id}, USE.NAMES = T)
  } else {
    areasFiltering <- list(x = areasFiltering$.id)
    names(areasFiltering) <- mc_all_timesteps
  }
  areasFiltering$hourly <- NULL
  log_info("prepare aggreg filtering (profiling)")
  
  log_info("compute other timesteps mc-ind areas")
  ##compute other timesteps mc-ind areas
  for (elmt in names(areasFiltering)){
    ars <- areasFiltering[[elmt]]
    elmt <- unlist(strsplit(elmt, ", "))
    elmt <- elmt[!elmt %in% "hourly"]
    computeOtherFromHourlyMulti(areas = ars, opts = opts, nbcl = nbcl, 
                                type = c('areas'), writeOutput = T, timeStep = elmt)
  }
  log_info("mc-ind aggreg areas (profiling)")
  
  log_info("compute other timesteps mc-ind links")
  ##compute other timesteps mc-ind links
  linksFiltering <- unique(unlist(strsplit(unique(linksFiltering$`filter-synthesis`),", ")))
  linksFiltering <- linksFiltering[!linksFiltering %in% "hourly"]
  computeOtherFromHourlyMulti(areas = areas_fb, opts = opts, nbcl = nbcl, type = c('links'), 
                                writeOutput = T, timeStep = linksFiltering)
  log_info("mc-ind aggreg links (profiling)")
  
  log_info("Write mc all")
  ##Write mc all
  if(T || calculate_mc_all){
    cat("Write mc all")
    parAggregateMCall(opts, nbcl, filtering = T, selected = selected)
  }
  log_info("mc-all aggreg (profiling)")
  
  endTime <- Sys.time() - startTime
  log_info(paste0("Treatment adq total duration: ", round(endTime,2)))
  log_info("ADEQUACY PATCH End of Treatment")
}



#' Applies the Adequacy and write study by mc year
#'
#'
#' @param sim_opts Simulation options, as returned by antaresRead::setSimulationPath
#' @param areas (string or vector of strings) what areas the patch should
#' be applied on. Default: ""
#' @param virtual_areas (string or vector of strings) Virtual areas of the study,
#' excluded from the patch. Default: NULL
#' @param mcYears (numeric or vector of numeric) The Monte-Carlo years to
#' extract from. The special value "all" extracts all Monte-Carlo Years.
#' Default: "all"
#' @param ptdf_FB_data ptdf
#' @param capacity_FB_data capa
#' @param ts_FB_data ts
#' @param antaresfbzone name for new antares area
#'
#' @export
#'
#'
adq_write <- function(sim_opts,
                      areas,
                      virtual_areas,
                      ptdf_FB_data,
                      capacity_FB_data,
                      ts_FB_data,
                      mcYears,
                      antaresfbzone,
                      thresholdFilter,
                      core_ahc){
  
  profiling <- F
  if (mcYears == sim_opts$mcYears[1]){
    profiling <- T
    log_info("Checking time perf. for one iteration :")
  }
  output <- apply_adq_patch(sim_opts = sim_opts,
                            areas = areas,
                            virtual_areas = virtual_areas,
                            ptdf_FB_data = ptdf_FB_data,
                            capacity_FB_data = capacity_FB_data,
                            ts_FB_data = ts_FB_data,
                            mcYears = mcYears,
                            core_ahc = core_ahc)
  if (profiling) log_info("-- apply adq patch (main)")
  
  if(!is.null(output)){

    areas_data <- readAntares(areas = areas, mcYears = mcYears, showProgress = FALSE)
    if (profiling) log_info("readAntares Areas")
    
    output <- .transformOutput(output, antaresfbzone)
    if (profiling) log_info("transform output")
   
    links_data <- readAntares(links = getLinks(areas), mcYears = mcYears, showProgress = FALSE)
    if (profiling) log_info("readAntares Links")
    
    
    ###Filter to important modification
    log_info("Filter to important modification")
    output$areas <- removeAreas(unique(c(output$areas$area, antaresfbzone)), output$areas, links_data, add=TRUE, sim_opts = sim_opts)
    
    ##Filtering
    log_info("Start Filtering")
    tpmerge <- merge(areas_data, output$areas, by = c("area", "mcYear", "timeId"))
    out <- tpmerge[, .(av = sum(`UNSP. ENRG.x`), ap = sum(`UNSP. ENRG.y`)), by = c("mcYear", "timeId")]
    outexclude <- out[av - ap > -thresholdFilter]
    outexclude$av <- outexclude$ap <- NULL
    output$areas <- merge(output$areas, outexclude, by = c("mcYear", "timeId"))
    output$links <- merge(output$links, outexclude, by = c("mcYear", "timeId"))
    if (profiling) log_info("filtering merge")
    log_info("End Filtering")
    ##End filtering
    
    .write_adq_area(sim_opts, areas_data, output, links_data, unique(output$areas$area))
    if (profiling) log_info("write area")

    links_data$from <- NULL
    links_data$to <- NULL
    links_data <- links_data[link%in%unique(output$links$link)]
    .write_adq_link(sim_opts, links_data, output)
    if (profiling) log_info("write link")
    
  }
}
