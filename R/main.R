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
#' @param links_NTC_data links_NTC_data
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
                           links_NTC_data = NULL,
                           ptdf_FB_data = NULL,
                           capacity_FB_data = NULL,
                           ts_FB_data = NULL,
                           core_ahc) {
  
  patch_data <- extract_patch(areas = areas,
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
#' @import doParallel plyr antaresEditObject fs antaresRead pipeR
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
  
  
  cat("Import NTC \n")
  links_NTC_data = extract_NTC_links(areas=areas, sim_opts=opts)
  cat("Import PTDF \n")
  if(core_ahc){
    ptdf_FB_data = extract_ptdf_core(sim_opts=opts)
  } else {
    ptdf_FB_data = extract_FB_ptdf(sim_opts=opts)
  }
  cat("Import FB capacity \n")
  capacity_FB_data = extract_FB_capacity(sim_opts=opts)
  cat("Import FB time series \n")
  ts_FB_data = extract_FB_ts(sim_opts=opts)
  
  
  cat("Compute ADQ \n")
  
  if(nbcl>1){
    parallel <- TRUE
  }else{
    parallel <- FALSE
  }
  
  
  if(parallel){
    cl <- makeCluster(nbcl)
    clusterExport(cl, c("ptdf_FB_data", "capacity_FB_data", "ts_FB_data",
                        "areas", "opts", "virtual_areas", "antaresfbzone", "links_NTC_data"), envir = environment())
    
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
  
  llply(mcYears,
        .fun = function(mcYear){
          adq_write(sim_opts = opts,
                    areas = areas,
                    virtual_areas = virtual_areas,
                    links_NTC_data = links_NTC_data,
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
  
  if(parallel){
    stopCluster(cl)
  }
  
  
  computeTimeStampFromHourly(opts, nbcl = nbcl, type = c('areas', 'links'))
  
  ##Write mc all
  
  if(calculate_mc_all == T){
    cat("Write mc all")
    parAggregateMCall(opts, nbcl)
    .add_csv_digest(opts)
  }

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
#' @param links_NTC_data NTC
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
                      links_NTC_data,
                      ptdf_FB_data,
                      capacity_FB_data,
                      ts_FB_data,
                      mcYears,
                      antaresfbzone,
                      thresholdFilter,
                      core_ahc){
  
  output <- apply_adq_patch(sim_opts = sim_opts,
                            areas = areas,
                            virtual_areas = virtual_areas,
                            links_NTC_data = links_NTC_data,
                            ptdf_FB_data = ptdf_FB_data,
                            capacity_FB_data = capacity_FB_data,
                            ts_FB_data = ts_FB_data,
                            mcYears = mcYears,
                            core_ahc = core_ahc)
  
  if(!is.null(output)){

    areas_data <- readAntares(areas = areas, mcYears = mcYears, showProgress = FALSE)
    output <- .transformOutput(output, antaresfbzone)
    links_data <- readAntares(links = getLinks(areas), mcYears = mcYears, showProgress = FALSE)
    
    
    ###Filter to important modification
    
    output$areas <- removeAreas(unique(c(output$areas$area, antaresfbzone)), output$areas, links_data, add=TRUE, sim_opts = sim_opts)
    
    ##Filtering
    tpmerge <- merge(areas_data, output$areas, by = c("area", "mcYear", "timeId"))
    out <- tpmerge[, .(av = sum(`UNSP. ENRG.x`), ap = sum(`UNSP. ENRG.y`)), by = c("mcYear", "timeId")]
    outexclude <- out[av - ap > -thresholdFilter]
    outexclude$av <- outexclude$ap <- NULL
    output$areas <- merge(output$areas, outexclude, by = c("mcYear", "timeId"))
    output$links <- merge(output$links, outexclude, by = c("mcYear", "timeId"))
    ##End filtering
    
    
    .write_adq_area(sim_opts, areas_data, output, links_data, unique(output$areas$area))

    links_data$from <- NULL
    links_data$to <- NULL
    links_data <- links_data[link%in%unique(output$links$link)]
    .write_adq_link(sim_opts, links_data, output)
    
  }
}
