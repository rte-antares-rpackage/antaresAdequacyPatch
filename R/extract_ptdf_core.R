extract_ptdf_core <- function(sim_opts=antaresRead::simOptions()){
  require(stringr)
  
  ptdf = data.table::fread(
    paste(sim_opts$studyPath, "user/flowbased/weight.txt", sep="/")
  )
  
  ptdf_flat <- melt(ptdf, id.vars = "Name", variable.name = "link", value.name = "PTDF")
  
  ptdf_flat[, c("from", "to") := tstrsplit(link, "\\.")]
  ptdf_flat[, `:=`(country = tolower(from), zone_raw = tolower(str_replace(link, "\\.", " - ")))]
  
  # Changement de signe du PTDF pour les pays pour lesquels la zone est à l'envers
  ptdf_flat[, zone := sapply(zone_raw, function(X){
    sorted <- sort(strsplit(X, " - ")[[1]])
    paste0(sorted[1], " - ", sorted[2])
  })]
  # ptdf_flat[, zone_changed := (zone != zone_raw)]
  # ptdf_flat[zone_changed == TRUE, PTDF := -PTDF]

  ptdf_flat[grepl("zz_flowbased", zone), zone := "CWE"]
  ptdf_flat <- ptdf_flat[, .SD, .SDcols = c("Name", "PTDF", "country", "zone")]
  setnames(ptdf_flat, "Name", "CB")
  setnames(ptdf_flat, function(colname){paste("ptdf", colname, sep=".")})
  
  ptdf_flat
  
}
