extract_ptdf_core <- function(sim_opts=antaresRead::simOptions()){
  
  ptdf = data.table::fread(
    paste(sim_opts$studyPath, "user/flowbased/weight.txt", sep="/")
  )
  
  ptdf_flat <- melt(ptdf, id.vars = "Name", variable.name = "link", value.name = "PTDF")
  
  ptdf_flat[, c("from", "to") := tstrsplit(link, "\\.")]
  ptdf_flat[, `:=`(country = tolower(from), zone = tolower(to))]
  ptdf_flat[zone == "zz_flowbased", zone := "CWE"]
  ptdf_flat[, `:=`(link = NULL, from = NULL, to = NULL)]
  setnames(ptdf_flat, "Name", "CB")
  setnames(ptdf_flat, function(colname){paste("ptdf", colname, sep=".")})
  
  ptdf_flat
  
}
