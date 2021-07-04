#' Extracts the Power Transmission and Distribution Flows on each CB for each
#' country for the flow-based domain of a study.
#' It also converts the initial PTDFs, given for each link in PTDFs fiven for
#' each country.
#'
#' @param sim_opts (list) Simulation options as given by antaresRead::setSimulationPath
#'
#' @return (data.table) Table containing the PTDFs of each country for each
#' critical branch
#' @export
#'
#' @examples
#' \dontrun{
#' sim_opts = antaresRead::setSimulationPath("path/to/my/simulation")
#'
#' ptdf_FB_data = extract_FB_ptdf(sim_opts=sim_opts)
#' }
extract_FB_ptdf = function(sim_opts=antaresRead::simOptions(), patch_alegro = T) {
	ptdf = data.table::fread(
		paste(sim_opts$studyPath, "user/flowbased/weight.txt", sep="/")
	)

	# Pattern : "FROM.TO"
	links = names(ptdf)[0:-1]

	# Incidence matrix :
	#	conversion[link, country] = 1 if link = (country, *)
	#	conversion[link, country] = -1 if link = (*, country)
	#	conversion[link, country] = 0 otherwise
	conversion = data.table::data.table(links)
	for (link in links) {

		from_to = strsplit(link, "\\.")[[1]]

		from = tolower(from_to[[1]])
		to = tolower(from_to[[2]])

		conversion[links == link, c(from, to) := .(1, -1)]
	}
	data.table::setnafill(conversion, fill=0, cols=(2:ncol(conversion)))

	# Adds a dummy row to avoid infinite solutions
	# (Last country will have a PTDF of 0 on each CB)
	last_row = conversion[1]
	last_row[, c(1, ncol(conversion)) := list("dummy", 1)]
	last_row[, (2:(ncol(conversion) - 1)) := 0]

	conversion = rbind(conversion, last_row)

	if(patch_alegro){
	  conversion[, alegro2 := NULL]
	}
	
	# Adds a dummy column to match conversion and impose the PTDF of the last country
	ptdf[, dummy := 0]

	# Keeps only numeric columns and casts to matrix for solving
	keep_cols = 2:(ncol(conversion))
	A = as.matrix(conversion[, ..keep_cols])


	ptdf_FB_data = ptdf[
		,
		data.table::setDT(as.data.frame(  # Converts result into data.table

			# Solves equation Incidence * PTDF.country = PTDF.links
			# with unknown PTDF.country using qr decomposition
			# (as Incidence is rectangular)

			qr.solve(
				A, as.matrix(.SD)[1,],
			)

		), keep.rownames="country"),
		by=Name
	][
		,
		.(
			zone = "CWE",
			CB = Name, country,
			PTDF = as.numeric(sapply(
				`qr.solve(A, as.matrix(.SD)[1, ], )`,
				round,
				digits=3
			))
		)
	]
	data.table::setnames(ptdf_FB_data, function(colname){paste("ptdf", colname, sep=".")})

	ptdf_FB_data
}
