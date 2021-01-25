
#' @noRd
.complete_link <- function(links, links_new)
{

	keys <- c("mcYear", "timeId", "link")
	setkeyv(links_new, keys)
	setkeyv(links, keys)

	setnames(links, "FLOW LIN.", "FLN")
	setnames(links_new, "FLOW LIN.", "FLN")


	links[links_new, `:=`( FLN = i.FLN)]
	setnames(links, "FLN", "FLOW LIN.")

}

#' @noRd
.write_adq_link <- function(opts, links, output){
	.complete_link(links, output$links)
	antaresEditObject::writeOutputValues(links, opts)
}
