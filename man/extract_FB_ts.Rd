% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_FB_ts.R
\name{extract_FB_ts}
\alias{extract_FB_ts}
\title{Extracts the flow-based time-series from an Antares study}
\usage{
extract_FB_ts(sim_opts = antaresRead::simOptions())
}
\arguments{
\item{sim_opts}{(list) Simulation options as given by antaresRead::setSimulationPath}
}
\value{
(data.table) Table containing the typical day for each day in each
Monte-Carlo year of the simulation.
}
\description{
Extracts the flow-based time-series from an Antares study
}
\examples{
\dontrun{
sim_opts = antaresRead::setSimulationPath("path/to/my/simulation")

ts_FB_data = extract_FB_ts(sim_opts=sim_opts)
}
}
