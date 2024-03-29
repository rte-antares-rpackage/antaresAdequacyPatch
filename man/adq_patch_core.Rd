% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adq_patch_core.R
\name{adq_patch_core}
\alias{adq_patch_core}
\title{Applies the Adequacy patch on given DENS and DMRG for countries and
constrained by the flow_based and NTC data.}
\usage{
adq_patch_core(
  areas,
  patch_data,
  ts_FB_data,
  capacity_FB_data,
  capacity_NTC_data,
  ptdf_FB_data,
  ptdf_NTC_data,
  sim_opts
)
}
\arguments{
\item{patch_data}{(data.table) DENS and DMRG for each country at each time-step}

\item{ts_FB_data}{(data.table) typical day for each day}

\item{capacity_FB_data}{(data.table) Capacity on each critical branch in the
flow-based domain depeding on the typical day}

\item{capacity_NTC_data}{(data.table) Maximum transfer capacity of each NTC
border, mimicking capacity_FB_data}

\item{ptdf_FB_data}{(data.table) PTDF for each country on each critical branch}

\item{ptdf_NTC_data}{(data.table) Mimics ptdf_FB_data for each border}
}
\value{
(data.table) Table giving the MRG, ENS and net-position for each country
at each time-step
}
\description{
The Adequacy patch is a post-processing phase on an Antares study simulation,
applying the local-matching and curtailment sharing rules as defined by the
EUPHEMIA to correct situations with at least one country in loss of load.
}
\details{
This function does not solve anything itself, it sets up and transfers the
relevant data to an AMPL model which then solves it using XPRESS.
}
\examples{
\dontrun{
sim_opts = antaresRead::setSimulationPath("path/to/my/simulation")
areas = antaresRead::getAreas()

patch_data = extract_patch(areas=areas, mcYears=c(1, 3), sim_opts=sim_opts)
ts_FB_data = extract_FB_ts(sim_opts=sim_opts)
capacity_FB_data = extract_FB_capacity(sim_opts=sim_opts)
ptdf_FB_data = extract_FB_ptdf(sim_opts=sim_opts)
links_NTC_data = extract_NTC_links(areas=areas, sim_opts=sim_opts)

output = adq_patch(
patch_data,
ts_FB_data,
capacity_FB_data, links_NTC_data$capacity,
ptdf_FB_data, links_NTC_data$ptdf
)
}
}
