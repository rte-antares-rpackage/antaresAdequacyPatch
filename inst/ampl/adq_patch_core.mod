### Sets
# FLow based zones (or NTC borders, modelled by trivial FB)
set FB_zones;

# Countries for each FB zone (if NTC border, only 2 countries)
set countries{FB_zones};
set all_countries := union{zone in FB_zones} countries[zone];

# Critical branches in each zone
set CB;


### Parameters
# Countries inside the fb zone but outside of the patch
param out{all_countries} binary default 0;

# Power Transmission and Distribution Flows, for each CB
param PTDF{zone in FB_zones, CB, countries[zone]} default 0;
# Capacity of each critical branch
param capacity{CB} >= 0 default 0;

# Pre-patch values (D = Domestic, without trades)
param DENS{all_countries} >= 0;
# param DMRG{all_countries} >= 0;

param D_available_energy{all_countries};# := DMRG[country] - DENS[country];

# Loss of load duration, in hours
# Here, equivalent to boolean telling if the country is in LOL for the considered hour
param LOLD{country in all_countries} := (if DENS[country] > 0 then 1 else 0);

param global_net_position_init{all_countries} default 0;


### Variables
# Net position (exports - imports) for each country in each zone
var net_position{zone in FB_zones, countries[zone]};
# Global net position for each country
var global_net_position{country in all_countries} = sum{zone in FB_zones : country in countries[zone]} net_position[zone, country];

# Total post-patch energy not served in each country
var ENS{country in all_countries} >=0, <= DENS[country];
# Total post-patch margin in each country (DTG MRG)
var MRG{all_countries} >= 0;

# Total post-patch algebraic energy in each country (= MRG - ENS)
var available_energy{country in all_countries} = 
	D_available_energy[country] - global_net_position[country];

# Curtailment ratios, proxied by ENS and DENS (instead of PTOs)
var curtailment_ratio{country in all_countries : LOLD[country] = 1} = ENS[country] / DENS[country];

### Constraints
# Compute ENS and MRG from available energy for each country
subject to compute_ENS_lol{country in all_countries : LOLD[country] = 1}:
	ENS[country] >= -available_energy[country];
subject to compute_ENS_not_lol{country in all_countries : LOLD[country] = 0}:
	ENS[country] = 0;

subject to compute_MRG{country in all_countries}:
	MRG[country] = available_energy[country] + ENS[country];

# In each zone, everything exported from somewhere must be imported elsewhere
subject to energy_conservation{zone in FB_zones}:
	sum{country in countries[zone]} net_position[zone, country] == 0;
	
# No overload on a critical branch in each zone
subject to flow_based{cb in CB}:
	sum{zone in FB_zones, country in countries[zone]} PTDF[zone, cb, country] * net_position[zone, country] <= capacity[cb] + 1;

# Don't export if in LOL
subject to local_matching{country in all_countries : LOLD[country] == 1 and out[country] == 0}:
	global_net_position[country] <= 0;
	
# Countries out shouldn't change
subject to outside_adqp_up{country in all_countries : out[country] == 1}:
  global_net_position[country] <= global_net_position_init[country] + 1;
subject to outside_adqp_down{country in all_countries : out[country] == 1}:
  global_net_position[country] >= global_net_position_init[country] - 1;

# Can't export more than Antares
# subject to de_optimization{country in all_countries : LOLD[country] == 0}:
	# global_net_position[country] <= global_net_position_init[country];


### Objective
# Minimize curtailment while maintaining relatively close curtailment ratios
minimize curtailment_sharing:
	100000 * sum{country in all_countries : LOLD[country] = 1} DENS[country] * (curtailment_ratio[country])^2
	+ sum{country in all_countries} (global_net_position[country] - global_net_position_init[country])^2;
