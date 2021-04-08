
### Sets


###
# from R
###
# FLow based zones (or NTC borders, modelled by trivial FB)
set FB_zones;

# Countries for each FB zone (if NTC border, only 2 countries)
set countries{FB_zones};
set all_countries := union{zone in FB_zones} countries[zone];

# Critical branches in each zone
set CB{FB_zones};


set FB_zones_CB 				:= {z_ in FB_zones, cb_ in CB[z_]};
set FB_zones_countries 		:= {z_ in FB_zones, countries_ in countries[z_]};
set FB_zones_CB_countries 	:= {z_ in FB_zones, cb_ in CB[z_], countries_ in countries[z_]};


# ###
# # from TXT
# ###
# set all_countries;
# set FB_zones_CB 			dimen 2;
# set FB_zones_CB_countries 	dimen 3;

# set FB_zones_countries 		dimen 2 := union{(z_, cb_, c_) in FB_zones_CB_countries}{(z_, c_)};

# set FB_zones := union{(z_, c_) in FB_zones_countries}{z_};

# set countries{z_ in FB_zones} := {(z_, c_) in FB_zones_countries};
# set CB		 {z_ in FB_zones} := {(z_, c_) in FB_zones_CB};


### Parameters
# # Power Transmission and Distribution Flows, for each CB
# param PTDF{zone in FB_zones, CB[zone], countries[zone]} default 0;
# # Capacity of each critical branch
# param capacity{zone in FB_zones, CB[zone]} >= 0 default 0;

param PTDF{FB_zones_CB_countries} default 0;
param capacity{FB_zones_CB} >= 0 default 0;

# Pre-patch values (D = Domestic, without trades)
param DENS{all_countries} >= 0;
# param DMRG{all_countries} >= 0;

param D_available_energy{country in all_countries};# := DMRG[country] - DENS[country];

# Loss of load duration, in hours
# Here, equivalent to boolean telling if the country is in LOL for the considered hour
param LOLD{country in all_countries} := (if DENS[country] > 0 then 1 else 0);

param global_net_position_init{all_countries} default 0;


set all_countries_LOLD := {country in all_countries : LOLD[country] = 1};

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
subject to flow_based{zone in FB_zones, cb in CB[zone]}:
	sum{country in countries[zone]} PTDF[zone, cb, country] * net_position[zone, country] <= capacity[zone, cb] + 1;

# Don't export if in LOL
subject to local_matching{country in all_countries : LOLD[country] == 1}:
	global_net_position[country] <= 0;

# Can't export more than Antares
# subject to de_optimization{country in all_countries : LOLD[country] == 0}:
	# global_net_position[country] <= global_net_position_init[country];

var y_1{country in all_countries_LOLD}; 
var y_2{country in all_countries};
var w_1{country in all_countries_LOLD} >= 0; 
var w_2{country in all_countries} >= 0;


param W_1{country in all_countries_LOLD}default 0; 
param W_2{country in all_countries} 	default 0;
param Y_1{country in all_countries_LOLD}default 0; 
param Y_2{country in all_countries}		default 0;

param ERROR_1{country in all_countries_LOLD} := abs(W_1[country]-Y_1[country]^2);
param ERROR_2{country in all_countries} := abs(W_2[country]-Y_2[country]^2);

param MAX_ERROR_1 := max{country in all_countries_LOLD} ERROR_1[country];
param MAX_ERROR_2 := max{country in all_countries} ERROR_2[country];

param LB_1{country in all_countries_LOLD} default -1;
param UB_1{country in all_countries_LOLD} default -1;

param LB_2{country in all_countries} default -1;
param UB_2{country in all_countries} default -1;

set LP_1 dimen 2 default {};
set LP_2 dimen 2 default {};

subject to ctr_1{country in all_countries_LOLD}:y_1[country] = (curtailment_ratio[country]);
subject to ctr_2{country in all_countries}:		y_2[country] = (global_net_position[country] - global_net_position_init[country]);

# f(y) >= f(y0)+f'(y0)*(y-y0)
# y²   >= y0² + 2*y0*(y-y0) = -x0²+2*x0*x
subject to lin_1{country in all_countries_LOLD, (country, y0) in LP_1}: w_1[country] >= -y0^2 + 2*y0*y_1[country];
subject to lin_2{country in all_countries, (country, y0) in LP_2}: w_2[country] >= -y0^2 + 2*y0*y_2[country];

param EPS_LIN := 1e-5;
### Objective
# Minimize curtailment while maintaining relatively close curtailment ratios
minimize curtailment_sharing_lin:
	+ 100000 *	sum{country in all_countries_LOLD} DENS[country] * w_1[country]
	+ 			sum{country in all_countries} w_2[country];
minimize curtailment_sharing:
	+ 100000 * 	sum{country in all_countries_LOLD} DENS[country] * (curtailment_ratio[country])^2
	+			sum{country in all_countries} (global_net_position[country] - global_net_position_init[country])^2;