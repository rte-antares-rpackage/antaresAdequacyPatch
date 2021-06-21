from ortools.linear_solver import pywraplp
import logging
import pandas as pd
import copy

logging.basicConfig(format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger('adq_patch_python')
logger.setLevel(logging.INFO)

def solve_single_time_step(zones_id, cb_id, country_id, grouped_patch, ptdf, capacity):
  # TODO: find a way to make a base problem to copy for each timestep
  # would be more efficient
  
  ### Short work on the dataframes
  # changing the index of patch for easier access
  grouped_patch = grouped_patch.set_index('country.id')
  # changing the index of ptdf for easier access
  ptdf = ptdf.set_index(['zone.id', 'cb.id', 'country.id'])
  # changing the index of capacity for easier access
  capacity = capacity.set_index(['zone.id', 'cb.id'])
  
  ### Solver Instantiation
  problem = pywraplp.Solver.CreateSolver('GLOP')
  
  ### Options
  solver_options = pywraplp.MPSolverParameters()
  solver_options.SetIntegerParam(pywraplp.MPSolverParameters.PRESOLVE, pywraplp.MPSolverParameters.PRESOLVE_ON)
  solver_options.SetIntegerParam(pywraplp.MPSolverParameters.LP_ALGORITHM, pywraplp.MPSolverParameters.DUAL)
  problem.SetNumThreads(1)
  
  ### Sets
  # FLow based zones (or NTC borders, modelled by trivial FB)
  FB_zones = zones_id
  
  # Countries for each FB zone (if NTC border, only 2 countries)
  countries = country_id.groupby('zone.id')['country.id'].apply(set).to_dict() # set to avoid duplicate
  all_countries = set().union(*countries.values())
  
  # Critical branches in each zone
  CB = cb_id.groupby('zone.id')['cb.id'].apply(set).to_dict() # set to avoid duplicate
  
  ### Parameters Check
  # capacity:
  capacity['capacity'] = capacity['capacity'].apply(lambda c: 0 if (c < 0) else c)
  # DENS:
  grouped_patch["DENS"] = grouped_patch["DENS"].apply(lambda dens: 0 if (dens < 0) else dens)
  # LOLD Definition
  LOLD = {country: (1 if grouped_patch.loc[country, "DENS"] > 0 else 0) for country in all_countries}
  all_countries_LOLD = [country for country in all_countries if LOLD[country] == 1]

  ### Variables
  # Net position (exports - imports) for each country in each zone
  net_position = {zone: {country: problem.NumVar(-problem.infinity(),
                                                 problem.infinity(),
                                                 "net_position_{}_{}".format(zone, country))
                          for country in countries[zone]}
                   for zone in FB_zones}

  # Global net position for each country
  global_net_position = {country: problem.NumVar(-problem.infinity(),
                                                 problem.infinity(),
                                                 "global_net_position_{}".format(country))
                         for country in all_countries}
  # Global net position = sum of net positions
  for country in all_countries:
    problem.Add(global_net_position[country] == sum(net_position[zone][country]
                for zone in FB_zones if country in countries[zone]),
                name="global_net_position_definition_{}".format(country))

  # Total post-patch energy not served in each country
  ENS = {country: problem.NumVar(0.0, problem.infinity(), "ENS_{}".format(country))
         for country in all_countries}
  # ENS <= DENS
  for country in all_countries:
    problem.Add(ENS[country] <= grouped_patch.loc[country, "DENS"],
                name="ENS_definition_{}".format(country))

  # Total post-patch margin in each country (DTG MRG)
  MRG = {country: problem.NumVar(0.0, problem.infinity(), "MRG_{}".format(country))
         for country in all_countries}

  # Total post-patch algebraic energy in each country
  available_energy = {country: problem.NumVar(-problem.infinity(),
                                              problem.infinity(),
                                              "available_energy_{}".format(country))
                      for country in all_countries}
  # available energy = D_available_energy - global_net_position
  for country in all_countries:
    problem.Add(available_energy[country] == grouped_patch.loc[country, "D_available_energy"] - global_net_position[country],
                name="available_energy_definition_{}".format(country))

  # Curtailment ratios, proxied by ENS and DENS (instead of PTOs)
  curtailment_ratio = {country: problem.NumVar(-problem.infinity(),
                                               problem.infinity(),
                                               "curtailment_ratio_{}".format(country))
                      for country in all_countries_LOLD}
  # curtailment_ratio = ENS / DENS
  for country in all_countries_LOLD:
    problem.Add(curtailment_ratio[country] == ENS[country] / grouped_patch.loc[country, "DENS"],
                name="curtailment_ratio_definition_{}".format(country))

  ### Constraints
  # Compute ENS and MRG from available energy for each country
  for country in all_countries_LOLD:
    problem.Add(ENS[country] >= -available_energy[country],
                name="compute_ENS_lol_{}".format(country))
  for country in [c for c in all_countries if c not in all_countries_LOLD]: # (all_countries_no_LOLD)
    problem.Add(ENS[country] == 0,
                name="compute_ENS_not_lol_{}".format(country))


  for country in all_countries:
    problem.Add(MRG[country] == available_energy[country] + ENS[country],
                name="compute_MRG_{}".format(country))

  # In each zone, everything exported from somewhere must be imported elsewhere
  for zone in FB_zones:
    problem.Add(sum(net_position[zone][country] for country in countries[zone]) == 0,
                name="energy_conservation_{}".format(zone))

  # No overload on a critical branch in each zone
  for zone in FB_zones:
    for cb in CB[zone]:
      # making sure ptdf defaults to 0
      problem.Add(sum((ptdf.loc[(zone, cb, country)].item() if ptdf.index.isin([(zone, cb, country)]).any() else 0)\
                      * net_position[zone][country] for country in countries[zone])\
                  <= capacity.loc[(zone, cb)].item() + 1,
                  name="flow_based_{}_{}".format(zone, cb))

  # Don't export if in LOL
  for country in all_countries_LOLD:
    problem.Add(global_net_position[country] <= 0,
                name="local_matching_{}".format(country))

  ### Linearization part
  # Additional variables
  y_1 = {country: problem.NumVar(-problem.infinity(), problem.infinity(), "y1_{}".format(country))
         for country in all_countries_LOLD}
  y_2 = {country: problem.NumVar(-problem.infinity(), problem.infinity(), "y2_{}".format(country))
         for country in all_countries}
  w_1 = {country: problem.NumVar(0.0, problem.infinity(), "w1_{}".format(country))
         for country in all_countries_LOLD}
  w_2 = {country: problem.NumVar(0.0, problem.infinity(), "w2_{}".format(country))
         for country in all_countries}

  # Additional constraints
  # ctr_1
  for country in all_countries_LOLD:
    problem.Add(y_1[country] == curtailment_ratio[country],
                name="ctr_1_{}".format(country))
  # ctr_2
  for country in all_countries:
    problem.Add(y_2[country] == (global_net_position[country] - grouped_patch.loc[country, "global_net_position_init"]),
                name="ctr_2_{}".format(country))

  EPS_LIN = 1e-5

  ### Objective
  # Minimize curtailment while maintaining relatively close curtailment ratios
  problem.Minimize(100000 * sum(grouped_patch.loc[country, "DENS"] * w_1[country] for country in all_countries_LOLD)
                   + sum(w_2[country] for country in all_countries))

  # Solving
  status = problem.Solve(solver_options)
  iteration = 1
  if status != pywraplp.Solver.OPTIMAL:
    raise Exception("problem on the first iteration, is data ok ? status: ", status)

  # Additional parameters
  W_1, Y_1, W_2, Y_2, ERROR_1, ERROR_2, MAX_ERROR_1, MAX_ERROR_2 =\
      update_parameters(w_1, y_1, w_2, y_2, iteration, all_countries_LOLD, all_countries)

  # Additional Sets
  LP_1 = {}
  LP_2 = {}

  # TODO: Exit with error if not optimal solve
  # TODO: add timeout just in case
  while (MAX_ERROR_1 > EPS_LIN or MAX_ERROR_2 > EPS_LIN) and (iteration < 1000):
    # Update Sets
    for country in all_countries_LOLD:
      if ERROR_1[country] >= EPS_LIN:
        LP_1[country] = Y_1[country]
    for country in all_countries:
      if ERROR_2[country] >= EPS_LIN:
        LP_2[country] = Y_2[country]

    # Add constraints
    # f(y) >= f(y0)+f'(y0)*(y-y0)
    # y²   >= y0² + 2*y0*(y-y0) = -x0²+2*x0*x
    # lin_1
    for country in LP_1.keys():
      y0 = LP_1[country]
      problem.Add(w_1[country] >= -y0 ** 2 + 2 * y0 * y_1[country],
                  name="lin_1_{}".format(country))
    # lin_2
    for country in LP_2.keys():
      y0 = LP_2[country]
      problem.Add(w_2[country] >= -y0 ** 2 + 2 * y0 * y_2[country],
                  name="lin_2_{}".format(country))

    # Solve
    status = problem.Solve(solver_options)
    iteration += 1
    if status != pywraplp.Solver.OPTIMAL:
      raise Exception("problem not solved properly on iteration {} with status {}".format(iteration, status))

    # Update Parameters
    W_1, Y_1, W_2, Y_2, ERROR_1, ERROR_2, MAX_ERROR_1, MAX_ERROR_2 =\
      update_parameters(w_1, y_1, w_2, y_2, iteration, all_countries_LOLD, all_countries)

  # gather outputs
  areas = {"ENS": {country: ENS[country].solution_value() for country in all_countries},
           "MRG": {country: MRG[country].solution_value() for country in all_countries},
           "global_net_position": {country: global_net_position[country].solution_value() for country in all_countries}}
  areas = pd.DataFrame.from_dict(areas)
  areas.index.name = "country"
  # logger.info(countries)
  links_values = {(zone, country): net_position[zone][country].solution_value()
                  for zone in FB_zones for country in countries[zone]}
  links_index = pd.MultiIndex.from_tuples(links_values.keys(), names=("zone", "country"))
  links = pd.DataFrame({"net_position.zone.country.": pd.Series(links_values, index=links_index)})
  output = links.join(areas)
  return output

def update_parameters(w_1, y_1, w_2, y_2, iteration, all_countries_LOLD, all_countries):
  W_1 = {country: w_1[country].solution_value() for country in all_countries_LOLD}
  Y_1 = {country: y_1[country].solution_value() for country in all_countries_LOLD}

  W_2 = {country: w_2[country].solution_value() for country in all_countries}
  Y_2 = {country: y_2[country].solution_value() for country in all_countries}

  ERROR_1 = {country: abs(W_1[country] - Y_1[country]**2) for country in all_countries_LOLD}
  ERROR_2 = {country: abs(W_2[country] - Y_2[country]**2) for country in all_countries}

  MAX_ERROR_1 = max(ERROR_1.values())
  MAX_ERROR_2 = max(ERROR_2.values())

  logger.info('iteration {} - MAX_ERROR_1 {} - MAX_ERROR_2 {}'.format(iteration, MAX_ERROR_1, MAX_ERROR_2))

  return W_1, Y_1, W_2, Y_2, ERROR_1, ERROR_2, MAX_ERROR_1, MAX_ERROR_2
