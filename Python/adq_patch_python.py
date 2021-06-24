from ortools.linear_solver import pywraplp
import logging
import pandas as pd
import copy

### Parameters
# logging
logging.basicConfig(format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger('adq_patch_python')
logger.setLevel(logging.WARNING)
# relaxation levels for optimization problem
RELAXATION_ORDER= [0, 1, 10, 100] # MWh
# Linearization precision (used for linearization loop)
EPS_LIN = 1e-5
# Solver Options
PRESOLVE_VALUE = pywraplp.MPSolverParameters.PRESOLVE_ON
DUAL_TOLERANCE_VALUE = 0.1
PRIMAL_TOLERANCE_VALUE = 0.1


def solve_single_time_step(mcYear, Date, timeId, zones_id, cb_id, country_id, grouped_patch, ptdf, capacity):
  """
  Main Python function, formatting input & output, and the linearization loop logic
  :param mcYear: Monte Carlo year as integer, used for logging
  :param Date: Date as string, used for logging
  :param timeId: timeId as integer, used for logging
  :param zones_id: list of all zones ids
  :param cb_id: pandas dataframe with two columns:
    - zone.id: the zone id
    - cb.id: the critical branch id in the zone
  :param country_id: pandas dataframe with two columns:
    - zone.id: the zone id
    - country.id: the country id in the zone
  :param grouped_patch: the patch data as a dataframe, but grouped by mcYear, Date & timeId
  :param ptdf: the ptdf matrix of the network, as a dataframe
  :param capacity: Limit capacity on each CB as a dataframe, containing both flow-based and NTC data
  :return: a dictionary of the optimization result with two pairs (key, value):
    - "areas": a pandas dataframe containing 4 columns: index0 (the country id),
    ENS.country., MRG.country., and global_net_position.country.
    - "links": a pandas dataframe containing 3 columns: index0 (the zone id),
    index1 (the country id), and net_position.zone.country.
  """
  # If showing more logs becomes relevant, pass the logger level to info
  logger.info("solving mcYear {} Date {} timeId {}".format(mcYear, Date, timeId))
  
  ### Short work on the dataframes
  # changing the index of patch for easier access
  grouped_patch = grouped_patch.set_index('country.id')
  # changing the index of ptdf for easier access
  ptdf = ptdf.set_index(['zone.id', 'cb.id', 'country.id'])
  # changing the index of capacity for easier access
  capacity = capacity.set_index(['zone.id', 'cb.id'])
  
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
  
  
  ### Linearization parameter initialization
  linearization_parameters = {
    "LP_1": [],
    "LP_2": [],
    "W_1": {country: 0 for country in all_countries_LOLD},
    "Y_1": {country: 0 for country in all_countries_LOLD},
    "W_2": {country: 0 for country in all_countries},
    "Y_2": {country: 0 for country in all_countries},
    "ERROR_1": {country: 0 for country in all_countries_LOLD},
    "ERROR_2": {country: 0 for country in all_countries},
    "MAX_ERROR_1": 0,
    "MAX_ERROR_2": 0
  }
  # In case of infeasibility
  relaxation_stage = 0
  
  # First Solve
  try:
    problem, relaxation_stage, w_1, y_1, w_2, y_2 = build_and_solve(FB_zones, CB, countries, all_countries, all_countries_LOLD,
                                                                    grouped_patch, ptdf, capacity, linearization_parameters, relaxation_stage)
  except ValueError as e:
    # Catch a failed optimization
    # And warn about it
    logger.warning("FAILED solving mcYear {} Date {} timeId {}".format(mcYear, Date, timeId))
    return {"areas": pd.DataFrame(columns=["ENS.country.", "MRG.country.", "global_net_position.country.", "index0"]),
            "links": pd.DataFrame(columns=["index0", "index1", "net_position.zone.country."])}
  
  iteration = 1
  
  # Update linearization parameters
  linearization_parameters = update_parameters(linearization_parameters, w_1,
                                               y_1, w_2, y_2, iteration,
                                               all_countries, all_countries_LOLD)
  # Linearization loop logic
  while (linearization_parameters["MAX_ERROR_1"] > EPS_LIN \
         or linearization_parameters["MAX_ERROR_2"] > EPS_LIN) \
        and (iteration < 1000):
    try:
      problem, relaxation_stage, w_1, y_1, w_2, y_2 = build_and_solve(FB_zones, CB, countries, all_countries, all_countries_LOLD, grouped_patch, ptdf, capacity, linearization_parameters, relaxation_stage)
    except ValueError as e:
      logger.warning("FAILED solving mcYear {} Date {} timeId {}".format(mcYear, Date, timeId))
      return {"areas": pd.DataFrame(columns=["ENS.country.", "MRG.country.", "global_net_position.country.", "index0"]),
              "links": pd.DataFrame(columns=["index0", "index1", "net_position.zone.country."])}
    iteration += 1

    # Update linearization parameters
    linearization_parameters = update_parameters(linearization_parameters, w_1,
                                                 y_1, w_2, y_2, iteration,
                                                 all_countries, all_countries_LOLD)
  
  # gather outputs
  ENS = {country: problem.LookupVariable("ENS_{}".format(country))
         for country in all_countries}
  MRG = {country: problem.LookupVariable("MRG_{}".format(country))
         for country in all_countries}
  global_net_position = {country: problem.LookupVariable("global_net_position_{}".format(country))
                         for country in all_countries}
  net_position = {zone: {country: problem.LookupVariable("net_position_{}_{}".format(zone, country))
                          for country in countries[zone]}
                   for zone in FB_zones}
  
  ### Building Output
  # changing sets to lists to get a consistent order for the outputs
  all_countries = list(all_countries)
  countries = {key: list(value) for key, value in countries.items()}
  areas = {"ENS.country.": [ENS[country].solution_value() for country in all_countries],
           "MRG.country.": [MRG[country].solution_value() for country in all_countries],
           "global_net_position.country.": [float(global_net_position[country].solution_value()) for country in all_countries],
           "index0": all_countries}
  areas = pd.DataFrame.from_dict(areas)
  links = {"index0": [zone for zone in FB_zones for country in countries[zone]],
           "index1": [country for zone in FB_zones for country in countries[zone]],
           "net_position.zone.country.": [net_position[zone][country].solution_value() for zone in FB_zones for country in countries[zone]]}
  links = pd.DataFrame.from_dict(links)
  return {"areas":areas, "links": links}


def build_and_solve(FB_zones, CB, countries, all_countries, all_countries_LOLD,
                    grouped_patch, ptdf, capacity, linearization_parameters,
                    relaxation_stage):
  """
  Builds a problem and solves it. Is responsible for retries.
  :param FB_zones: list of all zones ids
  :param CB: dictionary of key zone id and value set of critical branches id in this zone
  :param countries: dictionary of key zone id and value set of countries id in this zone
  :param all_countries: set of all countries ids
  :param all_countries_LOLD: list of all countries ids with LOLD
  :param grouped_patch: the patch data as a dataframe, but grouped by mcYear, Date & timeId
  :param ptdf: the ptdf matrix of the network, as a dataframe
  :param capacity: Limit capacity on each CB as a dataframe, containing both flow-based and NTC data
  :param linearization_parameters: a dictionary containing all linearization-related data
  :param relaxation_stage: index of the RELAXATION_ORDER list. Used to keep track of the current needed relaxation status of the problem (in MW in some equations)
  :return:
    - problem, the pywraplp.Solver used to solve the optimization problem (useful to access variables)
    - the variables w_1, y_1, w_2 and y_2, to use for linearization parameters update
  """
  
  ### Options
  solver_options = pywraplp.MPSolverParameters()
  solver_options.SetIntegerParam(pywraplp.MPSolverParameters.PRESOLVE, PRESOLVE_VALUE)
  solver_options.SetIntegerParam(pywraplp.MPSolverParameters.LP_ALGORITHM, pywraplp.MPSolverParameters.DUAL)
  solver_options.SetDoubleParam(pywraplp.MPSolverParameters.DUAL_TOLERANCE, DUAL_TOLERANCE_VALUE)
  solver_options.SetDoubleParam(pywraplp.MPSolverParameters.PRIMAL_TOLERANCE, PRIMAL_TOLERANCE_VALUE)
  
  #### Build the problem
  problem, w_1, y_1, w_2, y_2 = make_problem(FB_zones, CB, countries, all_countries, all_countries_LOLD, grouped_patch, ptdf, capacity, linearization_parameters, RELAXATION_ORDER[relaxation_stage])
  # Set the options and solve
  # One thread (parallelization already done through the R code)
  problem.SetNumThreads(1)
  # time limit of 20 seconds
  problem.SetTimeLimit(20*1000)
  # Solve
  status = problem.Solve(solver_options)
  
  ### Contingency Plan
  while (status != pywraplp.Solver.OPTIMAL) and (relaxation_stage + 1) < len(RELAXATION_ORDER):
    # attempt to lauch a more relaxed problem with a different solving algorithm
    relaxation_stage += 1
    problem, w_1, y_1, w_2, y_2 = make_problem(FB_zones, CB, countries, all_countries, all_countries_LOLD, grouped_patch, ptdf, capacity, linearization_parameters, RELAXATION_ORDER[relaxation_stage])
    problem.SetNumThreads(1)
    problem.SetTimeLimit(20*1000)
    if solver_options.GetIntegerParam(pywraplp.MPSolverParameters.LP_ALGORITHM) != pywraplp.MPSolverParameters.PRIMAL:
      solver_options.SetIntegerParam(pywraplp.MPSolverParameters.LP_ALGORITHM, pywraplp.MPSolverParameters.PRIMAL)
    status = problem.Solve(solver_options)
  
  ### If all fails
  if status != pywraplp.Solver.OPTIMAL:
    if status == pywraplp.Solver.INFEASIBLE:
      logger.warning("Problem is INFEASIBLE despite relaxation attempts")
    elif status == pywraplp.Solver.UNBOUNDED:
      logger.warning("Problem is UNBOUNDED")
    elif status == pywraplp.Solver.ABNORMAL:
      logger.warning("Problem is ABNORMAL despite attempt to change algorithm used")
    elif status == pywraplp.Solver.NOT_SOLVED:
      logger.warning("Problem is NOT_SOLVED")
    else:
      logger.warning("Problem with unknown status {}".format(status))
    # We raise an exception to be caught by solve_single_time_step to return a default output
    raise ValueError("Problem did not get solved properly")
  
  else:
    # Return the results
    return problem, relaxation_stage, w_1, y_1, w_2, y_2
    
    
def make_problem(FB_zones, CB, countries, all_countries, all_countries_LOLD, grouped_patch, ptdf, capacity,
                 linearization_parameters, tolerance = 0):
  """
  Function dedicated to making the optimization problem
  :param FB_zones: list of all zones ids
  :param CB: dictionary of key zone id and value set of critical branches id in this zone
  :param countries: dictionary of key zone id and value set of countries id in this zone
  :param all_countries: set of all countries ids
  :param all_countries_LOLD: list of all countries ids with LOLD
  :param grouped_patch: the patch data as a dataframe, but grouped by mcYear, Date & timeId
  :param ptdf: the ptdf matrix of the network, as a dataframe
  :param capacity: Limit capacity on each CB as a dataframe, containing both flow-based and NTC data
  :param linearization_parameters: a dictionary containing all linearization-related data
  :param tolerance: integer value of the tolerance to be used in case of relaxed problem
  :return:
    - problem, the pywraplp.Solver used to solve the optimization problem (useful to access variables)
    - the variables w_1, y_1, w_2 and y_2, to use for linearization parameters update
  """
  # We are following the structure of the ampl .mod file
  ### Solver Instantiation
  problem = pywraplp.Solver.CreateSolver('GLOP')

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
      problem.Add(available_energy[country] <= grouped_patch.loc[country, "D_available_energy"] - global_net_position[country] + tolerance,
                  name="available_energy_definition_1_{}".format(country))
      problem.Add(available_energy[country] >= grouped_patch.loc[country, "D_available_energy"] - global_net_position[country] - tolerance,
                  name="available_energy_definition_2_{}".format(country))

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
      problem.Add(MRG[country] >= available_energy[country] + ENS[country] - tolerance,
                  name="compute_MRG_1_{}".format(country))
      problem.Add(MRG[country] <= available_energy[country] + ENS[country] + tolerance,
                  name="compute_MRG_2_{}".format(country))

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
  # Add constraints
  # f(y) >= f(y0)+f'(y0)*(y-y0)
  # y²   >= y0² + 2*y0*(y-y0) = -x0²+2*x0*x
  # lin_1
  for i, (country, y0) in enumerate(linearization_parameters["LP_1"]):
    problem.Add(w_1[country] >= -y0 ** 2 + 2 * y0 * y_1[country],
                name="lin_1_{}".format(i))
  # lin_2
  for i, (country, y0) in enumerate(linearization_parameters["LP_2"]):
    problem.Add(w_2[country] >= -y0 ** 2 + 2 * y0 * y_2[country],
                name="lin_2_{}".format(i))

  ### Objective
  # Minimize curtailment while maintaining relatively close curtailment ratios
  problem.Minimize(100000 * sum(grouped_patch.loc[country, "DENS"] * w_1[country] for country in all_countries_LOLD)
                   + sum(w_2[country] for country in all_countries))
  return problem, w_1, y_1, w_2, y_2


def update_parameters(linearization_parameters, w_1, y_1, w_2, y_2, iteration, all_countries, all_countries_LOLD):
  """
  Updates the linearization parameters after each iteration
  :param linearization_parameters: a dictionary containing all linearization-related data
  :param w_1, y_1, w_2, y_2: variables of the problem containing the result value of the previous iteration
  :param iteration: integer value of the iteration number (used for logging only)
  :param all_countries: set of all countries ids
  :param all_countries_LOLD: list of all countries ids with LOLD
  :return: the updated linearization_parameters
  """
  linearization_parameters["W_1"] = {country: w_1[country].solution_value() for country in all_countries_LOLD}
  linearization_parameters["Y_1"] = {country: y_1[country].solution_value() for country in all_countries_LOLD}

  linearization_parameters["W_2"] = {country: w_2[country].solution_value() for country in all_countries}
  linearization_parameters["Y_2"] = {country: y_2[country].solution_value() for country in all_countries}

  linearization_parameters["ERROR_1"] = {country: abs(linearization_parameters["W_1"][country] - linearization_parameters["Y_1"][country]**2) for country in all_countries_LOLD}
  linearization_parameters["ERROR_2"] = {country: abs(linearization_parameters["W_2"][country] - linearization_parameters["Y_2"][country]**2) for country in all_countries}

  linearization_parameters["MAX_ERROR_1"] = max(linearization_parameters["ERROR_1"].values())
  linearization_parameters["MAX_ERROR_2"] = max(linearization_parameters["ERROR_2"].values())
  
  for country in all_countries_LOLD:
    if linearization_parameters["ERROR_1"][country] >= EPS_LIN:
      linearization_parameters["LP_1"].append((country, linearization_parameters["Y_1"][country]))
  for country in all_countries:
    if linearization_parameters["ERROR_2"][country] >= EPS_LIN:
      linearization_parameters["LP_2"].append((country, linearization_parameters["Y_2"][country]))
  
  logger.info('iteration {} - MAX_ERROR_1 {} - MAX_ERROR_2 {}'.format(iteration,
  linearization_parameters["MAX_ERROR_1"], linearization_parameters["MAX_ERROR_2"]))

  return linearization_parameters
