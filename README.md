<img src="man/figures/antares_simulator.png" align="right" width=250 />
<br/>

# antaresAdequacyPatch 

## Installation

Temporary overwriting the Installation instruction, until the merge is done with the master branch.

In order to use this version of the antaresAdequacyPatch, you will have to use:
```r
devtools::install_github("rte-antares-rpackage/antaresAdequacyPatch", ref="linearization")
```

To display the help of the package and see all the functions it provides, use:

```r 
help(package="antaresAdequacyPatch")
```


## Requirements

* You will need reticulate to run python code inside R:
```
install.packages("reticulate")
```
* You need a Python 3.7.1+
* Then create a .Renviron file with the path to your python:
  * If on Mac/Linux: create the $HOME/.Renviron file
  * If on Windows: create the c:\\Users\\$env:USERNAME\\Documents\\.Renviron file
* Inside the file, write:
```
RETICULATE_PYTHON="path-to-your-python/bin/python3"
```
* Install the pandas library on your python
https://pandas.pydata.org/pandas-docs/stable/getting_started/install.html
* Install the ortools library on your python
https://developers.google.com/optimization/install


### L'utilisation de la fonction `run_adq`

La fonction principale est nommée `run_adq` et permet de lancer l'adequacy patch sur une étude Antares.


La fonction accepte 8 arguments :

* opts
* areas : Areas concernées par l'adquacy patch.
* virtual_areas : Plus utilisé aujourd'hui (à supprimer)
* mcYears : mcYears sur lesquelles appliquer le traitement.
* ext : Nom de l'output pour la sortie après adequacy, si NULL, la sortie sera écrasée
* nbcl : Nombre de coeurs de calcul
* antaresfbzone : Nom de la zone flow-based
* showProgress : Montrer l'avancement
* thresholdFilter : Filtre des résultats (seuil après lequel les modifications deviennent trop importantes pour un pas de temps donné)
* core_ahc: Si TRUE, utilise l'area CORE, si FALSE utilise l'area CWE
* calculate_mc_all: Réalise l'aggregation des timestep horaires lorsque activé (TRUE). __Non fonctionnel pour le moment__
* log_detail: TRUE pour plus de détails, default à FALSE

```{r, eval=FALSE}
library(AdequacyPatch)
opts <- setSimulationPath("myoutputstudy")

areas <- c("fr", "at", "be", "de", "nl", "es", "ukgb", "ch", "ie", "itn", "model_description_fb")
virtual_areas = getAreas(select = "_", regexpSelect = TRUE,
                         exclude = c("model_description_fb"), regexpExclude = FALSE)


run_adq(opts = opts,
					areas = areas,
					virtual_areas = virtual_areas,
					mcYears = "all",
					antaresfbzone = "model_description_fb",
					ext = 'adq',
					nbcl = 8, thresholdFilter = 100)



```
