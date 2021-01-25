#Copyright © 2016 RTE Réseau de transport d’électricité

# Copy the test study in a temporary folder

path0 <- tempdir()

sourcedir <- system.file("inst/testdata", package = "AdequacyPatch")


if(sourcedir == ""){ sourcedir <- system.file("testdata", package = "AdequacyPatch")}

if (length(strsplit(packageDescription("antaresRead")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllAntaresReadTests"="yes")
}


# Hack: For some unknown reason, this script is executed at some point of
# the R CMD CHECK before package is correctly installed and tests actually run.
# The following "if" prevents errors at this step
if (sourcedir != "") {

  studies <- list.files(
    path = sourcedir,
    pattern = "^antares-test-study.*\\.tar\\.gz$"
  )

  studies_names <- basename(studies)
  studies_names <- sub("\\.tar\\.gz$", "", studies_names)

  for (s in seq_along(studies)) {
    dir.create(file.path(path0, studies_names[s]))
    untar(file.path(sourcedir, studies[s]), exdir = file.path(path0, studies_names[s]))
  }


  assign(
    x = "studyPathS",
    value = file.path(path0, studies_names, "test_case"),
    envir = globalenv()
  )

  assign("nweeks", 2, envir = globalenv())
  assign("nmonths", 2, envir = globalenv())
  assign("firstDay", 113, envir = globalenv())
  assign("lastDay", 126, envir = globalenv())
}
