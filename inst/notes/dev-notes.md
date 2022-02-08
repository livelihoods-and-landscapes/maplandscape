# Maplandscape development notes and gotchas

## branches

## renv

To keep package dependencies up to date, run `renv_utils.R` in `/inst/dev/renv_utils.R`.

This will create an `renv::snapshot()` of the current state of the package dependencies. It will then call
`renv::update()` to update packages and `renv::snapshot()` to update the lockfile. Rinse and repeat until all packages update and install successfully. 

