# Maplandscape development notes and gotchas

## Branches

## Build

```
devtools::check_all()
```

Document package and update NAMESPACE:

```
devtools::document()
```

Check package loads:

```
devtools::load_all()
```

### Pkgdown and building docs

If `pkgdown::build_site()` throws a `Failed to connect to cloud.r-project.org port 443: Operation timed out` error. Try running `pkgdown::build_site(new_process=FALSE)`. 


## renv

To keep package dependencies up to date, run `renv_utils.R` in `/inst/dev/renv_utils.R`.

This will create an `renv::snapshot()` of the current state of the package dependencies. It will then call
`renv::update()` to update packages and `renv::snapshot()` to update the lockfile. Rinse and repeat until all packages update and install successfully. 

