# run script from root of package
# get system dependencies for R packages used in project.
# use to create Dockerfile
library(remotes)
library(renv)
library(stringr)

# get list of packages required for
pckgs <- renv::dependencies()$Package
pckgs <- unique(pckgs)

# filter out renv and remotes
pckgs <- pckgs[pckgs != "renv" & pckgs != "remotes"]

# get list of system libraries
sys_deps_list <- c()

for (i in pckgs) {
  sys_deps <- system_requirements(
    "ubuntu-20.04",
    os_release = NULL,
    path = ".",
    package = i,
    curl = Sys.which("curl")
  )

  if (length(sys_deps) > 0) {
    for (ii in sys_deps) {
      z <- stringr::str_split(ii, " ")[[1]]
      z <- z[length(z)]
      sys_deps_list <- c(sys_deps_list, z)
    }
  }
}

# get unique system dependencies
sys_deps_list <- unique(sys_deps_list)

print('R Package Dependencies:')
print(pckgs)
print('Ubuntu 20.04 LTS System Dependencies')
print(sys_deps_list)
