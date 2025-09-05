installed <- row.names(installed.packages())

install_packages <- function(pkgs_to_install) {
  installed <- rownames(installed.packages())
  
  for (pkg in pkgs_to_install) {
    if (!(pkg %in% installed)) {
      message(sprintf("Installing '%s' from CRAN...", pkg))
      tryCatch({
        install.packages(pkg)
      }, error = function(e) {
        message(sprintf("'%s' not found on CRAN. Trying Bioconductor...", pkg))
        if (!requireNamespace("BiocManager", quietly = TRUE)) {
          install.packages("BiocManager")
        }
        tryCatch({
          BiocManager::install(pkg)
        }, error = function(e2) {
          message(sprintf("Failed to install '%s' from Bioconductor.", pkg))
        })
      })
    } else {
      message(sprintf("Package '%s' is already installed.", pkg))
    }
  }
}

install.packages(rpacks)

sapply(setdiff(rpacks, row.names(installed.packages())), BiocManager::install)
