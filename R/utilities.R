### =========================================================================
### Helpers for package-specific resource discovery
### -------------------------------------------------------------------------

.filterResources <- function(package, filterBy=character()) {
    if (!is.character(filterBy))
        stop("'filterBy' must be a character vector")
    suppressMessages({eh <- ExperimentHub()})
    if (!package %in% unique(package(eh)))
        stop(paste0("'", package, "' resources are not in ExperimentHub"))

    sub <- query(eh, package)
    if (length(filterBy))
        query(sub, filterBy)
    else
        sub
}

listResources <- function(package, filterBy=character()) { 
    metadata <- .filterResources(package, filterBy)
    mcols(metadata)$title
}

loadResources <- function(package, filterBy=character()) {
    metadata <- .filterResources(package, filterBy)
    eh <- ExperimentHub()
    lapply(names(metadata), function(i) eh[[i]]) 
}
