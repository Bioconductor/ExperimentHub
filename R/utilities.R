### =========================================================================
### Helpers for package-specific resource discovery
### -------------------------------------------------------------------------

listPackageResources <- function(package, filterBy=character()) { 
    if (!is.character(filterBy))
        stop("'filterBy' must be a character vector")
    suppressMessages({eh <- ExperimentHub()})
    if (!package %in% unique(package(eh)))
        stop(paste0("'", package, "' resources are not in ExperimentHub"))

    sub <- query(eh, package)
    mcols(query(sub, filterBy))$title
}

loadPackageResources <- function(package, filterBy=character()) {
    resources <- listPackageResources(package, filterBy)
    ans <- lapply(resources, function(x) suppressMessages(get(x)()))
    names(ans) <- resources
    ans
}
