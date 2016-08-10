### =========================================================================
### Helpers for package-specific resource discovery
### -------------------------------------------------------------------------

.filterResources_EH <- function(package, filterBy=character()) {
    if (!is.character(filterBy))
        stop("'filterBy' must be a character vector")
    suppressMessages({eh <- ExperimentHub()})
    if (!package %in% unique(package(eh)))
        stop(paste0("'", package, "' resources were not found in ExperimentHub"))

    sub <- query(eh, package)
    if (length(filterBy))
        query(sub, filterBy)
    else
        sub
}

setMethod("listResources", "ExperimentHub", 
    function(hub, package, filterBy=character()) {
        metadata <- .filterResources_EH(package, filterBy)
        mcols(metadata)$title
})

setMethod("loadResources", "ExperimentHub",
    function(hub, package, filterBy=character()) {
        metadata <- .filterResources_EH(package, filterBy)
        eh <- ExperimentHub()
        lapply(names(metadata), function(i) eh[[i]]) 
})
