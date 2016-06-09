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
    if (length(filterBy))
        mcols(query(sub, filterBy))$title
    else
        mcols(sub)$title
}

loadPackageResources <- function(package, filterBy=character()) {
    if (!package %in% rownames(installed.packages()))
        biocLite(package, suppressUpdates=TRUE)
    if (!isNamespaceLoaded(package))
        attachNamespace(package)

    resources <- listPackageResources(package, filterBy)
    ans <- lapply(resources, function(x) suppressMessages(get(x)()))
    names(ans) <- resources
    ans
}
