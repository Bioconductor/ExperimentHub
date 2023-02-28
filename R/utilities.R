### =========================================================================
### Helpers for package-specific resource discovery
### -------------------------------------------------------------------------

## export resources as accessor functions
.HUB <- new.env(parent = emptyenv())

.GET_HUB <- function() get("eh", envir = .HUB)

.SET_HUB <- function(value) assign("eh", value, envir=.HUB)

.get_ExperimentHub <- function() {
     eh <- try(.GET_HUB(), silent=TRUE)
     if (inherits(eh, "try-error")) {
       eh <- ExperimentHub::ExperimentHub()
       .SET_HUB(eh)
     }
     eh
}

.hubAccessorFactory <- function(ehid) {
     force(ehid)
     function(metadata=FALSE) {
         eh <- .get_ExperimentHub()
         if (metadata) {
             eh[ehid]
         } else
             eh[[ehid]]
     }
}

createHubAccessors <- function(pkgname, titles) {
    ## map titles to ExperimentHub identifiers
    eh <- query(.get_ExperimentHub(), pkgname)

    ehids <- vapply(titles, function(tle, exactMatch) {
        ehid <- names(subset(eh, eh$title == tle))
        if (length(ehid) == 0L) {
            stop(sQuote(tle), " not found in ExperimentHub")
        } else if (length(ehid) != 1L) {
            stop(sQuote(tle),
                 " matches more than 1 ExperimentHub resource")
        }
        ehid
    }, character(1))

    ## create and export accessor functions in package namespace
    ns <- asNamespace(pkgname)
    for (i in seq_along(titles)) {
        assign(titles[[i]], .hubAccessorFactory(ehids[[i]]), envir=ns)
        namespaceExport(ns, titles[[i]])
    }
}

## resource discovery

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
