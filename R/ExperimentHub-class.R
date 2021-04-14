### =========================================================================
### ExperimentHub objects
### -------------------------------------------------------------------------
###

setClass("ExperimentHub", contains="Hub")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

ExperimentHub <-
    function(..., hub=getExperimentHubOption("URL"),
             cache=getExperimentHubOption("CACHE"),
             proxy=getExperimentHubOption("PROXY"),
             localHub=getExperimentHubOption("LOCAL"),
             ask=getExperimentHubOption("ASK"))
{
    if ((cache == R_user_dir("ExperimentHub", which="cache")) && (Sys.getenv("EXPERIMENT_HUB_CACHE")=="")){
        olddefault = rappdirs::user_cache_dir(appname="ExperimentHub")
        if (dir.exists(olddefault) && (length(list.files(olddefault)) != 0)){
            warning("DEPRECATION: As of ExperimentHub (>1.17.2), default caching location has changed.\n",
                 "  Problematic cache: ", path.expand(olddefault), "\n",
                 "  See https://bioconductor.org/packages/devel/bioc/vignettes/ExperimentHub/inst/doc/ExperimentHub.html#default-caching-location-update\n")
            cache = olddefault
        }
    }

    if (is.null(proxy)){
        connect <- suppressWarnings(tryCatch({
            readBin(hub, n=1L, what="raw")
            TRUE
        }, error = function(...){
            FALSE
        }))
    } else {
        connect <- TRUE
        message("Assuming valid proxy connection through '",
                ifelse(is(proxy,"request"),
                       paste(unlist(proxy), collapse=":"),
                       proxy),
                "'",
                "\n If you experience connection issues consider ",
                "using 'localHub=TRUE'")
    }
    if (!connect && !localHub){
        message("Cannot connect to ExperimentHub server, using 'localHub=TRUE' instead")
        localHub <- !connect
    }
    .Hub("ExperimentHub", hub, cache, proxy, localHub, ask, ...)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor-like methods
###

setMethod("package", "ExperimentHub",
    function(x)
{
    query <- sprintf("SELECT preparerclass FROM resources\n
                     WHERE resources.id IN (%s)",
             AnnotationHub:::.id_as_single_string(x))
    ans <- AnnotationHub:::.db_query(dbfile(x), query)[["preparerclass"]]
    names(ans) <- names(x)
    ans
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### cache methods
###

setMethod("cache", "ExperimentHub",
    function(x, ..., force=FALSE, verbose=FALSE) {
        callNextMethod(x,
                       cache.root="ExperimentHub",
                       cache.fun=setExperimentHubOption,
                       proxy=getExperimentHubOption("PROXY"),
                       max.downloads=getExperimentHubOption("MAX_DOWNLOADS"),
                       force=force, verbose=verbose)
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

.tryload <- function(pkg) {
    ## Try to install/load; don't fail if not available
    success <- paste0("see ?", pkg, " and browseVignettes('",
                      pkg, "') for documentation")
    if (!pkg %in% rownames(installed.packages()))
        BiocManager::install(pkg, suppressUpdates=TRUE)
    if (pkg %in% rownames(installed.packages())) {
        suppressPackageStartupMessages({
            require(pkg, quietly = TRUE, character.only = TRUE)
        })
        message(success)
    }
}

setMethod("[[", c("ExperimentHub", "numeric", "missing"),
    function(x, i, j, ..., force=FALSE, verbose=TRUE)
{
    if (length(x[i]) != 1L)
        stop("'i' must be length 1")
    pkg <- AnnotationHub:::.count_resources(x[i], "preparerclass")
    .tryload(pkg)
    callNextMethod(x, i, j, ..., force=force, verbose=verbose)
    ## or AnnotationHub:::.Hub_get1(x[i])
})

setMethod("[[", c("ExperimentHub", "character", "missing"),
    function(x, i, j, ..., force=FALSE, verbose=TRUE)
{
    if (length(i) != 1L)
        stop("'i' must be length 1")
    idx <- match(i, names(AnnotationHub:::.db_uid(x)))
    if (is.na(idx)){
        status = recordStatus(x, i)
        if (isLocalHub(x)){
            if (status$status == "Public")
                stop(
                    "File not previously downloaded.\n",
                    "  Run with 'localHub=FALSE'",
                    call.=FALSE
                )
        }
        if (status$status == "Public" && (as.Date(status$dateadded) >=
                as.Date(snapshotDate(x))))
            stop(status$record, " added after current Hub snapshot date.\n",
                 "  added: ", as.character(status$dateadded), "\n",
                 "  snapshote date: ",  as.character(snapshotDate(x)),
                 call.=FALSE)

        msg <- status$status
        if (!is.null(status$dateremoved))
            msg <- paste0(msg, "\n  Resource removed on: ", status$dateremoved)
        stop(msg, call.=FALSE)
    }

    pkg <- AnnotationHub:::.count_resources(x[i], "preparerclass")
    .tryload(pkg)
    callNextMethod(x, i, j, ..., force=force, verbose=verbose)
    ## or AnnotationHub:::.Hub_get1(x[idx])
})
