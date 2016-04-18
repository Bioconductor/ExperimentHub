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
             proxy=getExperimentHubOption("PROXY")) 
{
    .Hub("ExperimentHub", hub, cache, proxy, ...)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor-like methods 
###

setMethod("package", "ExperimentHub",
    function(x)
{
    if (length(x) != 1L)
        stop("'x' must be of length 1")
    AnnotationHub:::.count_resources(x, "preparerclass")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### cache methods
###

setMethod("cache", "ExperimentHub",
    function(x, ...) {
        callNextMethod(x,
                       cache.root=".ExperimentHub", 
                       cache.fun=setExperimentHubOption,
                       proxy=getExperimentHubOption("PROXY"), 
                       max.downloads=getExperimentHubOption("MAX_DOWNLOADS"))
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting 
###

setMethod("[[", c("ExperimentHub", "numeric", "missing"),
    function(x, i, j, ...)
{
    if (length(x[i]) != 1L)
        stop("'i' must be length 1")
    packagename <- AnnotationHub:::.count_resources(x[i], "preparerclass")
    if (!packagename %in% rownames(installed.packages()))
        biocLite(packagename, suppressUpdates=TRUE)
    message("see ?", packagename, " and browseVignettes('",
            packagename, "') for documentation")
    callNextMethod(x, i, j, ...)
    ## or AnnotationHub:::.Hub_get1(x[i])
})

setMethod("[[", c("ExperimentHub", "character", "missing"),
    function(x, i, j, ...)
{
    if (length(i) != 1L)
        stop("'i' must be length 1")
    idx <- match(i, names(AnnotationHub:::.db_uid(x)))
    if (is.na(idx))
        stop("unknown key ", sQuote(i))

    packagename <- AnnotationHub:::.count_resources(x[i], "preparerclass")
    if (!packagename %in% rownames(installed.packages()))
        biocLite(packagename, suppressUpdates=TRUE)
    message("see ?", packagename, " and browseVignettes('",
            packagename, "') for documentation")
    callNextMethod(x, i, j, ...)
    ## or AnnotationHub:::.Hub_get1(x[idx])
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show 
###

setMethod("show", "ExperimentHub", function(object) 
{
    len <- length(object)
    cat(sprintf("%s with %d record%s\n", class(object), len,
                ifelse(len == 1L, "", "s")))
    cat("# snapshotDate():", snapshotDate(object), "\n")
    cat("# package(): ", package(object)[[1]], "\n", sep="")
    callNextMethod(object) 
})
