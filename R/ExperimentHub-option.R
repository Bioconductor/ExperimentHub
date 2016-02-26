.EH_hub_options <- new.env(parent=emptyenv())

getExperimentHubOption <- function(arg) {
    arg <- toupper(arg)
    key <- c("URL", "CACHE", "PROXY", "MAX_DOWNLOADS")
    if (!arg %in% key)
        stop(paste0("'arg' must be one of 'URL', 'CACHE', 'PROXY', ",
             "or 'MAXDOWNLOADS'"))
    .EH_hub_options[[arg]]
}

setExperimentHubOption <- function(arg, value)
{
    key <- .hub_option_key(toupper(trimws(arg)))

    .EH_hub_options[[key]] <- switch(key, URL=, CACHE={
        value <- as.character(value)
        stopifnot(isSingleString(value))
        value
    }, MAX_DOWNLOADS={
        value <- as.integer(value)
        stopifnot(isSingleInteger(value))
        value
    }, PROXY={
        if (is.null(value) || inherits(value, "request"))
            value
        else if (isSingleString(value)) {
            .httr_proxy(value)
        } else {
            txt <- "'value' must be an httr proxy request (use_proxy()),
                    character(1), or NULL"
            stop(paste(strwrap(txt, exdent=2), collapse="\n"))
        }
    })
}
