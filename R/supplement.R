.NA_version_ <- numeric_version("0.0")  # proxy for unknown version
.as.numeric_version <-
    function(x, ...)
{
    if (is(x, "character"))
        x[x == "unknown"] <- as.character(.NA_version_)
    base::as.numeric_version(x)
}

recipeFromJson <- function( ahroot = '/var/ahdata/upload', jsonpath ){
    path <- file.path(ahroot, jsonpath)[1]
    lst <- .decodeNA(fromJSON(file=path))
    lst <- lst[!sapply(lst, is.null)]         # replace with default values
    recipe <- new( 'AnnotationHubRecipe',
                                        #              recipeName = paste( lst$Recipe$package, lst$Recipe[[1]], sep = '::' ),
                  recipeName = paste( lst$Recipe[[1]], sep = '::' ),
                  inputFiles = file.path( ahroot, lst$SourceFile ),
                  outputFile = file.path( ahroot, lst$RDataPath )
                  )
    recipe
}

.getAuth <- function(){
    credential.file <- file.path(Sys.getenv("HOME"), ".AnnotationHubServer",
                                 "credentials.json")
    if (file.exists(credential.file))
        return(unlist(fromJSON(paste0(
            readLines(credential.file), collapse=""))))
    else
        return(c(username="", password=""))
}

.onload <- function(libname, pkgname){
    database <- "ExperimentHub"
    credentials <- .getAuth()
    ## See MongoHandle-class.R in AnnotationHubServer package
    ## This creates an instance of the MongoHandle-class
    .mongo <<- .MongoHandle(database=database, namespace="metadata")
}

setOldClass("mongo")

.MongoHandle <- setRefClass("MongoHandle",
                            fields=list(
                                mongo="mongo",
                                database="character",
                                namespace="character"),
                            methods=list(
                                getMongo=function() {
                                    if (is.null(.self$mongo))
                                        .self$mongo <- .loadMongo(.self$database)
                                    .self$mongo
                                },
                                setMongo=function(mongo) {
                                    .self$mongo <- mongo
                                    .self
                                },
                                getNamespace=function() {
                                    sprintf("%s.%s", .self$database, .self$namespace)
                                },
                                setNamespace=function(namespace) {
                                    .self$namespace <- namespace
                                    .self
                                },
                                setDatabase=function(database) {
                                    .self$database <- database
                                    .self
                                },
                                getDatabase=function() {
                                    .self$database
                                },
                                getCustomNamespace=function(namespace) {
                                    sprintf("%s.%s", .self$database, namespace)
                                },
                                finalize=function() {
                                    if (!is.null(.self$mongo)) {
                                        mongo.destroy(.self$mongo)
                                    }
                                }))

.loadMongo <- function(database){
    credentials <- .getAuth()
    mongo <- mongo.create(db=database, username=credentials['username'],
                          password=credentials['password'])
}

json2mongo <-  function(jsonFile,filename){
    l <- fromJSON(file=jsonFile)
    l <- .decodeNA( l )
    buf <- mongo.bson.buffer.create()
    for (name in names(l)){
        if (grepl("Date", name)){
            l[[name]] <- as.POSIXct(as.character(l[[name]]))
        }
        if (grepl("Size", name))
            l[[name]] <- as.integer(l[[name]])
        if( length( l[[name]] ) > 0 ){
            if( !is.na( l[[name]] ) )
                mongo.bson.buffer.append( buf, name, l[[name]] )
        }
    }
    mongo.bson.buffer.append( buf, "jsonfile", filename )
    ns <- "test.people" ## name of the table
    b <- mongo.bson.from.buffer(buf)
    return( b )
}

.decodeNA <- function(lst){
    .NAmap <- setNames(list(NA, NA_integer_, NA_character_, NA_real_,
                            NA_complex_),
                       c(".__NA__logical_", ".__NA__integer_",
                         ".__NA__character_", ".__NA__numeric_",
                         ".__NA__complex_"))
    rapply(lst, function(elt) {
        isNA <- elt %in% names(.NAmap)
        if (any(isNA)) {
            type <- elt[isNA][[1]]
            ## reverse coercion
            elt[isNA] <- NA
            elt <- as(elt, class(.NAmap[[type]]))
        }
        elt
    }, "character", how="replace")
}

#' Select the distinct values of a column.
.project <- function( col ){
    field = list ( 1 )
    names(field)[1] = col
    field[[1]] = col
    ret <- mongo.find( .mongo$getMongo(), .mongo$getNamespace(), fields = field )
    retvec <- NULL
    while ( mongo.cursor.next( ret ) ){
        rid <- mongo.bson.to.list( mongo.cursor.value( ret ) )
        retvec <- c( retvec, rid[[col]] )
    }
    return( unique( retvec ) )
}

library(Rook)
library(rjson)
library(rmongodb)
library(AnnotationHubServer)

