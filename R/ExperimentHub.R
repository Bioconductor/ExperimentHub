#' The ExperimentHub project.
#' 
#' @author Chandler Zuo \email{zuo@@stat.wisc.edu}
#' 
#' @export
run <- function(){
##    s$stop()
    s <- Rhttpd$new()
    s$add(name="ExperimentHub",
          app <-Rook::URLMap$new(
              '/load'=function(env){
                  req <- Rook::Request$new(env)
                  res <- Rook::Response$new()
                  res$write(paste('<h1>Please select the data to upload </h1>\n'))
                  res$write(c('<form method="POST" enctype="multipart/form-data">\n' ) )
                  res$write(c('Select your JSON file','\n'))
                  res$write(c('<input type="file" name="jsonfile" stype="width:400px" value="Upload JSON File">','<br>'))
                  res$write(c('Select your data file','\n'))
                  res$write(c('<input type="file" name="data" stype="width:400px" value="Upload Data File">','<br>'))
                  res$write( 'Convert raw data into RData<select name="convert">' )
                  res$write( '<option value="Yes">Yes</option><option value="No">No</option></select><br>' )
                  res$write('<input type="submit" name="Upload" value="Upload">\n</form>\n<br>')
                  res$write( paste( names( req$POST()[[ "jsonfile" ]] ), collapse = "," ))
                  if( is.null(req$POST()[[ "jsonfile" ]] ) | is.null( req$POST()[[ "data" ]] ) )
                      res$write( ' Both json file and data must be provided. <br> ' )
                  else{
                      ## Create a mongoDB connection instance
                      .onload()
                      ## parse the JSON file and insert the metadata structure into database
                      ## See mongo.R in AnnotationHubServer package
                      print( req$POST()[["jsonfile"]] )
                      b <- json2mongo( req$POST()[[ "jsonfile" ]]$tempfile, req$POST()[[ "jsonfile" ]]$filename )
                      res$write(mongo.insert( .mongo$getMongo(), .mongo$getNamespace(), b ) )
                      res$write(c( "File to be added to ", mongo.bson.to.list( b )[['SourceFile']],"<br>") )
                      res$write(sprintf('<input type="button" value="Upload More Data" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/load')))
                      .mongo$finalize()
                      res$write(c("<br>data file located at",req$POST()[[ "data" ]]$tempfile,"<br>" ) )
                      ## move the uploaded data file
                      newfileloc <- paste( "/var/ahdata/upload/", mongo.bson.to.list( b )[[ "SourceFile" ]], sep = "" )
                      print(paste("create dir", dir.create( dirname( newfileloc ), recursive = TRUE ) , sep = " "  ) )
                      print(paste("create", file.create( newfileloc ) ) )
                      print(paste("copy", file.copy( from = req$POST()[[  "data" ]]$tempfile, to = newfileloc, recursive = TRUE ),sep = " " ) )
                      print(paste("remove", file.remove(from=req$POST()[["data"]]$tempfile) ) )
                      ## move the json file
                      jsonfileloc <- paste( "/var/ahdata/upload/", mongo.bson.to.list( b )[[ "jsonfile" ]], sep = "" )
                      print(paste("create dir", dir.create( dirname( jsonfileloc ), recursive = TRUE ) , sep = " "  ) )
                      print(paste("create", file.create( jsonfileloc ) ) )
                      print(paste("copy", file.copy( from = req$POST()[[  "jsonfile" ]]$tempfile, to = jsonfileloc, recursive = TRUE ),sep = " " ) )
                      print(paste("remove", file.remove(from=req$POST()[["jsonfile"]]$tempfile) ) )
                      ## generate RData file
                      if( req$POST()[[ "convert" ]] == "Yes" ){
                          recipe <- recipeFromJson( jsonpath = mongo.bson.to.list( b )[[ 'jsonfile' ]], ahroot = s$full_url( 'root' ) )
                          AnnotationHubData::run( recipe )
                      }
                  }
                  res$write(sprintf('<input type="button" value="View All Metadata" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/view')))
                  res$write(sprintf('<input type="button" value="Search Data" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/search')))
                  res$finish()
              },
              '/view' = function ( env ){
                  req <- Rook::Request$new(env)
                  res <- Rook::Response$new()
                  res$write(paste('<h1> View Records </h1>\n'))
                  .onload()
                  cursor <- mongo.find( .mongo$getMongo(), .mongo$getNamespace() )
                  i=0
                  res$write(c('<table border="1"><tr><th>ID</th><th>RDataClass</th><th>Genome</th><th>Species</th><th>Tags</th><th>TaxonomyId</th><th>Title</th><th>RDataVersion</th><th>Maintainer</th><th>RDataDateAdded</th><th>Description</th><th>Download</th></tr>'))
                  while (mongo.cursor.next(cursor)){
                      i <- i+1
                      rid <- mongo.bson.to.list(mongo.cursor.value(cursor))
                      fileloc <- paste( s$full_url( 'root' ), '/', rid[[ 'SourceFile' ]], sep = '' )
                      rdataloc <- paste( s$full_url( 'root' ), '/', rid[[ 'RDataPath' ]], sep = '' )
                      res$write( c( '<tr><td>', i, '</td><td>', rid[[ 'RDataClass' ]], '</td><td>', rid[[ 'Genome' ]], '</td><td>', rid[[ 'Species' ]], '</td><td>', rid[[ 'Tags' ]], '</td><td>', rid[[ 'TaxonomyId' ]], '</td><td>', rid[[ 'Title' ]], '</td><td>', rid[[ 'RDataVersion' ]], '</td><td>', rid[[ 'Maintainer' ]], '</td><td>', rid[[ 'RDataDateAdded' ]], '</td><td>', rid[[ 'Description' ]], '</td><td><a href="', fileloc, '">Sourcefile</a>', '  <a href="', rdataloc, '">RData</a></td></tr>' ) )
                  }
                  res$write(paste("</table>"))
                  res$write(sprintf('<input type="button" value="Upload More Data" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/load')))
                  res$write(sprintf('<input type="button" value="Search Data" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/search')))
                  res$finish()
              },
          '/search' = function ( env ){
              req <- Rook::Request$new(env)
              res <- Rook::Response$new()
              
              .onload()

              formNames <- c( "class", "genome", "species", "taxonomyid" )
              colNames <- c( "RDataClass", "Genome", "Species", "TaxonomyId" )

              res$write('<h1>Please enter search criteria</h1>\n')
              res$write('<form method="POST">')
              res$write(c('<table border="1"><tr><th>FIELD</th><th>VALUE</th></tr>'))
              for( i in 1:length( colNames ) ){
                  res$write( c( '<tr><td>', colNames[i], '</td>' ) )
                  res$write( c( '<td><select name="', formNames[i], 'class" >' ) )
                  for( opt in .project( colNames[i] ) ){
                      res$write( c( '<option value="', opt, '">', opt, '</option>' ) )
                  }
                  res$write( '<option value="Any">Any</option>' )
                  res$write('</select></td></tr>')
              }
              res$write( '</table>' )
              
              class <- genome <- species <- taxonomyid <- NULL
              if(!is.null(req$POST())){
                  if (!is.null(req$POST()[['class']]))
                      class <- as.character(req$POST()[['class']])
                  if (!is.null(req$POST()[['genome']]))
                      genome <- as.character(req$POST()[['genome']])
                  if (!is.null(req$POST()[['species']]))
                      species <- as.character(req$POST()[['species']])
                  if (!is.null(req$POST()[['taxonomyid']]))
                      taxonomyid <- as.character(req$POST()[['taxonomyid']])
                  
                  buf <- mongo.bson.buffer.create()
                  if( class != 'Any' )
                      mongo.bson.buffer.append(buf,"RDataClass",class)
                  if( genome != 'Any' )
                      mongo.bson.buffer.append(buf,"Genome",genome)
                  if( species != 'Any' )
                      mongo.bson.buffer.append(buf,"Species",species)
                  if( taxonomyid != 'Any' )
                      mongo.bson.buffer.append(buf,"TaxonomyId",taxonomyid)
                  
                  cursor <- mongo.find( .mongo$getMongo(), .mongo$getNamespace(), mongo.bson.from.buffer( buf ) )
                  
                  i=0
                  res$write(c('<table border="1"><tr><th>ID</th><th>RDataClass</th><th>Genome</th><th>Species</th><th>Tags</th><th>TaxonomyId</th><th>Title</th><th>RDataVersion</th><th>Maintainer</th><th>RDataDateAdded</th><th>Description</th><th>Download</th></tr>'))
                  while (mongo.cursor.next(cursor)){
                      i <- i+1
                      rid <- mongo.bson.to.list(mongo.cursor.value(cursor))
                      fileloc <- paste( s$full_url( 'root' ), '/', rid[[ 'SourceFile' ]], sep = '' )
                      rdataloc <- paste( s$full_url( 'root' ), '/', rid[[ 'RDataPath' ]], sep = '' )
                      res$write( c( '<tr><td>', i, '</td><td>', rid[[ 'RDataClass' ]], '</td><td>', rid[[ 'Genome' ]], '</td><td>', rid[[ 'Species' ]], '</td><td>', rid[[ 'Tags' ]], '</td><td>', rid[[ 'TaxonomyId' ]], '</td><td>', rid[[ 'Title' ]], '</td><td>', rid[[ 'RDataVersion' ]], '</td><td>', rid[[ 'Maintainer' ]], '</td><td>', rid[[ 'RDataDateAdded' ]], '</td><td>', rid[[ 'Description' ]], '</td><td><a href="', fileloc, '">SourceFile</a>', '  <a href="', rdataloc, '">RData</a></td></tr>' ) )
                  }
                  res$write( "</table>" )
                  res$write(sprintf('<input type="button" value="Upload More Data" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/load')))
                  res$write(sprintf('<input type="button" value="View All Data" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/view')))
              }
              else
                  res$write(paste("<br>No value entered. <br>"))
              
              res$write(sprintf('<input type="button" value="Insert Records" onclick="window.location=\'%s\'"">\t\t',req$to_url('/insert')))
              res$write(sprintf('<input type="button" value="View Results" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/view')))
              res$finish()
          },
              '/?' = function(env){
                  req <- Rook::Request$new(env)
                  res <- Rook::Response$new()
                  res$redirect(req$to_url('/load'))
                  res$finish()
              }
              )
          )
    s$add( name = 'root', app = File$new( '/var/ahdata/upload' ) )
    s$start()
    s$browse( 'ExperimentHub' )
}

##source("~/ExperimentHub/R/supplement.R")


