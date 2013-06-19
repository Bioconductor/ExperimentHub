#' run A demo session.
#' 
#' @author Chandler Zuo \email{zuo@@stat.wisc.edu}
#' 
#' @export
runtest <- function(){
  s <- Rhttpd$new()
  
  s$add(name="webform",
        app <-Rook::URLMap$new(
          '/insert'=function(env){
            req <- Rook::Request$new(env)
            res <- Rook::Response$new()
            res$write(paste('<h1>Please fill the form </h1>\n'))
            res$write(c('First Name','\n'))
            res$write(c('<form method="POST">'))
            res$write(c('<input type="text" name="fname">','<br>'))
            res$write(c('Last Name','<br>'))
            res$write(c('<input type="text" name="lname">','<br>'))
            res$write(c('Age','<br>'))
            res$write(c('<input type="range" name="age" min="0" max="130">','<br>'))
            res$write('<input type="submit" name="Submit">\n</form>\n<br>')

            if(!is.null(req$POST())){
              lname <- fname <- age <- NULL
              ## check the values to be inserted
              if (!is.null(req$POST()[['fname']]))
                fname <- toupper(req$POST()[['fname']])
              if (!is.null(req$POST()[['lname']]))
                lname <- toupper(req$POST()[['lname']])
              if (!is.null(req$POST()[['age']]))
                age <- as.integer(req$POST()[['age']])
              ## the error flag
              flag <- FALSE
              buf <- mongo.bson.buffer.create()
              if( !is.na(age) & !is.null(age) & age > 0 )
                mongo.bson.buffer.append(buf, "age", age)
              if( is.na(age) || age <= 0 || age > 130 )
                flag <- TRUE
              ##insert to the database
              ns <- "test.people" ## name of the table
              if(!is.null(fname))
                mongo.bson.buffer.append(buf, "fname", fname)
              if(!is.null(lname))
                mongo.bson.buffer.append(buf, "lname", lname)
              ## insert the record if no error
              if(!flag){
                b <- mongo.bson.from.buffer(buf)
                mongo <- mongo.create(db="test")
                res$write(paste("<br>inserted value: ",mongo.insert(mongo, ns, b),"<br>"))
                mongo.disconnect(mongo)
              }
              else
                res$write(paste("<br>error in insersion occured <br>"))
            }
            res$write(sprintf('<input type="button" value="Seach by Age" onclick="window.location=\'%s\'"">\t\t',req$to_url('/search')))
            res$write(sprintf('<input type="button" value="View Results" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/view')))
            res$write('<br><br><br><input type="button" value="My Homepage" onclick="window.location=\'http://www.stat.wisc.edu/~zuo\'">\t\t')
            res$write('<input type="button" value="My R Package" onclick="window.location=\'http://code.google.com/p/chip-seq-power\'">\n<br>')
            res$finish()
          },
          '/view' = function ( env ){
            req <- Rook::Request$new(env)
            res <- Rook::Response$new()
            res$write(paste('<h1>View Records </h1>\n'))
            mongo <- mongo.create(db="test")
            cursor <- mongo.find(mongo, "test.people")
            i=0
            while (mongo.cursor.next(cursor)){
              rid <- mongo.bson.to.list(mongo.cursor.value(cursor))
              rec <- paste("ID = ",i,sep="")
              if(length(rid)>1){
                for(j in 2:length(rid)){
                  rec <- paste(rec,"\t",names(rid)[j]," = ",rid[[j]])
                }
              }
              i <- i+1
              res$write(paste(rec,"<br>"))
            }
            res$write(sprintf('<input type="button" value="Seach by Age" onclick="window.location=\'%s\'"">\t\t',req$to_url('/search')))
            res$write(sprintf('<input type="button" value="Insert Records" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/insert')))
            res$write('<br><br><input type="button" value="My Homepage" onclick="window.location=\'http://www.stat.wisc.edu/~zuo\'">\t\t')
            res$write('<input type="button" value="My R Package" onclick="window.location=\'http://code.google.com/p/chip-seq-power\'">\n<br>')
            res$finish()
          },
          
          '/search' = function ( env ){
            req <- Rook::Request$new(env)
            res <- Rook::Response$new()
            res$write('<h1>Please enter the age</h1>\n')
            res$write('<form method="POST">')
            res$write('<input type="range" name="age" min="0" max="130" ><br>')
            res$write('<input type="submit" name="Submit"></form>\n<br>')
            
            if(!is.null(req$POST())){
              age <- NULL
              if (!is.null(req$POST()[['age']]))
                age <- as.integer(req$POST()[['age']])
              ## the error flag
              flag <- FALSE
              if( is.na(age) || age <= 0 || age > 130 )
                flag <- TRUE
              ##insert to the database
              ns <- "test.people" ## name of the table
              ## insert the record if no error
              if(!flag){
                mongo <- mongo.create(db="test")
                buf <- mongo.bson.buffer.create()
                mongo.bson.buffer.start.object(buf,"age")
                mongo.bson.buffer.append(buf,"$gt",age)
                mongo.bson.buffer.finish.object(buf)
                cursor <- mongo.find(mongo, "test.people",mongo.bson.from.buffer(buf))
                i=0
                while (mongo.cursor.next(cursor)){
                  rid <- mongo.bson.to.list(mongo.cursor.value(cursor))
                  rec <- paste("ID = ",i,sep="")
                  if(length(rid)>1){
                    for(j in 2:length(rid)){
                      rec <- paste(rec,"\t",names(rid)[j]," = ",rid[[j]])
                    }
                  }
                  i <- i+1
                  res$write(paste(rec,"<br>"))
                }
              }
              else
                res$write(paste("<br>error in entered value <br>"))
            }
            
            res$write(sprintf('<input type="button" value="Insert Records" onclick="window.location=\'%s\'"">\t\t',req$to_url('/insert')))
            res$write(sprintf('<input type="button" value="View Results" onclick="window.location=\'%s\'"">\n<br>',req$to_url('/view')))
            res$write('<br><br><input type="button" value="My Homepage" onclick="window.location=\'http://www.stat.wisc.edu/~zuo\'">\t\t')
            res$write('<input type="button" value="My R Package" onclick="window.location=\'http://code.google.com/p/chip-seq-power\'">\n<br>')
            res$finish()
          },
          '/?' = function(env){
            req <- Rook::Request$new(env)
            res <- Rook::Response$new()
            res$redirect(req$to_url('/insert'))
            res$finish()
          }
          )
        )
  
  s$start()
  s$browse("webform")
  
}
  


