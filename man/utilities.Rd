\name{utilities}
\alias{utilities}

\alias{listResources}
\alias{loadResources}


\title{
  Utility functions for discovering package-specific resources
  in ExperimentHub}

\description{
  List and load resources from ExperimentHub filtered by package
  name and optional search terms.
}

\usage{
listResources(package, filterBy = character())
loadResources(package, filterBy = character())
}

\arguments{
  \item{package}{
    A \code{character(1)} name of a package with resources hosted in
    ExperimentHub.
  }
  \item{filterBy}{
    A \code{character()} vector of search terms for additional filtering. 
    Can be any terms found in the metadata (mcols()) of the resources. 
    When not provided, there is no additional filtering and all resources 
    associated with the given package are returned.
  }
}

\value{
  \code{listResources} returns a character vector; 
  \code{loadResources} returns a list of data objects.
}

\seealso{
}

\examples{
\dontrun{
## All packages with resources hosted in ExperimentHub:
eh <- ExperimentHub()
unique(package(eh))

## All resources associated with the 'GSE62944' package:
listResources("GSE62944")

## Resources associated with the 'curatedMetagenomicData' package
## filtered by 'plaque.abundance':
listResources("curatedMetagenomicData", "plaque.abundance")

## 'loadResources()' returns a list of the data objects:
loadResources("curatedMetagenomicData", "plaque.abundance")
}
}
\keyword{utilities}
