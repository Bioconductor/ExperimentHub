\name{utilities}
\alias{utilities}

\alias{listPackageResources}
\alias{loadPackageResources}


\title{
  Utility functions for discovering package-specific resources
  in ExperimentHub}

\description{
  List and load resources from ExperimentHub filtered by package
  name and optional search terms.
}

\usage{
listPackageResources(package, filterBy = character())
loadPackageResources(package, filterBy = character())
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
  \code{listPackageResources} returns a character vector; 
  \code{loadPackageResources} returns a list of data objects.
}

\seealso{
}

\examples{
\dontrun{
## All packages with resources hosted in ExperimentHub:
eh <- ExperimentHub()
unique(package(eh))

## All resources associated with the 'GSE62944' package:
listPackageResources("GSE62944")

## Resources associated with the 'curatedMetagenomicData' package
## filtered by 'plaque.abundance':
listPackageResources("curatedMetagenomicData", "plaque.abundance")

## 'loadPackageResources()' returns a list of the data objects:
loadPackageResources("curatedMetagenomicData", "plaque.abundance")
}
}
\keyword{utilities}
