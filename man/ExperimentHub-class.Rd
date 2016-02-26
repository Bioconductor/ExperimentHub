\name{ExperimentHub-objects}
\docType{class}

% Classes
\alias{class:ExperimentHub}
\alias{ExperimentHub-class}

% Constructor
\alias{ExperimentHub}

% Accessor-like methods
\alias{cache,ExperimentHub-method}
\alias{package,ExperimentHub-method}

% Subsetting
\alias{[[,ExperimentHub,character,missing-method}
\alias{[[,ExperimentHub,numeric,missing-method}


\title{ExperimentHub objects and their related methods and functions}

\description{

  ## TODO: Explain differences between AnnotationHub and ExperimentHub

  Use \code{ExperimentHub} to interact with Bioconductor's ExperimentHub
  service.  Query the instance to discover and use resources that are of
  interest, and then easily download and import the resource into R for
  immediate use.

  Use \code{ExperimentHub()} to retrieve information about all records
  in the hub.

  Discover records in a hub using \code{mcols()}, \code{query()},
  \code{subset()}, \code{[}, and \code{display()}.

  Retrieve individual records using \code{[[}. On first use of a
  resource, the corresponding files or other hub resources are
  downloaded from the internet to a local cache. On this and all
  subsequent uses the files are quickly input from the cache into the R
  session.

  \code{ExperimentHub} records can be added (and sometimes removed) at
  any time. \code{snapshotDate()} restricts hub records to those
  available at the time of the snapshot; use \code{possibleDates()} to
  see possible snapshot dates.

  The location of the local cache can be found (and updated) with
  \code{getExperimentHubCache} and \code{setExperimentHubCache}; 
  \code{removeCache} removes all cache resources.
}

\section{Constructors}{
  \describe{
    \item{}{
      \code{ExperimentHub(..., hub=getExperimentHubOption("URL"), 
                         cache=getExperimentHubOption("CACHE"))}:
      Create an \code{ExperimentHub} instance, possibly updating the
      current database of records.
    }
  }
}

\section{Accessors}{
  In the code snippets below, \code{x} and \code{object} are
  ExperimentHub objects.

  \describe{
    \item{}{
      \code{hubCache(x)}:
      Gets the file system location of the local ExperimentHub cache.
    }
    \item{}{
      \code{hubUrl(x)}:
      Gets the URL for the online hub.
    }
    \item{}{
      \code{length(x)}:
      Get the number of hub records.
    }
    \item{}{
      \code{names(x)}:
      Get the names (ExperimentHub unique identifiers, of the form
      AH12345) of the hub records.
    }
    \item{}{
      \code{fileName(x)}:
      Get the file path of the hub records as stored in the local cache
      (ExperimentHub files are stored as unique numbers, of the form
      12345).  NA is returned for those records which have not been
      cached.
    }
    \item{}{
      \code{package(x)}:
      Get the package name associated with the hub resource.
    }
    \item{}{
      \code{mcols(x)}:
      Get the metadata columns describing each record. Columns include:
      \describe{

        \item{title}{Record title, frequently the file name of the
          object.}

        \item{dataprovider}{Original provider of the resource, e.g.,
          Ensembl, UCSC.}

        \item{species}{The species for which the record is most
          relevant, e.g., \sQuote{Homo sapiens}.}

        \item{taxonomyid}{NCBI taxonomy identifier of the species.}

        \item{genome}{Genome build relevant to the record, e.g., hg19.}

        \item{description}{Textual description of the resource,
          frequently automatically generated from file path and other
          information available when the record was created.}

        \item{tags}{Single words added to the record to facilitate
          identification, e.g,. TCGA, Roadmap.}

        \item{rdataclass}{The class of the R object used to represent
          the object when imported into R, e.g., \code{GRanges},
          \code{VCFFile}.}

        \item{sourceurl}{Original URL of the resource.}

        \item{sourectype}{Format of the original resource, e.g., BED
          file.}
      }
    }
  }
}

\section{Subsetting and related operations}{
  In the code snippets below, \code{x} is an ExperimentHub object.

  \describe{
    \item{}{
      \code{x$name}:
      Convenient reference to individual metadata columns, e.g.,
      \code{x$species}.
    }
    \item{}{
      \code{x[i]}:
      Numerical, logical, or character vector (of ExperimentHub names)
      to subset the hub, e.g., \code{x[x$species == "Homo sapiens"]}.
    }
    \item{}{
      \code{x[[i]]}:
      Numerical or character scalar to retrieve (if necessary) and
      import the resource into R.
    }
    \item{}{
      \code{query(x, pattern, ignore.case=TRUE, pattern.op= `&`)}:
      Return an ExperimentHub subset containing only those elements
      whose metadata matches \code{pattern}. Matching uses
      \code{pattern} as in \code{\link{grepl}} to search the
      \code{as.character} representation of each column, performing a
      logical \code{`&`} across columns.
      e.g., \code{query(x, c("Homo sapiens", "hg19", "GTF"))}.
      \describe{
        \item{\code{pattern}}{A character vector of patterns to search
          (via \code{grepl}) for in any of the \code{mcols()} columns.}
        \item{\code{ignore.case}}{A logical(1) vector indicating whether
          the search should ignore case (TRUE) or not (FALSE).}
        \item{\code{pattern.op}}{Any function of two arguments,
          describing how matches across pattern elements are to be
          combined. The default \code{`&`} requires that only records
          with \emph{all} elements of \code{pattern} in their metadata
          columns are returned.}
      }
    }
    \item{}{
      \code{subset(x, subset)}:
      Return the subset of records containing only those elements whose
      metadata satisfies the \emph{expression} in \code{subset}. The
      expression can reference columns of \code{mcols(x)}, and should
      return a logical vector of length \code{length(x)}.
      e.g., \code{subset(x, species == "Homo sapiens" &
        genome="GRCh38")}.
    }
    \item{}{
      \code{display(object)}:
      Open a web browser allowing for easy selection of hub records via
      interactive tabular display. Return value is the subset of hub
      records identified while navigating the display.
    }
  }
}

\section{Cache and hub management}{
  In the code snippets below, \code{x} is an ExperimentHub object.
  \describe{
    \item{}{
      \code{snapshotDate(x)} and \code{snapshotDate(x) <- value}:
      Gets or sets the date for the snapshot in use. \code{value} should
      be one of \code{possibleDates()}.
    }
    \item{}{
      \code{possibleDates(x)}:
      Lists dates for snapshots that the hub could potentially use.
    }
    \item{}{
      \code{cache(x)} and \code{cache(x) <- NULL}: Adds (downloads) all
      resources in \code{x}, or removes all local resources
      corresponding to the records in \code{x} from the cache. In this case,
      \code{x} would typically be a small subset of ExperimentHub resources.
    } 
    \item{}{
      \code{hubUrl(x)}:
      Gets the URL for the online ExperimentHub.
    }
    \item{}{
      \code{hubCache(x)}:
      Gets the file system location of the local ExperimentHub cache.
    }
    \item{}{
      \code{removeCache(x)}:
      Removes local ExperimentHub database and all related resources. After
      calling this function, the user will have to download any ExperimentHub
      resources again.
    }
    \item{}{
      \code{getExperimentHubOption()}:
      TODO: Get cache options "CACHE", "URL", "MAXDOWNLOADS" ... 
    }
    \item{}{
      \code{setExperimentHubOption()}:
      TODO: Set cache options "CACHE", "URL", "MAXDOWNLOADS" ... 
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{x} is an ExperimentHub object.
  \describe{
    \item{}{
      \code{as.list(x)}:
      Coerce x to a list of hub instances, one entry per
      element. Primarily for internal use.
    }
    \item{}{
      \code{c(x, ...)}:
      Concatenate one or more sub-hub. Sub-hubs must reference the same
      ExperimentHub instance. Duplicate entries are removed.
    }
  }
}

\author{Bioconductor Core Team}

\examples{
  ## TODO:
}

\keyword{classes}
\keyword{methods}
