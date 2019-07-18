\name{getExperimentHubOption}

\alias{getExperimentHubOption}
\alias{setExperimentHubOption}

\title{Get and set options for default ExperimentHub behavior.}

\description{
  These functions get or set options for creation of new
  \sQuote{ExperimentHub} instances.
}

\usage{
getExperimentHubOption(arg)
setExperimentHubOption(arg, value)
}

\arguments{
  \item{arg}{The character(1) hub options to set. see \sQuote{Details}
    for current options.}
  \item{value}{The value to be assigned to the hub option.}
}

\details{
  Supported options include:

  \describe{

    \item{\dQuote{URL}:}{character(1). The base URL of the Experiment Hub. 
      Default: \url{https://experimenthub.bioconductor.org}}

    \item{\dQuote{CACHE}:}{character(1). The location of the hub
      cache. Default: \dQuote{.ExperimentHub} in the user home
      directory.}

    \item{\dQuote{MAX_DOWNLOADS}:}{numeric(1). The integer number of
      downloads allowed before triggering an error. This is to help
      avoid accidental download of a large number of ExperimentHub
      members.}

    \item{\dQuote{PROXY}:}{\code{request} object returned by
      \code{httr::use_proxy()}. The \code{request} object describes a proxy
      connection allowing Internet access, usually through a restrictive
      firewall. Setting this option sends all ExperimentHub requests through
      the proxy. Default: NULL.

      In \code{setHubOption("PROXY", value)}, \code{value} can be one of NULL,
      a \code{request} object returned by \code{httr::use_proxy()}, or a
      well-formed URL as character(1). The URL can be completely
      specified by \code{http://username:password@proxy.dom.com:8080};
      \code{username:password} and port (e.g. \code{:8080}) are optional.}

    \item{\dQuote{LOCAL}:}{logical(1). TRUE/FALSE should the
      ExperimentHub create a hub consisting only of previously
      downloaded resourcesd. Default: FALSE.}
  }

  Default values may also be determined by system and global R
  environment variables visible \emph{before} the package is loaded. Use
  options or variables preceded by \dQuote{EXPERIMENT_HUB_}, e.g.,
  \code{options(EXPERIMENT_HUB_MAX_DOWNLOADS=10)} prior to package load
  sets the default number of downloads to 10. 

}

\value{The requested or successfully set option.}

\author{Bioconductor Core Team}

\examples{
getExperimentHubOption("URL")
\dontrun{
setExperimentHubOption("CACHE", "~/.myHub")
}
}
\keyword{ manip }
