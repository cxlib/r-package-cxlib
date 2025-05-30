% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cxlib_remote.R
\name{cxlib_remote}
\alias{cxlib_remote}
\title{(Experimental) Utility function to run one or more programs in a remote queue}
\usage{
cxlib_remote(x, label = NULL, options = NULL, queue = NULL)
}
\arguments{
\item{x}{Vector of programs}

\item{label}{Job reference label}

\item{options}{Job options}

\item{queue}{URL to R Compute Service}
}
\value{
A Job ID with attributes
}
\description{
The utility function submits a vector of programs to run remotely.

The \code{label} is a simple reference label that can be used to identify a job.

\code{options} can be used to override default options defined through cxlib
properties (see \link[cxlib]{cxlib_batch}).

The returned object is a Job ID with connection attributes.
}
