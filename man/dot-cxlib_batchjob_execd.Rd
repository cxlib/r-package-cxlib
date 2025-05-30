% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal-cxlib_batchjob_execd.R
\name{.cxlib_batchjob_execd}
\alias{.cxlib_batchjob_execd}
\title{Internal function to execute actions of a batch job}
\usage{
.cxlib_batchjob_execd(x, work = NULL, archive.results = FALSE)
}
\arguments{
\item{x}{Directory path to job actions}

\item{work}{Work area directory}

\item{archive.results}{Generate results archive}
}
\value{
Invisible NULL
}
\description{
The internal routine is in effect a runner deamon that can be executed as
either a foreground process (wait for the routine to end) or a background
process (do not wait for it to end).

The routine uses the \verb{*-action-*} files in the directory specified by \code{x}.
The action file is a JSON data file with action attributes and directives.

The natural sort order of the action files is the action queue. The queue of
actions are processed one at a time sequentially , e.g. \verb{001-action-*} is
processed before \verb{002-action-*}.

A file that ends \verb{*.json} is an action that is queued and not started or
completed.

A file that ends \verb{*.lck} is an action that is currently being executed and
not completed.

A file that ends \verb{*-completed.json} is an action that is completed.

The routine is persistent and will continue to execute until all actions have
been completed.

Signal interrupts are currently not supported.

The option \code{archive.results} equal to \code{TRUE} will archive the job results
after the last task is completed. The results archive \verb{job-<job id>-results.zip}
will be created and saved in the internal job directory and the job work area
will be automatically deleted.
}
\keyword{internal}
