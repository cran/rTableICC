\name{rTableICC-internal}

\alias{rTableICC.2x2xK.default}
\alias{rTableICC.RxC.default}
\alias{rTable.2x2xK.default}
\alias{rTable.RxC.default}
\alias{rtableICC.2x2xK.main}
\alias{rtableICC.RxC.main}
\alias{rtable.2x2xK.main}
\alias{rtable.RxC.main}
\alias{rtableICC.RxC.engine}
\alias{check}

\title{
Functions for internal use only
}
\description{
Contains functions desinged for internal use only. Functions including "default" in name makes basic checks using \code{check} function, and then call main function. Those including "main" in name carries on main processes to generate random tables. \code{rtableICC.RxC.engine} is the function multiply called by \code{rTable.RxC.main}.
}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}

\seealso{
\code{\link[rTableICC]{rTableICC.2x2xK}}, \code{\link[rTableICC]{rTableICC.RxC}}, \code{\link[rTableICC]{rTable.2x2xK}}, \code{\link[rTableICC]{rTable.RxC}}, \code{\link[rTableICC]{rDiscrete}}
}
