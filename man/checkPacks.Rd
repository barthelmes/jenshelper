% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jenshelper.R
\name{checkPacks}
\alias{checkPacks}
\title{checkPacks}
\usage{
checkPacks(path)
}
\arguments{
\item{path}{}
}
\value{
table with used packages (only direct!) and count of number of uses
}
\description{
Using NCmisc this fun extracts R code of all files under "path" and looks for all used funs and package they are from.
}
