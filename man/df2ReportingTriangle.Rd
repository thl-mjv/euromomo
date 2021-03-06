% Generated by roxygen2 (4.0.2): do not edit by hand
\name{df2ReportingTriangle}
\alias{df2ReportingTriangle}
\title{Extract reporting triangle from \code{data.frame}}
\usage{
df2ReportingTriangle(momo, groupindicator, back, dWeeks, dLastFullWeek)
}
\arguments{
\item{momo}{is a data.frame with the complete data
back is the number of weeks for delay-adjustment age group specific
groupindicator is the indicator of the age group for data aggregation}

\item{groupindicator}{A vector of Booleans of the same length as \code{momo}}

\item{back}{Number of weeks to go back in the delay distribution, i.e. we look at delays \code{0:back} .}

\item{dWeeks}{Vector of Dates containing all Mondays of the ISO weeks to triangulate.}

\item{dLastFullWeek}{Data object containing the Monday of the last full week before day of aggregation. Note: dLastFullWeek = max(dWeeks)}
}
\value{
A list containing the reporting triangle for each age group, the cumulated reporting triangle, the time points and the delays.
}
\description{
R function for reading in and performing the aggregation necessary
to get the reporting triangle.
}

