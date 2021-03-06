% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor_data.R
\name{cor.data}
\alias{cor.data}
\title{Displays a graph with the correlations between the variables}
\usage{
cor.data(abio, plot = TRUE, method = "pearson", rep = 1000)
}
\arguments{
\item{abio}{the rasters. Object of type RasterStack, generated by the function \code{\link[raster]{stack}}}

\item{plot}{logic. Plots one of the cut rasters.}

\item{method}{a character string indicating which correlation coefficient (or covariance) is to be computed: "pearson" (default), "kendall", or "spearman".}

\item{rep}{numeric. Number of points generated to extract rater values and use in correlation. The default is 1000.}
}
\value{
Returns a table with the correlation values between the variables.
}
\description{
A function to display a graph with the correlations between the reported environmental variables.
}
\details{
Graph indicating the correlations
}
\examples{
fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE )
predictors <- raster::stack(fnames)
cor.data(abio = predictors)

}
\seealso{
\code{\link[graphics]{pairs}}
}
\author{
Diogo S. B. Rocha
}
