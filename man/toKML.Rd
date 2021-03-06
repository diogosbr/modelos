% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toKML.R
\name{toKML}
\alias{toKML}
\title{Transforms the raster into a KML file}
\usage{
toKML(modelo, name = "meuKML", zeros = FALSE, open = FALSE)
}
\arguments{
\item{modelo}{raster with the distribution model}

\item{name}{name of the KML file to be generated.}

\item{zeros}{logical. If FALSE, the zeros of the model are convergent in NA.}

\item{open}{logical. If TURE, open the KML file in Google Earth.}
}
\value{
data.frame containing longitude and latitude.
}
\description{
A function to transform the niche model from raster format to KML file.
}
\details{
This function is a wrapper for \code{\link[plotKML]{plotKML}} the function from the roxygen2 package. See the documentation and vignettes of that package to learn how to use roxygen.
}
\examples{
fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE )
predictors <- stack(fnames)
toKML(modelo = predictors, name = "exemplo")


}
\seealso{
\code{\link[plotKML]{plotKML}}
}
\author{
Diogo S. B. Rocha
}
