#' @title Transforms the raster into a KML file
#' @name toKML
#'
#' @description A function to transform the niche model from raster format to KML file.
#'
#' @param modelo raster with the distribution model
#' @param name name of the KML file to be generated.
#' @param zeros logical. If FALSE, the zeros of the model are convergent in NA.
#' @param open logical. If TURE, open the KML file in Google Earth.
#'
#' @details This function is a wrapper for \code{\link[plotKML]{plotKML}} the function from the roxygen2 package. See the documentation and vignettes of that package to learn how to use roxygen.
#'
#' @return data.frame containing longitude and latitude.
#'
#' @author Diogo S. B. Rocha
#'
#' @seealso \code{\link[plotKML]{plotKML}}
#'
#' @examples
#' fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE )
#' predictors <- stack(fnames)
#' toKML(modelo = predictors, name = "exemplo")
#'
#'
#' @export
toKML = function(modelo, name = "meuKML", zeros = FALSE, open = FALSE){
  if(missing(modelo)){stop("Enter the name of the object that contains the template raster")}
  if(zeros==FALSE){
    values(modelo)[values(modelo)==0]=NA
    plotKML::plotKML(modelo,folder.name=name,open.kml = open)
  }else(plotKML::plotKML(modelo,folder.name=name,open.kml = open))
}
