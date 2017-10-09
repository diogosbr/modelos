#' @title Download the file maxen.jar and put it in the folder of the dismo
#' @name i.mx
#'
#' @description A function to automate the download of maxent.jar file
#'
#' @param no_params No arguments
#' 
#' @details Just download the maxent.jar file from the site: \link[http://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download ] and places in the package folder dismo
#'
#' @return Maxent.jar files in the drive folder.
#'
#' @author Diogo S. B. Rocha
#'
#' @seealso \code{\link[dismo]{bioclim}}, \code{\link[raster]{mask}}
#'
#' @examples
#' i.mx()
#' 
#' @export
i.mx = function(){
  jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
  if (file.exists(jar) != T) {
    url = "http://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download"
    download.file(url, dest = "maxent.zip", mode = "wb")
    unzip("maxent.zip", files = "maxent.jar", exdir = system.file("java", package = "dismo"))
    unlink("maxent.zip")
    warning("Maxent has been placed in the directory\n")
  }
  cat("Maxent.jar OK!")
} 
  