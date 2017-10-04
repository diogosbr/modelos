#' @title Geographic filter
#' @name geo.filt
#'
#' @description A function to generate ecological niche models.
#'
#' @param coord data.frame. Table with the species' data. It should contain only two columns: long and lat, in that order.
#' @param res numeric. Minimum distance (in Km) between points.
#' 
#' @details Nothing for now.
#'
#' @return Table of occurrence points with minimum distance indicated in res
#'
#' @author Diogo S. B. Rocha
#'
#' @seealso \code{\link[dismo]{gridSample}}
#'
#' @examples
#' geo.filt(pts=manimax,res=20)
#'
#' @importFrom dismo gridSample
#' @export
geo.filt = function(coord, resolution = 10){
  res = resolution/111
  r=raster(extent(range(coord[,1]), range(coord[,2])) + res)
  res(r)=res
  pts=gridSample(coord,r, n=1)
  cat(paste0(dim(pts)[1], ' points remaining after the geographic filter of ', resolution , "km", '\n'))
  #aa$geo.filt=dim(pts)[1]
  return(pts)
}

