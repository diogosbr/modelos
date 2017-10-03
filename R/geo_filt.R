#' @title Filtro geográfico
#' @name geo.filt
#'
#' @description Uma funcao para geral modelos de nicho ecológico.
#'
#' @param coord data.frame. Tabela com os dados de oco?ncia da espécie. Deve conter apenas duas colunas: long e lat, nesta ordem.
#' @param res numerico. distância mínima (em Km) entre os pontos.
#' 
#' @details Nada por enquanto
#'
#' @return tabela de pontos de ocorrência com distância mínima indicada em res
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
  cat(paste0(dim(pts)[1], ' pontos restantes apos o filtro geografico de ', resolution , "km", '\n'))
  #aa$geo.filt=dim(pts)[1]
  return(pts)
}

