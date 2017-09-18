#' @title Limpas os registros de ocorrência
#' @name clean
#'
#' @description Uma funcao para retirar os pontos que estão fora da extens?o do raster e manter, no máximo um registro por pixel.
#'
#' @param coord data.frame com as coordenadas
#' @param abio os rasters a serem cortados. Aceita um objeto do tipo _stack_
#'
#' @details Esta função é usada internamente na função \code{\link[modelos]{modelos}}.
#'
#' @return data.frame contendo longitude e latitude.
#'
#' @author Diogo S. B. Rocha
#'
#' @seealso \code{\link[raster]{cellFromXY}}, \code{\link[raster]{mask}}, \code{\link[raster]{extract}}
#'
#' @examples
#' fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE )
#' predictors <- stack(fnames)
#' clean(coord = manimax, abio = predictors)
#'
#' @import raster
#'
#' @export
clean = function(coord, abio) {
    if (dim(coord)[2] == 2) {
        if (exists("abio")) {
            # selecionar os pontos ?nicos e sem NA
            pts = coord
            mask = abio[[1]]
            # Selecionar pontos espacialmente únicos #
            cell <- raster::cellFromXY(mask, pts)  # get the cell number for each point
            dup <- duplicated(cell)
            pts1 <- pts[!dup, ]  # select the records that are not duplicated
            pts1 <- pts1[!is.na(extract(mask, pts1)), ]  #selecionando apenas pontos que tem valor de raster

            cat(dim(pts)[1] - dim(pts1)[1], "pontos retirados\n")
            cat(dim(pts1)[1], "pontos espacialmente únicos\n")
            # pts1
            names(pts1) = c("lon", "lat")#
            return(pts1)
        } else (cat("Indique o objeto com as vari?áveis preditoras"))
    } else (stop("Tabela de coordenadas tem mais de duas colunas.\nEsta tabela deve ter apenas longitude e latitude, nesta ordem."))
}
