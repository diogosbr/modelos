#' @title Limpas os registros de ocorrência
#' @name clean
#'
#' @description Uma função para retirar os pontos que estão fora da extensão do raster e manter, no máximo um registro por pixel.
#'
#' @param coord data.frame com as coordenadas
#' @param abio um objeto com os rasters a serem cortados. Aceita um objeto do tipo RasterStack, gerado pela função \code{\link[raster]{stack}}
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
#' predictors <- raster::stack(fnames)
#' clean(coord = manimax, abio = predictors)
#'
#' @import raster
#'
#' @export
clean = function(coord, abio) {
    if (dim(coord)[2] == 2) {
        if (exists("abio")) {
            # selecionar os pontos únicos e sem NA
            mask = abio[[1]]
            # Selecionar pontos espacialmente únicos #
            cell <- raster::cellFromXY(mask, coord)  # get the cell number for each point
            dup <- duplicated(cell)
            pts1 <- coord[!dup, ]  # select the records that are not duplicated
            pts1 <- pts1[!is.na(extract(mask, pts1)), ]  #selecionando apenas pontos que tem valor de raster

            cat(dim(coord)[1] - dim(pts1)[1], "pontos retirados\n")
            cat(dim(pts1)[1], "pontos espacialmente únicos\n")
            names(pts1) = c("lon", "lat")#
            return(pts1)
        } else (cat("Indique o objeto com as variáveis preditoras"))
    } else (stop("Tabela de coordenadas tem mais de duas colunas.\nEsta tabela deve ter apenas longitude e latitude, nesta ordem."))
}
