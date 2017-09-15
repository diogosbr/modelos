#' @title Exibe um gráfico com as correlações entre as variáveis
#' @name cor.data
#'
#' @description Uma funcao para exibir um gráfico com as correla??es entre as vari?veis ambientais informadas.
#'
#' @param abio os rasters. Objeto do tipo _stack_
#' @param plot l?gico. Plota um dos rasters cortados.
#' @param method a character string indicating which correlation coefficient (or covariance) is to be computed: "pearson" (default), "kendall", or "spearman".
#' @param rep n?merico. N?mero de pontos gerados para extrair os valores dos raters e utilizar na correla??o. O padr?o ? 1000.
#'
#' @details O ?ndice de correla??o utilizado ? spearman
#'
#' @return Retorna uma tabela com os valores de correla??o entre as vari?veis.
#'
#' @author Diogo S. B. Rocha
#'
#' @seealso aaa
#'
#' @examples
#' fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE )
#' predictors <- stack(fnames)
#' cor.data(abio = predictors)
#'
#' @import raster
#' @import dismo
#'
#' @export
cor.data = function(abio, plot = TRUE, method = "pearson", rep = 1000) {

    if(class(abio)=="RasterStack"|class(abio)=="RasterLayer"){
      backg <- randomPoints(abio, n = rep)
      colnames(backg) = c("long", "lat")
      backvalues = extract(abio, backg)
    }else(backvalues=abio)


    if (plot == T) {
        panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
            usr <- par("usr")
            on.exit(par(usr))
            par(usr = c(0, 1, 0, 1))
            r <- abs(cor(x, y, method = method))
            txt <- format(c(r, 0.123456789), digits = digits)[1]
            txt <- paste0(prefix, txt)
            if (missing(cex.cor))
                cex.cor <- 0.8/strwidth(txt)
            text(0.5, 0.5, txt, cex = cex.cor * r)
        }

        panel.hist <- function(x, ...){
            usr <- par("usr"); on.exit(par(usr))
            par(usr = c(usr[1:2], 0, 1.5) )
            h <- hist(x, plot = FALSE)
            breaks <- h$breaks; nB <- length(breaks)
            y <- h$counts; y <- y/max(y)
            rect(breaks[-nB], 0, breaks[-1], y, col = "gray", ...)
        }

        pairs(backvalues, lower.panel = panel.smooth, diag.panel= panel.hist, upper.panel = panel.cor)
    }
    return(round(cor(backvalues, method = method), 2))
}
