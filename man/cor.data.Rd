% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor_data.R
\name{cor.data}
\alias{cor.data}
\title{Exibe um gráfico com as correlações entre as variáveis}
\usage{
cor.data(abio, plot = TRUE, method = "pearson", rep = 1000)
}
\arguments{
\item{abio}{os rasters. Objeto do tipo _stack_}

\item{plot}{l?gico. Plota um dos rasters cortados.}

\item{method}{a character string indicating which correlation coefficient (or covariance) is to be computed: "pearson" (default), "kendall", or "spearman".}

\item{rep}{n?merico. N?mero de pontos gerados para extrair os valores dos raters e utilizar na correla??o. O padr?o ? 1000.}
}
\value{
Retorna uma tabela com os valores de correla??o entre as vari?veis.
}
\description{
Uma funcao para exibir um gráfico com as correla??es entre as vari?veis ambientais informadas.
}
\details{
O ?ndice de correla??o utilizado ? spearman
}
\examples{
fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''), pattern='grd', full.names=TRUE )
predictors <- stack(fnames)
cor.data(abio = predictors)

}
\seealso{
aaa
}
\author{
Diogo S. B. Rocha
}