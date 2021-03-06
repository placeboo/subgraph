% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/m2g.R
\name{m2g}
\alias{m2g}
\title{mobius form to graphs
Base on the form of linear combination of mobius parameters, list its corresponding graphs}
\usage{
m2g(mobius.mat)
}
\arguments{
\item{mobius.mat}{Matrix, each row is the linear combination of mobius parameters, representing an unknow graph. the column names of mobius.mat should be from graphTable.}
}
\value{
A vector reprents the graphs corresponding with each each row of mobius.mat
}
\description{
mobius form to graphs
Base on the form of linear combination of mobius parameters, list its corresponding graphs
}
\examples{
three_extrem = rbind(c(1, 0, 2/3, 1/3), c(1, 0, 0, 0), c(1, 0, 1/3, 0), c(1, 1, 1, 1))
colnames(three_extrem) = c("3k1", "k3", "p3.bar", "p3")
m2g(three_extrem)
}
