% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neighbor.R
\name{neighbor}
\alias{neighbor}
\title{Neighbor
For specific node in one graph, list all the nodes whcih are connected to the node.}
\usage{
neighbor(node, graph)
}
\arguments{
\item{node}{The specific node in the graph, representing as a numeric.}

\item{graph}{The graph, representing as a vector. Each entry in the vector is writen such as "1-2", "6-2".}
}
\value{
A numeric vector listing all the nodes which are connected to the nodex x in graph y.
}
\description{
Neighbor
For specific node in one graph, list all the nodes whcih are connected to the node.
}
\examples{
graph = c6(1:6)[1, ]
neighbor(2, graph)
}
