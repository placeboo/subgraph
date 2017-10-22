#' From probability to edges
#'
#' @param x The matrix, whose colomn names represents the edges, is written as 1 and 0, where 1 means there is edge.
#' @return A matrix, each rows represent edges of graphs

#' @examples
#' mat = fillprob(N=4, c4(c(1:4)))
#' probToEdges(mat)

probToEdges = function(x){
        edges = colnames(x)
        return(t(apply(x, 1, function(y) edges[y == 1])))
}
