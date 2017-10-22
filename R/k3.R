#' K3 graphs
#' List all possible k3 graphs based on given nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of k3 graphs

#' @examples
#' k3(c(1, 2, 4, 5))
#' k3(c(1, 4, 3, 5))
k3 = function(
        x # nodes, vector
        ){
        x = sort(x)
        pair3 = as.matrix(combn(x, 3))
        mat = apply(pair3, 2, function(y) c(deToIn(y[1], y[2]), deToIn(y[2], y[3]), deToIn(y[1], y[3])))
        return(t(mat))
}


