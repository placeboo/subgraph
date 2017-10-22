#' K2 edges
#' List all possible k2 graphs based on given nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of K2 graphs

#' @examples
#' k2(c(1, 2, 4, 5))
#' k2(c(1, 4, 3, 5))

k2 = function(
        x # a vector representing nodes
        ){
        x = sort(x) # increasing order
        pair2 = as.matrix(combn(x,2))
        mat = as.matrix(paste(pair2[1,], "-", pair2[2,], sep = ""))
        return(mat)
}
