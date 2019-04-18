#' c4 graphs

#' List all possible c4 graphs based on given FOUR nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of c4 graphs

#' @examples
#' c4(c(1:4))

c4 = function(x){
        x = sort(x)
        if(length(x) != 4){
                stop("The number of nodes should be FOUR.")
        }else{
                num_node = length(x)
                # complete graph, removing non-overlap edge (do not share a same node)
                pair2 = combn(x, 2)
                # observe pair2 column (1, 6) (2, 5) (3, 4) dont share the same node
                mtx = rbind(paste(pair2[1, -c(1, 6)], '-', pair2[2, -c(1, 6)], sep = ""), paste(pair2[1, -c(2, 5)], '-', pair2[2, -c(2, 5)], sep = ""), paste(pair2[1, -c(3, 4)], '-', pair2[2, -c(3, 4)], sep = ""))
                return(mtx)
        }

}
