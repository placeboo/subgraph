#' k14 graphs
#' List all possible k14 graphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of k14 graphs

#' @examples
#' k14(c(1:5))
k14 = function(x){
        mtx = matrix(NA, ncol = 4)
        if(length(x) !=5 ){
                stop("The number of nodes should be FIVE!")
        }else{
                for(i in 1:length(x)){ # pick one node as the "hub", which connect to the rest
                        temp_hub = x[i]
                        temp_rest = x[-i]
                        temp_edge = c(deToIn(temp_hub, temp_rest[1]), deToIn(temp_hub, temp_rest[2]), deToIn(temp_hub, temp_rest[3]), deToIn(temp_hub, temp_rest[4]))
                        mtx = rbind(mtx, temp_edge)
                }
                return(mtx[-1, ])
        }

}
