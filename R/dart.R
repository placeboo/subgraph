#' dart graphs
#' List all possible dart graphs based on given Five nodes.

#' @param x The vector representing nodes

#' @return A matrix listing edges of dart graphs

#' @examples
#' dart(1:5)
dart = function(x){
        if(length(x) != 5){
                stop('The number of nodes should be FIVE!')
        }
        co_dart.mat = co.dart(x)
        dart.prob.mat = 1 - fillprob(x, co_dart.mat)
        dart.mat = probToEdges(dart.prob.mat)
        return(dart.mat)
}
