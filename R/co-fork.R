#' co-fork graphs
#' List all possible co-fork graphs based on given Five nodes. Note that co-fork is the complement graphs of fork based on given five nodes.

#' @param x The vector representing nodes

#' @return A matrix listing edges of co-fork graphs

#' @examples
#' co-fork(c(1:5))

co.fork = function(x){
        if(length(x)!=5){
                stop("The number of nodes should be FIVE!")
        }
        x = sort(x)
        fork.mat = chair(x)
        co_fork_prob.mat = 1 - fillprob(x, fork.mat) # co_fork is the complement of fork
        return(probToEdges(co_fork_prob.mat))
}
