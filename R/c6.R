#' c6 graphs
#' List all possible R graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of c6 graphs

#' @examples
#' c6(c(1:6))

c6 = function(x){
        if(length(x)!=6){
                stop("The number of nodes should be SIX!")
        }
        # start from the first node in the x
        start_node = x[1]
        # permulate, using p5 function
        temp_p5 = p5(x[-1])
        # find out the nodes, which start_node will connect to
        # frequency of node in each column in temp_p5 is one, should be the nodes
        temp_mtx = matrix(NA, ncol = 2)
        for(n in 1: nrow(temp_p5)){
                temp_tab = as.matrix(table(unlist(strsplit(temp_p5[n,], split = "-"))))
                twonodes = as.numeric(rownames(temp_tab)[which(temp_tab[, 1] == 1)])
                temp_mtx = rbind(temp_mtx, c(deToIn(1, twonodes[1]), deToIn(1, twonodes[2])))
        }
        temp_mtx = temp_mtx[-1, ]
        return(cbind(temp_p5, temp_mtx))
}
