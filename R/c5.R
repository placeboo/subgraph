#' c5 graphs
#' List all possible c5 graphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of c5 graphs

#' @examples
#' c5(c(1:5))

c5 = function(x){
        if(length(x)!=5){
                stop("The number of nodes should be FIVE!")
        }
        x = sort(x)
        # start from the first node in the x
        start_node = x[1]
        rest = x[-1]
        # permulate, using p4 function
        p4.mat = p4(rest)

        # find the nodes which are connected twice
        temp.mat = apply(p4.mat, 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))
        # the number appear twice is the hub
        # connect the hub
        hub.mat = apply(temp.mat, 2, function(y) rest[table(y) == 1])
        # connec the start node to the hub
        cnct.mat = t(apply(hub.mat, 2, function(z) c(deToIn(z[1], start_node), deToIn(z[2], start_node))))
        return(cbind(p4.mat, cnct.mat))
}
