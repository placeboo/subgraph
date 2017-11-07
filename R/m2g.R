#' mobius form to graphs
#' Base on the form of linear combination of mobius parameters, list its corresponding graphs
#' @param mobius.mat Matrix, each row is the linear combination of mobius parameters, representing an unknow graph. the column names of mobius.mat should be from graphTable.
#' @return A vector reprents the graphs corresponding with each each row of mobius.mat

#' @examples
#' three_extrem = rbind(c(1, 0, 2/3, 1/3), c(1, 0, 0, 0), c(1, 0, 1/3, 0), c(1, 1, 1, 1))
#' colnames(three_extrem) = c("3k1", "k3", "p3.bar", "p3")
#' m2g(three_extrem)


m2g = function(mobius.mat){
        if(!is.matrix(mobius.mat)){
                stop("The input should be matrix")
        }
        data("graphTable")
        graphTable = as.data.frame(t(graphTable))
        if(any(is.na(match(colnames(mobius.mat), graphTable$name)))){
                stop("The column name should be from graphTable")
        }
        mobius_name = colnames(mobius.mat)
        mobius_rowname = rep(0, nrow(mobius.mat))
        for(nn in 1: nrow(mobius.mat)){
                extre_temp = mobius.mat[nn, ]
                # find the groph with the most edges
                nonzeo_subgraph = mobius_name[extre_temp != 0]
                nonzeo_subgraph_edges = graphTable$edge[match(nonzeo_subgraph,graphTable$name)]
                mobius_rowname[nn] = nonzeo_subgraph[which.max(nonzeo_subgraph_edges)]
        }
        return(mobius_rowname)
}
