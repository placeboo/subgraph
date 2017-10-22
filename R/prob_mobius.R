#' Mobius parameters
#' Base on the binary data probability space, all the Mobius parameters, or marginal probability are represented.
#' @param prob_space The matrix with 0 and 1 entries to list all possible binary data

#' @return Returns a matrix
#' @examples
#'
#' prob_mobius(prob_space.mat)


prob_mobius = function(prob_space){ # input: matrix
        colname = unique(rownames(prob_space))
        rowname = rownames(prob_space)
        prob_mobius = matrix(0, nrow = length(rowname), ncol = length(colname))
        colnames(prob_mobius) = colname
        rownames(prob_mobius) = rowname
        iso_num.tab = as.matrix(as.matrix(table(rowname))[colname,])
        edge_num.tab = as.matrix(apply(prob_space[which(!duplicated(rowname)), ], 1, sum))
        fullgraph_edge = max(edge_num.tab[, 1])
        for(mobius in colname){ # go though each mobius
                # the number of the mobius
                temp_iso_num = iso_num.tab[mobius, ]
                temp_edge_num = edge_num.tab[mobius, ]
                # find supergraphs

                if(temp_edge_num == 0){ # empty graph
                        prob_mobius[mobius, ] =  (-1)^edge_num.tab[colname,1] * iso_num.tab[, 1]
                }else if(temp_edge_num == fullgraph_edge){ # full graph
                        prob_mobius[mobius, mobius] = 1
                }else{ # the rest
                        # pick one from iso group, and mark the edge
                        edge_index = which(prob_space[mobius, ] == 1)
                        # find all its supergraphs and its number
                        temp_supergraph.tab = as.matrix(table(names(which(apply(as.matrix(prob_space[, edge_index]), 1, function(x) all(x == rep(1, temp_edge_num)))))))
                        # name of supergraph
                        temp_supergraph = rownames(temp_supergraph.tab)
                        temp_mobius = (-1)^(edge_num.tab[temp_supergraph, ] - temp_edge_num) * temp_supergraph.tab[, 1]
                        # fill the mobius
                        prob_mobius[which(rowname == mobius), temp_supergraph] = matrix(rep(temp_mobius, temp_iso_num), ncol = length(temp_supergraph), byrow = T)
                }

        }
        return(prob_mobius)
}
