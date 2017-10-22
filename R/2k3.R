#' 2k3 graphs
#' List all possible R graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of 2k3 graphs

#' @examples
#' twoK3(c(1:6))

twoK3 = function(x){
        if(length(x) != 6){
                stop('The number of nodes should be SIX!')
        }
        x = sort(x)
        pair3 = combn(x, 3)
        twoK3 = matrix(, , ncol = 6)
        for(i in 1: (ncol(pair3)/2)){
                # left + right = x
                left = pair3[, i]
                c3.left = k3(left)
                right = pair3[, 21 - i]
                c3.right = k3(right)
                twoK3 = rbind(twoK3, c(c3.left, c3.right))
        }
        return(twoK3[-1, ])
}
