#' claw_k1.bar graphs
#' List all possible claw_k1.bar graphs based on given nodes nodes. Claw_k1.bar is the complement graph of claw_k1 base on given five nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of claw claw_k1.bar graphs

#' @examples
#' claw_k1.bar(c(1:5))


claw_k1.bar = function(x){
        if(length(x)!=5){
                stop('The number of nodes should be FIVE!')
        }
        claw_k1.mat = claw(x)
        claw_k1.bar.prob =  1 - fillprob(x, claw_k1.mat)
        claw_k1.bar.mat = probToEdges(claw_k1.bar.prob)
        return(claw_k1.bar.mat)
}
