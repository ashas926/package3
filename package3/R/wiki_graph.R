#' Wiki_Graph
#'
#' Wiki_Graph is a graph which consists of several vertices. Here its a made of a dataframe.
#' This dataframe consists of three variables (v1, v2 and w) that contains the edges of the graph (fromv1 to v2) with the weight of the edge (w).
#'
#' @docType data
#'
#' @usage data(wiki_graph)
#'
#' @format An object
#'
#' @keywords datasets
#'
#' @references \href{http://www.r-tutor.com/r-introduction/data-frame}{R tutorial}
#'
#' @source From the lab manual
#' 
#' @examples dijkstra(graph = wiki_graph, init_node = 1)
"wiki_graph"