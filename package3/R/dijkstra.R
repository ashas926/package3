#' Dijkstra Algorithm.
#'
#' The function is an implementation of algorithm for finding the shortest paths between nodes in a graph,
#' which may represent, for example, road networks.
#' 
#' @param graph A data frame consists of three variables.
#' @param init_node A numeric scalar variable.
#'
#' @return Returns the shortest path to every other node from the starting node as a vector.
#' @export
#' @references \href{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}{Dijkstra Algorithm}
#' 
#' @examples dijkstra(wiki_graph, 1)
#' 
dijkstra<- function(graph,init_node){
 
  data1<- as.vector(graph$v1)
  data2<- as.vector(graph$v2)
  data3<- as.vector(graph$w)
  queue<-unique(data1)
  
  mat<-matrix(c(queue,rep(NaN,length(queue)),rep(0,length(queue))),nrow=length(queue),ncol=3)
  mat[init_node,2]<-0
  
  while(length(queue)>0){
    val<-min(mat[queue,2],na.rm = TRUE)
    index<-which(mat[,2] %in% val)

#when two weights are equal and both vertexes are in queue, "dum" is used to pick one. 

    dum<-intersect(queue,index)  
    current_node<- dum[1]
    id<-which(data1 %in% current_node )
    neighbor<-data2[id]
    weight<- data3[id]
    for(i in 1:length(neighbor)){
      
      if((mat[neighbor[i],2]) >(mat[current_node,2]+weight[i]) || is.nan(mat[neighbor[i],2])) {
        mat[neighbor[i],2] <- mat[current_node,2]+weight[i]
        mat[neighbor[i],3]<-current_node
      }
    }
    queue<-queue[-(which(queue %in% current_node))]
  }
  
  
  mat[,2]
  
}
wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)

