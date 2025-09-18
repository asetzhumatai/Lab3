#' Euclidean Algorithm
#'
#' Calculates the greatest common divisor (GCD) of two positive integers
#' using the Euclidean algorithm.
#'
#' @param a A positive numeric scalar.
#' @param b A positive numeric scalar.
#'
#' @return A single numeric value representing the greatest common divisor of \code{a} and \code{b}.
#'
#' @references Euclidean algorithm \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @export
#'
#' @examples
#' euclidean(100, 1000)
#' euclidean(123612, 13892347912)

euclidean <- function(a, b) {
  stopifnot(is.numeric(a) && is.numeric(b))
  while (a != b){
    if (a > b){
      a <- a-b
    }
    else {
      b <- b-a
    }
  }
  return(a)
}



#' Dijkstra's algorithm
#'
#' Algorithm takes \code{graph} and \code{init_node} and calculates the shortest path from the initial node to every other node in the graph
#'
#' @param graph Must be a \code{data.frame} object with three variables (\code{v1}, \code{v2} and \code{w}) that
#'      contains the edges of the \code{graph} (from \code{v1} to \code{v2}) with the weight of the edge (\code{w}).
#' @param init_node 'Initial node' must be a numeric scalar that exists in the graph description
#'
#' @return Vector with the distances from \code{init_node} to every other node in the graph
#'
#' @references Dijkstra's algorithm \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' wiki_graph <- data.frame(
#'   v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'   v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'   w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#'
#' dijkstra(wiki_graph, 1)

dijkstra <- function(graph, init_node) {

  stopifnot((is.data.frame(graph) && is.numeric(init_node)))
  stopifnot((c("v1", "v2", "w") %in% names(graph)))

  vertices <- unique(graph$v1)
  dist <- setNames(rep(Inf, length(vertices)), vertices)
  dist[as.character(init_node)] <- 0
  Q <- vertices

  while (length(Q) > 0) {
    u <- Q[which.min(dist[as.character(Q)])]
    Q <- setdiff(Q, u)
    neighbors <- graph[graph$v1 == u | graph$v2 == u, ]
    for (i in seq_len(nrow(neighbors))) {
      v <- if (neighbors$v1[i] == u) neighbors$v2[i] else neighbors$v1[i]
      if (v %in% Q) {
        alt <- dist[as.character(u)] + neighbors$w[i]
        if (alt < dist[as.character(v)]) {
          dist[as.character(v)] <- alt
        }
      }
    }
  }
  return(as.numeric(dist))
}


wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))


#' Example Graph Dataset for Dijkstra's Algorithm
#'
#' This dataset represents a weighted undirected graph with 6 nodes and edges with weights.
#' It can be used to test the Dijkstra function in this package.
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{v1}{Numeric: start node of the edge}
#'   \item{v2}{Numeric: end node of the edge}
#'   \item{w}{Numeric: weight of the edge}
#' }
#'
#' @references Wikipedia page for graphs: \url{https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)}
"wiki_graph"
