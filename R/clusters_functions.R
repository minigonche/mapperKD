#' Available clustering methods
#'
#' The following script contains the different clustering schemes available to pass as the parameter: \code{clustering_method} to the
#' main method. All the available functions should have the following structure (as well as any other function that is used as the mentioned parameter):
#' Function Structure:
#'    Parameters:
#'       distance_matrix: square symetric matrix with the distance between elements
#'    Return:
#'       numeric array: numeric array with the corresponding number of cluster each point belongs to (in the order of the distace matrix). Should start with 1 and have no gaps.
#'

# For Testing
cluster_all = function(distance_matrix)
{
  return(rep(1, dim(distance_matrix)[1]))
}

cluster_none = function(distance_matrix)
{
  return(1:dim(distance_matrix)[1])
}

