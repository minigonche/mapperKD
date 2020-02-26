#' Available generalclustering methods
#'
#' The following script contains the different clustering schemes available to pass as the parameter: \code{clustering_method} to the
#' main method. All the available functions should have the following structure (as well as any other function that is used as the mentioned parameter):
#'
#' NOTE: The clustering algorithms are used under two different schemes:
#'    - local_distance = FALSE. When local distance is FALSE, the clustering algorithm preforms over the distance matrix of the complete dataset
#'    - local_distance = TRUE. The clustering algorithm preforms only over the given indices, and not the complete data set. Therefore, the method should compute the clusters receving the indices as its only parameter.
#'
#' Function Structure:
#'    Parameters:
#'       distance_matrix: square symetric matrix with the distance between elements
#'       current_indices: a vector with the current indices of the data to preform clustering.
#'    Return:
#'       numeric array: numeric array with the corresponding number of cluster each point belongs to (in the order of the distace matrix). Should start with 1 and have no gaps.
#'

# Functions for Testing
# Clusters all point into a single cluster
cluster_all = function(distance_matrix)
{
  if(length(distance_matrix) == 1)
    times = 1
  else
    times = dim(distance_matrix)[1]

  return(rep(1, times))
}

# Creates a cluster for every point received
cluster_none = function(distance_matrix)
{
  if(length(distance_matrix) == 1)
    times = 1
  else
    times = dim(distance_matrix)[1]

  return(1:times)
}


# Hierarchical Clustering
#' Preforms Hierarchical Clustering
#' @param distance_matrix the distance matrix to be used.
#' @param method The method to be used to merge the clusters. See the documentation of hclust for details. Deafult parameter is: complete
#' @param height The height at wich the tree will be cut and the clusters generated. If a negative number id provided,
#' the tree will be cut at the mean of the upper triangular part of the provided distance matrix.
hierarchical_clustering = function(distance_matrix, method = 'single', height = -1 )
{

  # Assings the mean of the distance as height if no height is given
  if(height < 0)
    height = mean(upper.tri(distance_matrix))

  # Cuts the tree
  clusters = cutree(hclust(as.dist(distance_matrix), method = method), h = height)

  return(clusters)


}
