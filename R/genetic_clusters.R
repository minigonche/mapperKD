#' Biological Clusters
#' This module contains different cluster rutines designed to be used with genetic data.
#'
#' All clustering algorithms have to conform to the structure of the method explained in the file: general_clusters.R
#'

#' ----------------------------------
#' ---- Identity by Descend ---------
#' ----------------------------------

# get_genetic_ids_from_file
#' Transforms the given 1 esqueleton result into an igraph graph.
#' @param one_squeleton_result The one_squeleton to convert
#' @return A vector with the corresponding sizes
