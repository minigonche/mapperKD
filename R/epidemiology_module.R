# Epidemiology Module
# This module contains the different functions and procedures to recreate the results from the publication: MISSING_CITATION.



# extract_intersection_centrality
#' Extracts the intersection centrality described in MISSING_CITATION.
#' @param one_squeleton_result The one_squeleton object to extract the centralities
#' @return A vector with numeric values corresponding to the intersection centrality of the elements in the given one_squeleton
extract_intersection_centrality = function(one_squeleton_result)
{
  # Computes centrality
  centrality = as.vector(table(unlist(one_squeleton_result$points_in_nodes)))
  return(centrality)
}



# create_point_intersection_network
#' Constructs the Point Intersection Network as described in MISSING_CITATION.
#' @param one_squeleton_result A one_squeleton object to construct the network
#' @return The corresponding adjacency matrix for the point intersection network
create_point_intersection_network = function(one_squeleton_result)
{
  # Computes centrality
  centrality = extract_intersection_centrality(one_squeleton_result)

  # Constructs the return matrix
  adjacency =  matrix(0, length(centrality), length(centrality))

  # Sorry for the for
  for(i in 1:one_squeleton_result$num_nodes)
  {
    # Gets the points inside the node
    elements_in_node = one_squeleton_result$points_in_nodes[[i]]
    # Calculates the centrality of the elements inside the node
    centrality_in_node = centrality[elements_in_node]

    #Exctracts the central elements
    central_elements = elements_in_node[centrality_in_node == max(centrality_in_node)]

    # Extracts the other elements
    other_elements = elements_in_node[centrality_in_node < max(centrality_in_node)]

    # Creates the edge
    adjacency[other_elements, central_elements] = 1

  }

  return(adjacency)

}
