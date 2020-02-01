require('ggplot2')
require('igraph')

# Epidemiology Module
# This module contains the different functions and procedures to recreate the results from the publication: MISSING_CITATION.


#' ----------------------------------
#' --- 1-Esqueleton Functions -------
#' ----------------------------------


# get_1_esqueleton_node_sizes
#' Gets the size of each of the 1 esqueleton nodes. The size of each node is the amount of point it cointains
#' @param one_squeleton_result A one_squeleton object to calculate the sizes of the nodes
#' @return A vector with the corresponding sizes
get_1_esqueleton_node_sizes = function(one_squeleton_result)
{
  # Calculates the size
  return(sapply(one_squeleton_result$points_in_nodes, length))
}




# construct_grid_graph_layout
#' Constructs a grid layout of the 1 esqueleton (graph). If filter has only one dimension, the vertical layout is assumed evenly
#' If filter has more than two dimensional an exception is raised
#' @param one_squeleton_result A one_squeleton object to calculate the grid layout
#' @return A matrix n x 2, where n is the number of nodes in the 1 esqueleton
construct_grid_graph_layout = function(one_squeleton_result)
{
  # Gets the filter dimension
  filter_dim = get_filter_dimension(one_squeleton_result)


  if(filter_dim > 2)
    stop('The received 1 Esqueleton object was constructed using a filter with more than two dimensions. Maximum of two is allowed.')

  # Creates the return variables
  x = rep(0, one_squeleton_result$num_nodes)
  y = rep(0, one_squeleton_result$num_nodes)

  max_dimension = length(one_squeleton_result$nodes_per_interval)

  for(i in 1:max_dimension)
  {

    if(filter_dim == 1) # One dimensional case
    {
      # Nodes inside the interval
      nodes_in_interval = one_squeleton_result$nodes_per_interval[[i]]
      # Total numbers in the interval
      num_nodes_in_interval = length(nodes_in_interval)
      # X coordinate is constant
      x[nodes_in_interval] = i
      # Y Coordinate is evely sapced
      y[nodes_in_interval] = seq(0,max_dimension, max_dimension/(num_nodes_in_interval+1))[2:(num_nodes_in_interval + 1)]

    }
    else if(filter_dim == 2)# Two dimensional case
    {
      for(j in 1:length(one_squeleton_result$nodes_per_interval[[i]]))
      {
        # Nodes inside the interval
        nodes_in_interval = one_squeleton_result$nodes_per_interval[[c(i,j)]]
        # Assigns X and Y to their values
        x[nodes_in_interval] = i
        y[nodes_in_interval] = j
      }

    }
    else
      stop(paste('Unsuported amount of dimensions of the filter:',filter_dim))

  }

  # Creates the matrix and returns it
  return(cbind(x,y))

}

# get_filter_dimension
#' Calculates the filter's dimension of the provided 1 esqueleton
#' @param one_squeleton_result A one_squeleton object to calculate the filter's dimension
#' @return An int with the corresponding dimension
get_filter_dimension = function(one_squeleton_result)
{
  n = 1
  while(TRUE)
  {
    if(!is.list(one_squeleton_result$nodes_per_interval[[rep(1,n)]]))
    {
      return(n)
    }
    n = n + 1
  }
}


#' ----------------------------------
#' -- Point Intersection Network-----
#' ----------------------------------


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


# TODO: Include color
# TODO: Adjust sizes
# TODO: Remove FOr Loop
# TODO: Include de map scheme
# plot_point_intersection_network
#' Plots the point intersection network with geographical coordinates
#' @param adj_matrix The adjacency matrix of the point intersection network
#' @param lon A vector with the longitude coordinates (in decimal notation) of the points
#' @param lat A vector with the latitude coordinates (in decimal notation) of the points
#' @param sizes A vector or number with the size/s of the points. Default is 1.
#' @param colors A vector or number with the color/s of the points. Default is blue.
#' @return ggplot plottable object with the point intersection network
plot_point_intersection_network = function(adj_matrix, lon, lat, sizes = 1, colors = 'blue', map = NULL)
{
  # Constructs the arrows
  # SOrry for the for
  org = c()
  dest = c()
  for(i in 1:nrow(adj_matrix))
  {
    for(j in 1:ncol(adj_matrix))
    {
      if(adj_matrix[i,j] == 1)
      {
           org = c(org, i)
           dest = c(dest,j)
      }
    }
  }

  # Creates the coordinates of the arrows (strat and finish)
  # Start
  x1 = lon[org]
  y1 = lat[org]
  # FInish
  x2 = lon[dest]
  y2 = lat[dest]

  data_points = data.frame(lon = lon, lat = lat)
  data_segments = data.frame(x1 = x1, y1 = y1, x2 = x2, y2 = y2)


  # Creates objects
  if(is.null(map))
  {
    p =  ggplot() + geom_point(aes(x = lon, y = lat), size = sizes, colour = colors) +
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color = "yellow", size = 0.15, arrow = arrow(length = unit(0.03, "npc")))

  }
  else
  {
    p =  ggmap(map) + geom_point(data = data_points, aes(x = lon, y = lat), size = sizes, colour = colors) +
      geom_segment(data = data_segments, aes(x = x1, y = y1, xend = x2, yend = y2), color = "yellow", size = 0.15, arrow = arrow(length = unit(0.03, "npc")))

  }

  return(p)

}


