require('ggplot2')
require('igraph')
require('ggmap')

# Epidemiology Module
# This module contains the different functions and procedures to recreate the results from the publication: MISSING_CITATION.


#' ----------------------------------
#' --- 1-Esqueleton Functions -------
#' ----------------------------------

# convert_to_graph
#' Transforms the given 1 esqueleton result into an igraph graph.
#' @param one_squeleton_result The one_squeleton to convert
#' @return A vector with the corresponding sizes
convert_to_graph = function(one_squeleton_result)
{
  # Creates the graph
  g = graph.adjacency(one_squeleton_result$adjacency_matrix, mode = 'undirected')
  # Sets colors and labels
  V(g)$label = NA
  V(g)$color = rgb(0.2,1,0,0.3) # Lightgreen
  return(g)
}

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

# plot_1_esqueleton
#' Plots the given 1 Squeleton. The sizes of the nodes are proportional to the amount of elements they group.
#' @param one_squeleton_result A one_squeleton object to plot
#' @param layout The layout. This can either be a two column matrix with the x and y coordinates of each node of the 1 squeleton or the string 'grid' that calculates the layout using the method: construct_grid_graph_layout.
#' @param min_node_size Minimum node size. Default is 3.
#' @param max_node_size Maximum node size. Default is 23.
plot_1_esqueleton = function(one_squeleton_result, layout = 'grid', min_node_size = 3, max_node_size = 23)
{
 
  # Constructs Layout
  if(is.character(layout) & length(layout) == 1)
  {
    if(toupper(layout) == 'GRID')
      final_layout = construct_grid_graph_layout(one_squeleton_result)
    else
      stop(paste('Unsupported layout option:', layout))
  }
  else
    final_layout = layout
  
  # Constructs the graph
  g = convert_to_graph(one_squeleton_result)
  
  # Adjust the sizes
  node_size = get_1_esqueleton_node_sizes(one_squeleton_result)
  if(min(node_size) == max(node_size))
  {
    print('All nodes are the same size. Assuming minimum for all.')
    final_size = rep(3,length(node_size))
  }
  else
    final_size = 20*(node_size- min(node_size))/(max(node_size) - min(node_size)) + 3
  
  V(g)$size = final_size

  plot(g, layout = final_layout)
  
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


# create_point_intersection_adjacency
#' Constructs the Point Intersection Network as described in MISSING_CITATION.
#' @param one_squeleton_result A one_squeleton object to construct the network
#' @return The corresponding adjacency matrix for the point intersection network
create_point_intersection_adjacency = function(one_squeleton_result)
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

# create_point_intersection_network
#' Constructs the Point Intersection Network as described in MISSING_CITATION.
#' @param one_squeleton_result A one_squeleton object to construct the network
#' @return An igraph element with the point intersection network
create_point_intersection_network = function(one_squeleton_result)
{
  adjacency = create_point_intersection_adjacency(one_squeleton_result)
  pin = graph.adjacency(adjacency, mode = 'directed')
  
  # Sets the default parameters
  V(pin)$label = NA
  V(pin)$color = rgb(0.2,0,1,0.3) # Lighblue
  V(pin)$size = 1 + sqrt(2*degree(pin, v = V(pin), mode = "in"))
  
  return(pin)
  
}

# plot_intersection_network
# TODO: Include size and groups
# TODO: Que se vea bonito (tal vez mirar layouts)
#' Plots the point intersection network
#' @param one_squeleton_result A one_squeleton object to construct the network
plot_intersection_network = function(one_squeleton_result)
{
  plot(create_point_intersection_network(one_squeleton_result))
}

# plot_intersection_network_over_map
# TODO: Include size and groups. And noise (description)
# TODO: include scenario where lat and lons are the same
# TODO: allow subset of indices to be paseed
#' Plots the point intersection network with geographical coordinates over map
#' @param one_squeleton_result A one_squeleton object to construct the network
#' @param lon A vector with the longitude coordinates (in decimal notation) of the points
#' @param lat A vector with the latitude coordinates (in decimal notation) of the points
#' @param groups A vector with the corresponding groups of the elements. 

plot_intersection_network_over_map = function(one_squeleton_result, lon, lat, groups = NULL, noise = 1)
{
  

  # Adds noise to the coordinates So little overlap is obtained
  # lon
  lon_eps = abs((noise/100)*(max(lon) - min(lon)))
  final_lon  = lon + runif(length(lon), -1*lon_eps, lon_eps)
  # lat
  lat_eps = abs((noise/100)*(max(lat) - min(lat)))
  final_lat  = lat + runif(length(lat), -1*lat_eps, lat_eps)
  
  # Gets the elements for ploting
  elements = create_plot_elements_for_point_intersection_network(one_squeleton_result, lon = lon, lat = lat)
  data_points = elements[[1]]
  data_segments = elements[[2]]
  
  # Gets the sizes
  # TODO: creates sizes depending on the zoom
  size = rep(3, length(lat))
  
  # Gets the map
  # Calculates the square
  radius = max( max(final_lon) - min(final_lon), max(final_lat) - min(final_lat))/2
  mid_lon = min(final_lon) + (max(final_lon) - min(final_lon))/2
  mid_lat = min(final_lat) + (max(final_lat) - min(final_lat))/2
  
  margin = 0.05
  left = (mid_lon - radius) - radius*margin
  right = (mid_lon + radius) + radius*margin
  top = (mid_lat + radius) + radius*margin
  bottom = (mid_lat - radius) - radius*margin
  
  map = get_map(c(left = left, bottom = bottom, right = right, top =top), maptype = 'terrain')
  

  p =  ggmap(map) + geom_point(data = data_points, aes(x = lon, y = lat, color = meta_df$country), size = size) +
    geom_segment(data = data_segments, aes(x = x1, y = y1, xend = x2, yend = y2), color = "yellow", size = 0.15, arrow = arrow(length = unit(0.03, "npc")))
  
  plot(p)
}

# TODO: Remove For Loop
# create_plot_elements_for_point_intersection_network
#' Creates the elements to plot the point intersection network with geographical coordinates
#' @param one_squeleton_result A one_squeleton object to construct the network
#' @param lon A vector with the longitude coordinates (in decimal notation) of the points
#' @param lat A vector with the latitude coordinates (in decimal notation) of the points
#' @return a tuple with the following values
#'   - Points (DataFrame): Where x is lon and y is latitude
#'   - Arrows (DataFrame): where x1, x2 are the longitude of the start and finish of the arrows and y1,y2 the latitudes
create_plot_elements_for_point_intersection_network = function(one_squeleton_result, lon, lat)
{
  # Extracts adjacency
  adj_matrix = create_point_intersection_adjacency(one_squeleton_result)
  # Constructs the arrows
  # Sorry for the loop
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
  
  # Constructs the response scheme
  response = list()
  response[[1]] = data.frame(lon = lon, lat = lat)
  response[[2]] = data.frame(x1 = x1, y1 = y1, x2 = x2, y2 = y2)


  return(response)

}


