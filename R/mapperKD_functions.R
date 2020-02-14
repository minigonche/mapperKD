require('sets')

#' Implementation of the mapper algorithm described in:
#'
#' G. Singh, F. Memoli, G. Carlsson (2007). Topological Methods for the Analysis of High Dimensional Data Sets and 3D Object Recognition, Point Based Graphics 2007, Prague, September 2007.

#' @author Felipe González-Casabianca
#' mapperkD function
#'
#' NOTES:
#' 1. Uses closed inetervals over the filter space
#'
#' This function uses a filter function f: X -> R^k on a data set X that has n rows (observations) and m columns (variables).
#' @param k a positive integer indicating the number of dimensions of the filter space
#' @param distance an n x n matrix of pairwise distances or dissimilarities. If the parameter \code{local_distance} is set to \code{TRUE}, this parameter is ignored since the clustering algorithm will receive a subset of the data.
#' @param filter an n x k matrix of the filter space values, where n corresponds to the number of observations and k to the filter space dimension.
#' @param intervals a vector of k positive integers, the number of intervals for each correspong dimension of the filter space.
#' @param overlap a vector of k numbers between 0 and 100, specifying how much adjacent intervals should overlap for each dimension of the filter space.
#' @param clustering_method clustering method to be used for each pre-image. If the parameter \code{local_distance} is set to \code{TRUE}, the given funtion must receive as a parameter a distance matrix. If the parameter \code{local_distance} is set to \code{FALSE}, the given funtion must receive as a parameter a data.frame. In any case, it mus return an array with the corresiponding number cluster for each of the given points. Clusters numbers must start with 1 and have no gaps between them.
#' @param local_distance a boolean indicating if the algorithm should construct the distance function based on the data at every pre-image. Usefull for low RAM enviorments or specific clustering.
#' @param data a data frame containing the information necessary to excecute the clustering procedure. This parameter will only be used if \code{local_distance} is set to \code{TRUE}.
#'
#' @return An object of class \code{one_squeleton} which is composed of the following items:
#'
#' \code{adjacency_matrix} (adjacency matrix for the nodes),
#' \code{degree_matrix} (degree matrix for the nodes (the number of points in the intersection of nodes)),
#' \code{num_nodes} (integer number of vertices),
#' \code{points_in_nodes} (list with the points inside each node),
#' \code{nodes_per_interval} (list of lists, accesible through the vector id of the interval that contains the nodes that where constructucted inside of it)
#' \code{points_per_interval} (list of lists, accesible through the vector id of the interval that contains the points that are contained inside of it)
#'
#' @examples
#' library('mapperKD')
#' # Example
#' # ----------------
#' # Construct the data set
#' data_points = data.frame( x=c(cos(1:50) - 1, cos(1:50) + 1), y=sin(1:100) )
#' plot(data_points)
#' # Executes mapper
#' one_squeleton_result = mapperKD(k = 1,
#'                                 distance = as.matrix(dist(data_points)),
#'                                 filter = data_points$x,
#'                                 intervals = c(12),
#'                                 overlap = c(50),
#'                                 clustering_method = hierarchical_clustering,
#'                                 local_distance = FALSE,
#'                                 data = NA)
#' # Visualize the result
#' g = convert_to_graph(one_squeleton_result)
#' V(g)$size = sqrt(get_1_esqueleton_node_sizes(one_squeleton_result)*30)
#' splot(g)
#'
#' @export
#'
#' @export
#'
mapperKD = function(k,
                    distance,
                    filter,
                    intervals,
                    overlap,
                    clustering_method,
                    local_distance,
                    data)
{

  # Parameter consistency check
  # --------------------
  # Filter
  if(!is.matrix(filter))
  {
    #If supplied filter space is a one dimensional vector, it's transformed into a n x 1 matrix
    if(k == 1)
      filter = matrix(filter, nrow = length(filter), ncol = 1)
    else
      stop('The supplied filter parameter must by an n x k matrix')
  }

  if(dim(filter)[2] != k)
    stop(paste('Expected a matrix with',k,'columns for the filter parameter but got a matrix with',dim(filter)[2],'columns instead'))

  if(dim(filter)[1] == 1)
    stop('Sample needs at least two points to work.')

  # Intervals
  if(k != 1 && length(intervals) == 1)
  {
    print('The vector of intervals has dimension 1. Assuming its value for all dimensions of the filter space')
    intervals = rep(intervals, k)
  }

  if(length(intervals) != k )
    stop(paste('Expected a',k,'dimensional vector for the intervals parameter but got a',length(intervals),'dimensional vector instead'))

  if(any(intervals <= 0))
    stop('All interval values must be positive integers')


  # Overlap
  if(k != 1 && length(overlap) == 1)
  {
    print('The vector of overlap has dimension 1. Assuming its value for all dimensions of the filter space')
    overlap = rep(overlap, k)
  }

  if(length(overlap) != k )
    stop(paste('Expected a',k,'dimensional vector for the overlap parameter but got a',length(intervals),'dimensional vector instead'))

  if(any(intervals < 0) || any(intervals > 100))
    stop('All interval values must have values between 0 and a 100.')


  if(local_distance && missing(data))
    stop('local_distance is set to TRUE, so the data parameter must be supplied.')




  # Sets up the iteration process through intervals and overlap values
  # ----------------

  # Minimum and maximum filter values
  if(k == 1)
  {
    filter_min = min(filter)
    filter_max = max(filter)
  }
  else
  {
    filter_min = apply(filter,2, min)
    filter_max = apply(filter,2, max)
  }


  # Finds overlaped window size
  window_size = (filter_max - filter_min)/(intervals - (intervals - 1)*overlap/100)

  # Step size
  step_size = window_size*(1 - overlap/100)


  # Initializes the output parameters

  # Initializes the nodes_per_interval variable
  # A list of list of the number of dimensions in the filter space
  nodes_per_interval = sapply(1:intervals[length(intervals)], function(x) c())
  for(num_inte in rev(intervals)[-1])
    nodes_per_interval = sapply(1:num_inte, function(x) list(nodes_per_interval))

  # Initializes the point_per_interval variable
  # A list of list of the number of dimensions in the filter space
  points_per_interval = sapply(1:intervals[length(intervals)], function(x) c())
  for(num_inte in rev(intervals)[-1])
    points_per_interval = sapply(1:num_inte, function(x) list(points_per_interval))

  # Starts the elements_in_node variable
  elements_in_node = list()


  # Starts looping throught the different intervals
  # ------------------
  # Iteration is done advancing a k vector of coordinates for each of the intervals
  # Coordinates range in each dimension from 1 to the number of intervals

  coordinates = rep(1,k)


  # Loop ends when all coordinates get to the last value (the number of intervals)
  repeat
  {

    # Sets up the current interval
    # ------------------------
    #   Extracts the interval's current max and min values
    interval_min = filter_min + (coordinates - 1)*step_size
    interval_max = interval_min + window_size
    #   For of numeric precision, the max filter value for the las intervals in each dimension are forced to be the filter's max value on the correponding dimension
    interval_max[coordinates == intervals] = filter_max[coordinates == intervals]


    #   Gets the indices of the samples that fall on the current interval
    #   NOTE: this implemenatation uses closed intervals
    #     Currently uses the sweep function to build two matrices for the min and max
    above_matrix = sign(sweep(filter, 2, interval_min, FUN = ">="))
    below_matrix = sign(sweep(filter, 2, interval_max, FUN = "<="))
    #   Elementwise and operation
    inside_matrix = above_matrix*below_matrix

    #   Interval indices
    logical_interval_indices = rowSums(inside_matrix) == k
    interval_indices = which(logical_interval_indices)


    # Excecutes the clustering scheme
    # ------------------------
    if(!local_distance) # Global distance
    {
      # -Extracts the current matrix
      current_matrix = distance[interval_indices,interval_indices]

      # Excecutes the clustering algorithm
      clusters_of_interval = clustering_method(current_matrix)
    }
    else # Local distance
    {
      # Filters the data
      current_data = data[interval_indices,]
      # Excecutes the clustering algorithm
      clusters_of_interval = clustering_method(current_data)
    }



    # Constructs the nodes and adds them to the nodes_per_interval array
    #   Adjust the indices of the clusters to correpond with the nodes
    clusters_of_interval = clusters_of_interval + length(elements_in_node)

    #   Adds them to the nodes_per_interval variable
    nodes_per_interval[[coordinates]] = unique(clusters_of_interval)

    #   Adds the indices to the elements_in_node variable
    for(node in clusters_of_interval)
      elements_in_node[[node]] = interval_indices[clusters_of_interval == node]

    # Adds all the points to the npoint_per_interval
    points_per_interval[[coordinates]] = interval_indices

    # Stop criteria
    # Finished the last interval
    if(all(coordinates == intervals))
      break



    # Advances the coordinates
    # ------------------------
    coordinates = advance_coordinates(coordinates, intervals)





  }# Finish interval loop

  num_nodes = length(elements_in_node)


  # Builds adjacency and degree matrix
  # ------------------------
  #   Iterates over all intervals again and checks only intersection with possible intervals

  #   Starts the degree matrix
  degree_matrix = matrix(0, nrow = num_nodes, ncol = num_nodes)

  #   Starts the coordinates
  coordinates = rep(1,k)

  #   Constructs the search possibilities. Only checks intersection with further intervals (Adjacency matrix is symmetric)
  # Extracts the step grid
  step_grid = construct_step_grid(filter_min, filter_max, intervals, overlap)

  # Converts all the node lists to sets for efficiency
  elements_in_node_as_set = lapply(elements_in_node, as.set)

  # Loop ends when all coordinates get to the last value (the number of intervals)
  while(any(coordinates != intervals))
  {
    # Gets current nodes
    current_nodes = nodes_per_interval[[coordinates]]

    # If empty, continunes
    if(length(current_nodes) == 0)
    {
      coordinates = advance_coordinates(coordinates, intervals)
      next
    }


    # Calculates possible coordinates
    possible_coordinates = sweep(step_grid, 2, coordinates, "+")

    possible_indices = rowSums(sweep(possible_coordinates, 2, intervals, FUN = "<=")) == k
    possible_coordinates = matrix(possible_coordinates[possible_indices,], nrow = sum(possible_indices), ncol = k)


    # If empty, continunes
    if(length(possible_coordinates) == 0)
    {
      coordinates = advance_coordinates(coordinates, intervals)
      next
    }

    #   Iterates over the possible adjacent intervals
    for(i in 1:nrow(possible_coordinates))
    {

      # Gets neighbor interval
      neighbor_coordinates = possible_coordinates[i,]

      # Continues if outside interval grid
      #if(any(neighbor_coordinates > intervals))
      #  next

      neighbor_nodes = nodes_per_interval[[neighbor_coordinates]]

      # If empty, continunes
      if(length(neighbor_nodes) == 0)
        next

      for(v in current_nodes)
      {
        for(l in neighbor_nodes)
        {
          degree_matrix[l,v] = length(set_intersection(elements_in_node_as_set[[v]],elements_in_node_as_set[[l]]))
          degree_matrix[v,l] = degree_matrix[l,v]
        }
      }

    }

    # Advances the coordinates
    # ------------------------
    coordinates = advance_coordinates(coordinates, intervals)



  }# Finish adjacency and degrre loop

  #   Adjacency matrix is simply the sign of the degree matrix
  adjacency_matrix = sign(degree_matrix)

  # Constructs the response
  # Object of type 1_squeleton
  response = list()
  response$adjacency_matrix = sign(degree_matrix)
  response$degree_matrix = degree_matrix
  response$num_nodes = num_nodes
  response$points_in_nodes = elements_in_node
  response$nodes_per_interval = nodes_per_interval
  response$points_per_interval = points_per_interval


  #Return
  return(response)

}


#' Function that calculates the forward grid search space.
#' This method should return all the values that should be added to interval to obtain all the possible places
#' where an intersection might occur. This functionality is isolated inside a method to test it individually.
construct_step_grid = function(filter_min, filter_max, intervals, overlap)
{

  # Constructs the search possibilities. Only checks intersection with further intervals (Adjacency matrix is symmetric)

  # Finds overlaped window size
  window_size = (filter_max - filter_min)/(intervals - (intervals - 1)*overlap/100)
  # Step size
  step_size = window_size*(1 - overlap/100)

  # Depending on overlap, non adjacent intersection is possible

  max_search_posibilities = ceiling(1/(1 - overlap/100)) - 1
  max_search_posibilities = pmin(max_search_posibilities, intervals) # Should not check further than the total amount of intervals

  # Creates the search possibilities
  search_possibilities = lapply(1:length(intervals), function(i){0:max_search_posibilities[i]})

  #   The steps grid (structure to detect levels where possible intersection might occur)
  step_grid = as.matrix(expand.grid(search_possibilities))
  colnames(step_grid) <- NULL

  #   Removes itself from the possible steps
  step_grid = as.matrix(step_grid[rowSums(step_grid) > 0,])

  return(step_grid)
}

#' Function for advancing coordinates
advance_coordinates = function(coordinates, max_values)
{
  current_pos = 1
  carry = TRUE
  k = length(max_values)

  while(carry && current_pos <= k)
  {
    coordinates[current_pos] = coordinates[current_pos] + 1
    if(coordinates[current_pos] > max_values[current_pos])
    {
      coordinates[current_pos] = 1
      carry = TRUE
      current_pos = current_pos + 1
    }
    else
      carry = FALSE
  }

  return(coordinates)
}
