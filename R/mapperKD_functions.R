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
#' @param distance an n x n matrix of pairwise distances or dissimilarities. If the parameter \code{low_ram} is set to \code{TRUE}, this paraeter corresponds to a function that receives as parameters a dataset and index vector, to output the correponding distance matrix of only the given indices.
#' @param filter an n x k matrix of the filter space values, where n corresponds to the number of observations and k to the filter space dimension.
#' @param intervals a vector of k positive integers, the number of intervals for each correspong dimension of the filter space.
#' @param overlap a vector of k numbers between 0 and 100, specifying how much adjacent intervals should overlap for each dimension of the filter space.
#' @param clustering_method clustering method to be used for each pre-image. The given funtion must receive as a parameter a distance matrix and return an array with the corresiponding number cluster for each point. Clusters numbers must start with 1 and have no gaps between them.
#' @param low_ram a boolean indicating if the algorithm should be excecuted in a memory restricted enviorment
#' @param data a data frame (or any other type of structure, as long as the distance_function is aware) containing the information necessary to calculate the distance. This parameter will only be used if \code{lowe_ram} is set to \code{TRUE}.
#'
#' @return An object of class \code{1_squeleton} which is composed of the following items:
#'
#' \code{adjacency_matrix} (adjacency matrix for the nodes),
#' \code{degree_matrix} (degree matrix for the nodes (the number of points in the intersection of nodes)),
#' \code{num_nodes} (integer number of vertices),
#' \code{points_in_nodes} (list with the points inside each node),
#'
#' TODO: INCLUDE EXAMPLES
#' @export
#'
mapperKD = function(k,
                    distance,
                    filter,
                    intervals,
                    overlap,
                    clustering_method,
                    low_ram,
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

  # Checks if the ram parameter is True and the distance is a function
  if(low_ram && typeof(distance) != "closure")
    stop('low_ram is set to TRUE, so the distance parameter must be a function.')

  if(low_ram && missing(data))
    stop('low_ram is set to TRUE, so the data parameter must be supplied.')




  # Sets up the iteration process through intervals and overlap values
  # ----------------

  # Minimum and maximum filter values
  filter_min = sapply(filter, min)
  filter_max = sapply(filter, max)

  # Finds overlaped window size
  window_size = (filter_max - filter_min)/(intervals - (intervals - 1)*overlap/100)

  # Step size
  step_size = window_size*(1 - overlap/100)

  # Initializes the output parameters

  # Initializes the nodes_per_interval parameter
  # A list of list of the number of dimensions in the filter space
  nodes_per_interval = sapply(1:intervals[length(intervals)], function(x) c())
  for(num_inte in rev(intervals)[-1])
    nodes_per_interval = sapply(1:num_inte, function(x) list(nodes_per_interval))

  elements_in_node = list()


  # Starts looping throught the different intervals
  # ------------------
  # Iteration is done advancing a k vector of coordinates for each of the intervals
  # Coordinates range in each dimension from 1 to the number of intervals

  coordinates = rep(1,k)


  # Loop ends when all coordinates get to the last value (the number of intervals)
  while(any(coordinates != intervals))
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



    # Extracts the current distance matrix
    # ------------------------

    if(!low_ram)
      current_matrix = distance[interval_indices,interval_indices]
    else
      current_matrix = distance(data, interval_indices)



    # Excecutes the clustering algorithm
    # ------------------------
    clusters_of_interval = clustering_method(current_matrix)

    # Constructs the nodes and adds them to the nodes_per_interval array
    #   Adjust the indices of the clusters to correpond with the nodes
    clusters_of_interval = clusters_of_interval + length(elements_in_node)

    #   Adds them to the nodes_per_interval variable
    nodes_per_interval[[coordinates]] = unique(clusters_of_interval)

    #   Adds the indices to the elements_in_node variable
    for(node in clusters_of_interval)
      elements_in_node[[node]] = interval_indices[clusters_of_interval == node]



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
  #   If the overlap is 50% or larger, non adjacent intersection is possible and has to be taken into account
  search_possibilities = rep(list(0:1),length(intervals))
  search_possibilities[overlap >= 50] = list(0:2)


  #   The steps grid
  step_grid = as.matrix(expand.grid(search_possibilities))
  colnames(step_grid) <- NULL

  #   Removes itself from the possible steps
  step_grid = step_grid[rowSums(step_grid) > 0,]

  # Loop ends when all coordinates get to the last value (the number of intervals)
  while(any(coordinates != intervals))
  {

    current_nodes = nodes_per_interval[[coordinates]]
    #   Iterates over the possible adjacent intervals
    for(i in 1:nrow(step_grid))
    {

      # Gets neighbor interval
      neighbor_coordinates = coordinates + step_grid[i,]

      # Continues if outside interval grid
      if(any(neighbor_coordinates > intervals))
        next

      neighbor_nodes = nodes_per_interval[[neighbor_coordinates]]

      for(v in current_nodes)
      {
        for(k in neighbor_nodes)
        {
          degree_matrix[k,v] = length(interaction(elements_in_node[v],elements_in_node[k]))
          degree_matrix[v,k] = degree_matrix[k,v]
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

  #Return
  return(response)

}



#' Function for advancing
advance_coordinates = function(coordinates, max_values)
{
  current_pos = 1
  carry = TRUE
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
