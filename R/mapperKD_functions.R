library('sets')

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
#' @param interval_scheme A string indicating the interval scheme. Default value is: FIXED Currently there is only support for two schemes:
#'        - FIXED: traditional scheme. The user must provide the number of intervals (\code{num_intervals}) and the percentage of overlap (\code{overlap})
#'        - GMM: Gaussian Mixed Model. Excecutes a GMM procedure over each dimension to infer the intervals and overlaps to be used.
#' @param num_intervals a vector of k positive integers, the number of intervals for each correspong dimension of the filter space.
#' @param overlap a vector of k numbers between 0 and 100, specifying how much adjacent intervals should overlap for each dimension of the filter space.
#' @param width Parameter used when the interval_scheme is GMM. Corresponds to the width of the clusters when excecuting GMM, each interval corresponds to [mean - width*std, mean + width*std] (for each dimension)
#' @param clustering_method clustering method to be used for each pre-image. If the parameter \code{local_distance} is set to \code{TRUE}, the given funtion must receive as a parameter a distance matrix. If the parameter \code{local_distance} is set to \code{FALSE}, the given funtion must receive as a parameter a list of indices (The indices of the corresponding interval). In any case, it mus return an array with the corresiponding number cluster for each of the given points. Clusters numbers must start with 1 and have no gaps between them. Default is: hierarchical_clustering
#' @param local_distance a boolean indicating if the algorithm should construct the distance function based on the data at every pre-image. Usefull for low RAM enviorments or specific clustering. Default value is \code{FALSE}
#'
#' @return An object of class \code{one_skeleton} which is composed of the following items:
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
#' one_skeleton_result = mapperKD(k = 1,
#'                                 distance = as.matrix(dist(data_points)),
#'                                 filter = data_points$x,
#'                                 interval_scheme = "FIXED",
#'                                 intervals = c(12),
#'                                 overlap = c(50),
#'                                 clustering_method = hierarchical_clustering)
#' # Visualize the result
#' g = convert_to_graph(one_skeleton_result)
#' V(g)$size = sqrt(get_1_eskeleton_node_sizes(one_skeleton_result)*30)
#' plot(g)
#'
#' @export
#'
#'
mapperKD = function(k,
                    distance,
                    filter,
                    interval_scheme = "FIXED",
                    num_intervals = NA,
                    overlap = NA,
                    width = 2,
                    clustering_method = hierarchical_clustering,
                    local_distance = FALSE,
                    data = NA)
{
  # Corrects interval_scheme
  interval_scheme = toupper(interval_scheme)

  # Parameter consistency check
  # --------------------
  # Filter
  if(is.null(filter) || is.na(filter))
    stop('Filter cannot be NULL')

  if(!is.matrix(filter))
  {
    #If supplied filter space is a one dimensional vector, it's transformed into a n x 1 matrix
    if(k == 1)
      filter = matrix(filter, nrow = length(filter), ncol = 1)
    else
      stop('The supplied filter parameter must by an n x k matrix')
  }

  if(dim(filter)[2] != k)
    stop(paste('Expected a matrix with',k,'columns for the filter parameter but got a matrix with', dim(filter)[2], 'columns instead'))

  if(dim(filter)[1] == 1)
    stop('Sample needs at least two points to work.')


  if(toupper(interval_scheme) == "FIXED")
  {
    # Intervals
    if(is.null(num_intervals) || is.na(num_intervals))
      stop('If interval_scheme is FIXED, number of Intervals cannot be NULL')

    if(k != 1 && length(num_intervals) == 1)
    {
      print('The vector of intervals has dimension 1. Assuming its value for all dimensions of the filter space')
      num_intervals = rep(num_intervals, k)
    }

    if(length(num_intervals) != k )
      stop(paste('Expected a',k,'dimensional vector for the intervals parameter but got a',length(num_intervals),'dimensional vector instead'))

    if(any(num_intervals <= 0))
      stop('All interval values must be positive integers')


    # Overlap
    if(is.null(overlap) || is.na(overlap))
      stop('If interval_scheme is FIXED, overlap cannot be NULL')

    if(k != 1 && length(overlap) == 1)
    {
      print('The vector of overlap has dimension 1. Assuming its value for all dimensions of the filter space')
      overlap = rep(overlap, k)
    }

    if(length(overlap) != k )
      stop(paste('Expected a',k,'dimensional vector for the overlap parameter but got a',length(num_intervals),'dimensional vector instead'))

    if(any(num_intervals < 0) || any(num_intervals > 100))
      stop('All interval values must have values between 0 and a 100.')
  }
  if( interval_scheme == "GMM")
  {
    if(is.null(width))
      stop("Width cannot be NULL")

    if(width <= 0)
      stop("with has to be positive")
  }

  # Distance
  if(is.null(distance) || is.na(distance))
    stop('Distance cannot be NULL')

  if(local_distance && missing(data))
    stop('local_distance is set to TRUE, so the data parameter must be supplied.')

  # ------------
  # End of errrors



  # Sets up the interval scheme
  if(toupper(interval_scheme) == "FIXED")
  {
    # Sets the intervals
    intervals = num_intervals

    # Sets the function to extract the interval
    get_interval = get_fixed_interval_extractor(filter, num_intervals, overlap)

    #   Constructs the search possibilities. Only checks intersection with further intervals (Adjacency matrix is symmetric)
    # Extracts the step grid
    step_grid = construct_fixed_step_grid(filter, intervals, overlap)

  }
  else if(toupper(interval_scheme) == "GMM")
  {
    response = get_gmm_components(filter, width)
    intervals = response[['intervals']]
    get_interval = response[['get_interval']]
    step_grid = response[['step_grid']]


  }
  else
    stop(paste("The interval_scheme:", interval_scheme, "is not supported"))


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

    current_interval = get_interval(coordinates)
    interval_min = current_interval[['min']]
    interval_max = current_interval[['max']]


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


    # Excecutes the clustering scheme (if there is more than one element in the interval)
    # ------------------------
    if(length(interval_indices) > 0)
    {

        if(length(interval_indices) == 1) # Single point
        {
          clusters_of_interval = 1
        }
        else if(!local_distance) # Global distance
        {

          # -Extracts the current matrix
          current_matrix = distance[interval_indices,interval_indices]

          # Excecutes the clustering algorithm
          clusters_of_interval = clustering_method(current_matrix)


        }
        else # Local distance
        {
          # Excecutes the clustering algorithm with only the interval indices
          clusters_of_interval = clustering_method(interval_indices)
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
    }


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
  degree_matrix = matrix(-1, nrow = num_nodes, ncol = num_nodes)

  #   Starts the coordinates
  coordinates = rep(1,k)

  # Converts all the node lists to sets for efficiency
  elements_in_node_as_set = lapply(elements_in_node, as.set)

  # Loop ends when all coordinates get to the last value (the number of intervals)
  repeat
  {
    # Gets current nodes
    current_nodes = nodes_per_interval[[coordinates]]

    # Calculates possible coordinates
    possible_coordinates = sweep(step_grid, 2, coordinates, "+")

    # Removes thes ones that exceed the max size
    possible_indices = rowSums(sweep(possible_coordinates, 2, intervals, FUN = "<=")) == k
    possible_coordinates = matrix(possible_coordinates[possible_indices,], nrow = sum(possible_indices), ncol = k)

    # Or that are below one
    possible_indices = rowSums(sweep(possible_coordinates, 2, rep(1,k), FUN = ">=")) == k
    possible_coordinates = matrix(possible_coordinates[possible_indices,], nrow = sum(possible_indices), ncol = k)

    # If empty, continunes
    if(length(possible_coordinates) > 0 && length(current_nodes) > 0)
    {

      #   Iterates over the possible adjacent intervals
      for(i in 1:nrow(possible_coordinates))
      {

        # Gets neighbor interval
        neighbor_coordinates = possible_coordinates[i,]
        neighbor_nodes = nodes_per_interval[[neighbor_coordinates]]

        # If empty, continunes
        if(length(neighbor_nodes) == 0)
          next

        for(v in current_nodes)
        {
          for(l in neighbor_nodes)
          {
            if(degree_matrix[l,v] != -1) # Checks if it has been calculated before
              degree_matrix[v,l] = degree_matrix[l,v]
            else
              degree_matrix[v,l] = length(set_intersection(elements_in_node_as_set[[v]],elements_in_node_as_set[[l]]))
          }
        }
      }

    }
    # Stop criteria
    # Finished the last interval
    if(all(coordinates == intervals))
      break

    # Advances the coordinates
    # ------------------------
    coordinates = advance_coordinates(coordinates, intervals)



  }# Finish adjacency and degrre loop

  # All values that stayed -1 is because there was no overlap in filter, and should not intersect in any points
  degree_matrix[degree_matrix == -1] = 0

  #   Adjacency matrix is simply the sign of the degree matrix
  adjacency_matrix = sign(degree_matrix)

  # Constructs the response
  # Object of type 1_skeleton
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


# -------------------------
# -- Fixed Interval Methods
# -------------------------

#' Constructs a function that gives the mins and maxs filter values of the given coordinates.
#' Returns the desired function
#' TODO: Document
get_fixed_interval_extractor = function(filter, num_intervals, overlap)
{

  # Minimum and maximum filter values
  filter_min = apply(filter,2, min)
  filter_max = apply(filter,2, max)

  # Finds overlaped window size
  window_size = (filter_max - filter_min)/(num_intervals - (num_intervals - 1)*overlap/100)

  # Step size
  step_size = window_size*(1 - overlap/100)

  get_fixed_interval = function(coordinates)
  {
    #   Extracts the interval's current max and min values
    interval_min = filter_min + (coordinates - 1)*step_size
    interval_max = interval_min + window_size

    #   For of numeric precision, the max filter value for the las intervals in each dimension are forced to be the filter's max value on the correponding dimension
    #   Same scneario when the window size is zero (which should include the whole interval in the given dimension)
    interval_max[coordinates == num_intervals] = filter_max[coordinates == num_intervals]
    interval_max[window_size == 0] = filter_max[window_size == 0]

    # Saves the results
    response = list()
    response[['min']] = interval_min
    response[['max']] = interval_max

    return(response)
  }


  return(get_fixed_interval)

}

#' Function that calculates the forward grid search space.
#' This method should return all the values that should be added to interval to obtain all the possible places
#' where an intersection might occur. This functionality is isolated inside a method to test it individually.
#' TODO: Document
construct_fixed_step_grid = function(filter, intervals, overlap)
{

  # Constructs the search possibilities. Only checks intersection with further intervals (Adjacency matrix is symmetric)

  # Minimum and maximum filter values
  filter_min = apply(filter,2, min)
  filter_max = apply(filter,2, max)

  # Finds overlaped window size
  window_size = (filter_max - filter_min)/(intervals - (intervals - 1)*overlap/100)

  # Depending on overlap, non adjacent intersection is possible
  max_search_possibilities = ceiling(1/(1 - overlap/100)) - 1
  max_search_possibilities = pmin(max_search_possibilities, intervals) # Should not check further than the total amount of intervals

  # If window_size is equal to zero, then the amount of overlap has no efect and should check all intervals in every iteration.
  max_search_possibilities[window_size == 0] = intervals[window_size == 0]

  # Returns the stepgrid
  step_grid = construct_step_grid_by_max_search(max_search_possibilities)

  return(step_grid)

}

# construct_step_grid_by_max_search
#' Method that calculates a stepgrid by given the max look ahead by each dimension.
#' This methos is isolated to test it individually
#' @param max_search_possibilities Numeric vector with the maximum look ahead seach that should be done
#' @return matrix with the corresponding stepgrid. Columns: Number of dimensions and each road correspond to a posible next interval to be searched.
construct_step_grid_by_max_search = function(max_search_possibilities)
{


  # Creates the search possibilities
  search_possibilities = lapply(1:length(max_search_possibilities), function(i){(-1*max_search_possibilities[i]):max_search_possibilities[i]})

  #   The steps grid (structure to detect levels where possible intersection might occur)
  step_grid = as.matrix(expand.grid(search_possibilities))
  colnames(step_grid) <- NULL

  #   Removes itself from the possible steps
  step_grid = as.matrix(step_grid[rowSums(abs(step_grid)) > 0,])

  # For single row the method above returns a columns not a row
  if(dim(step_grid)[2] != length(max_search_possibilities))
    step_grid = t(step_grid)

  return(step_grid)

}



# -------------------------
# -- GMM Interval Methods-
# -------------------------


# TODO: Document
#' Method that returns the three component to excecute the mapper algorithm:
#' Intervals:
#' Interval Extractor:
#' Step Grid

get_gmm_components = function(filter, width = 2)
{
  # Loads the required packages
  require('mclust')


  # Initializes the interval variable
  intervals = c()

  # Variables to temporarily save the start and finish of each interval by dimension
  all_start_locations = list()
  all_finish_locations = list()

  # Variable to store the max forward look
  max_look_forward = c()


  # Iterates over each dimension of the filter space
  k = dim(filter)[2]
  for(i in 1:k)
  {
    current_set = data.frame(x = filter[,i])


    # Excecutes the clustering scheme (GMM)
    BIC = mclustBIC(current_set, verbose = FALSE)
    clust_res = Mclust(current_set, x = BIC, verbose = FALSE)

    # Extracts the result
    num_clusters = clust_res$G

    # Extracts the Means
    means = as.vector(clust_res$parameters$mean)

    # Calculates the min and max by clust
    min_in_interval = sapply(1:num_clusters, function(clu){min(means[clu], current_set[clust_res$classification == clu,])})
    max_in_interval = sapply(1:num_clusters, function(clu){max(means[clu], current_set[clust_res$classification == clu,])})


    var_model_name = clust_res$parameters$variance$modelName
    # Extracts the variance
    if( var_model_name == "V")
      stds = sqrt(clust_res$parameters$variance$sigmasq)
    else if(var_model_name == "E" || var_model_name == "X")
      stds = sqrt(rep(clust_res$parameters$variance$sigmasq, num_clusters))
    else
      stop(paste("Unrecognized variance model name:", clust_res$parameters$variance$modelName))

    # Calculates the intervals

    resp = extract_gmm_intervals(means, stds, width, min_in_interval, max_in_interval)


    # Adds the start and finish
    all_start_locations[[i]] = resp[['start_locations']]
    all_finish_locations[[i]] = resp[['finish_locations']]

    # Adds the interval number
    intervals = c(intervals, num_clusters)

    # Adds max look forward
    look_forward = extract_gmm_look_forward(resp[['start_locations']], resp[['finish_locations']])


    max_look_forward = c(max_look_forward, look_forward)

  }


  # The intervals
  final_intervals = intervals

  # The function that gets the interval
  get_gmm_interval = construct_gmm_get_interval_function(all_start_locations, all_finish_locations)

  # Finally constructs the stepgrid
  step_grid = construct_step_grid_by_max_search(max_look_forward)



  # Constructs the response
  response = list()
  response[['intervals']] = final_intervals
  response[['get_interval']] = get_gmm_interval
  response[['step_grid']] = step_grid

  return(response)

}



# extract_gmm_intervals
#' Support method that constructs the intervals and overlap of a given vector of means and standard deviations.
#' Each interval corresponds to: [mean - width*std, mean + width*std]. The first interval starts at min_interval and the last one should end at max_interval.
#' Also, no interval should be before the min_interval value or after the max_interval.
#' This method was made independent for testing
#' @param means Numeric vector with the means of the clusters
#' @param stds Numeric vector with the standard deviation of the clusters
#' @param width The desired width of the intervals as denoted above.
#' @param min_in_interval The lowest value in each of the intervals.
#' @param max_in_interval The largest value in each of the intervals.
#' @return A list with the following objects:
#'        - start_locations: numeric vector with the start of the intervals
#'        - finish_locations: numeric vector with the end of the intervals
extract_gmm_intervals = function(means, stds, width, min_in_interval, max_in_interval)
{

  num_clusters = length(means)

  # Minimum and maximum of all intervals
  global_min = min(min_in_interval)
  global_max = max(max_in_interval)

  # Sorts the values so means only increase
  sorted_indexes = order(means)
  means = means[sorted_indexes]
  stds = stds[sorted_indexes]
  min_in_interval = min_in_interval[sorted_indexes]
  max_in_interval = max_in_interval[sorted_indexes]

  # Creates the start and finish vectors according to the width. If width does nos cover the whole cluster, then the border are adjusted to fit it.
  start_locations = sapply(1:num_clusters, function(i){min( means[i] - width*stds[i] , min_in_interval[i])})
  finish_locations = sapply(1:num_clusters, function(i){max( means[i] + width*stds[i] , max_in_interval[i])})

  # Adjust so nothing goes before or beyond the filter values
  start_locations[start_locations < global_min] = global_min
  finish_locations[finish_locations > global_max] = global_max

  # Response
  response = list()
  response[['start_locations']] = start_locations
  response[['finish_locations']] = finish_locations

  return(response)

}

# extract_gmm_look_forward
#' Method that, given the intervals, check many intervals ahead the algorith should check
#' This method was isolated to test individually
#' @param start_locations Numeric vector with the start of the intervals.
#' @param finish_locations Numeric vector with the end of the intervals
#' @return Number with the amount of intervals to look ahead
extract_gmm_look_forward = function(start_locations, finish_locations)
{

  if(length(start_locations) == 1)
    return(0)

  max_look_ahead = 0
  for(i in 1:(length(start_locations) - 1))
  {
    # Checks intersection with filters ahead
    look_ahead = sum(start_locations[(i+1):length(start_locations)] <= finish_locations[i])
    max_look_ahead = max(look_ahead, max_look_ahead)
  }

  return(max_look_ahead)

}


# construct_gmm_get_interval_function
#' Method that constructs the function that returns the intervals given the parameters.
#' The result of this function is used  when the interval scheme is GMM. The funciton was isolated to test individually
#' @param all_start_locations List with numeric indices, where each location contains a numeric vector for the start positions on the filter space at that precise dimension.
#' @param all_finish_locations List with numeric indices, where each location contains a numeric vector for the finish positions on the filter space at that precise dimension.
#' @return A function with the following behaviour:
#'         - Parameters:
#'             - coordinates: numeric vector with the current coordinates of the intervals
#'         - Returns: a list with two elements:
#'            - min: numeric vector with the minimum position for the given interval for each dimension
#'            - max: numeric vector with the maximum position for the given interval for each dimension
construct_gmm_get_interval_function = function(all_start_locations, all_finish_locations)
{


  # Extracts the number of dimensions
  k = length(all_start_locations)

  get_gmm_interval = function(coordinates)
  {
    # Constructs the min and max
    interval_min = sapply(1:k, function(j) all_start_locations[[j]][coordinates[j]]  )
    interval_max = sapply(1:k, function(j) all_finish_locations[[j]][coordinates[j]]  )

    # Saves the results
    response = list()
    response[['min']] = interval_min
    response[['max']] = interval_max

    return(response)
  }

  return(get_gmm_interval)
}
