
#' @author Felipe González-Casabianca
#' Equivalent of unit tests for the library. Tests the algorithm on very simple cases that
#' try to isolate different sections of the algorithm (and any support methods)


percetange_of_test = 0.1



context("MapperKD Unit Tests. Support Functions. Advance Coordinates")

# Test for function: advance_coordinates
test_that("Unit Tests for: advance_coordinates", {

  # Case 1
  initial_coordinates = c(4,5,5)
  initial_max_values = c(5,5,5)

  response = advance_coordinates(initial_coordinates, initial_max_values)
  expected = c(5,5,5)

  expect_equal( response, expected )

  # Case 2
  initial_coordinates = c(5,4,5)
  initial_max_values = c(5,5,5)

  response = advance_coordinates(initial_coordinates, initial_max_values)
  expected = c(1,5,5)

  expect_equal( response, expected )

  # Case 3
  initial_coordinates = c(1,1,1)
  initial_max_values = c(5,5,5)

  response = advance_coordinates(initial_coordinates, initial_max_values)
  expected = c(2,1,1)

  expect_equal( response, expected )

  # Case 4
  initial_coordinates = c(5,5,5)
  initial_max_values = c(5,5,5)

  response = advance_coordinates(initial_coordinates, initial_max_values)
  expected = c(1,1,1)

  expect_equal( response, expected )

})


context("MapperKD Unit Tests. Support Functions. Construct Step Grid")
# Test for function: advance_coordinates
test_that("Unit Tests for: Construct Step Grid", {

  #TODO: Do tests


})



context("MapperKD Unit Tests. Error Scenarios")
# Test scnearios that respond with errors
test_that("MapperKD Unit Tests. Error Scenarios", {

  # General parameters
  intervals = 2
  overlap = 20
  filter = c(1,2,3)

  # Bad filter
  filter = c(1,2,3)
  # --- FIlter One dimensional OK (k = 1)
  expect_true({mapperKD(k = 1, distance = matrix(rep(1,length(filter)**2), ncol = length(filter)), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, low_ram = FALSE, data = NA); TRUE})
  # --- FIlter One dimensional not OK (k != 1)
  expect_error(mapperKD(k = 2, distance = matrix(rep(1,length(filter)**2), ncol = length(filter)), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, low_ram = FALSE, data = NA))
  # --- FIlter two dimensional OK
  filter = cbind(c(1,2,3),c(4,5,6))
  expect_true({mapperKD(k = 2, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, low_ram = FALSE, data = NA); TRUE})
  # --- FIlter two dimensional not OK
  expect_error(mapperKD(k = 1, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, low_ram = FALSE, data = NA))
  expect_error(mapperKD(k = 3, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, low_ram = FALSE, data = NA))

  # TODO: Finish
})


context("MapperKD Unit Tests. Toy Examples. 1 Dimension")

# Toy examples
test_that("Toy Examples. 1 Dimension", {

  # In following examples the distance matrix can be mock because the cluster method is: cluster all

  # Multiple overlapping intervals over same point
  # When only one point is included (or multiple with same filter), the output graph must be a clique

  num_ite = 100*percetange_of_test
  #num_ite = 1
  for(i in 1:num_ite)
  {
      num_points = sample(2:10, 1)
      filter = rep(1,num_points)
      intervals = sample(2:10, 1)
      overlap = sample(1:99, 1)


      #print('Values')
      #print(num_points)
      #print(filter)
      #print(intervals)
      #print(overlap)
      #print('')

      #num_points = 10
      #filter = rep(1,num_points)
      #intervals = 10
      #overlap = 99

      res = mapperKD(k = 1,
                     distance = matrix(rep(1,length(filter)**2), ncol = length(filter)),
                     filter = filter,
                     intervals = intervals,
                     overlap = overlap,
                     clustering_method = cluster_all,
                     low_ram = FALSE,
                     data = NA)

      expect_equal( res$num_nodes, intervals )
      for(lev in res$points_in_nodes)
        expect_equal( length(lev), num_points )

      expect_equal( sum(res$adjacency_matrix), res$num_nodes**2 - res$num_nodes )

  }

  # 3 points. 2 Intervals. Cluster all
  filter = c(1,2,3)
  intervals = 2
  overlap = 20
  res = mapperKD(k = 1,
           distance = matrix(rep(1,length(filter)**2), ncol = length(filter)),
           filter = filter,
           intervals = intervals,
           overlap = overlap,
           clustering_method = cluster_all,
           low_ram = FALSE,
           data = NA)

  expect_equal( res$num_nodes, 2 )
  expect_equal( length(res$points_in_nodes[[1]]), 2 )
  expect_equal( length(res$points_in_nodes[[2]]), 2 )
  expect_equal( sum(res$adjacency_matrix), 2 )

  # 4 points. 2 Intervals (no overlap). Cluster all
  filter = c(1,2,3,4)
  intervals = 2
  overlap = 20
  res = mapperKD(k = 1,
                 distance = matrix(rep(1,length(filter)**2), ncol = length(filter)),
                 filter = filter,
                 intervals = intervals,
                 overlap = overlap,
                 clustering_method = cluster_all,
                 low_ram = FALSE,
                 data = NA)

  expect_equal( res$num_nodes, 2 )
  expect_equal( length(res$points_in_nodes[[1]]), 2 )
  expect_equal( length(res$points_in_nodes[[2]]), 2 )
  expect_equal( sum(res$adjacency_matrix), 0 )


})




