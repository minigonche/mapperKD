
#' @author Felipe González-Casabianca
#' Equivalent of unit tests for the library. Tests the algorithm on very simple cases that
#' try to isolate different sections of the algorithm (and any support methods)

# Size of test
percetange_of_test = 1

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


context("MapperKD Unit Tests. Support Functions. Construct Step Grid. Dimension 1")
# Test for function: advance_coordinates
test_that("Unit Tests for: Construct Step Grid. Dimension 1", {


  # The structure of the test will be:
  # Sample points in an 1 dimensinal space. Invokes mapperKD and will only work with the: nodes_per_interval
  # attribute. Will check the intersection by grid search and by brute force. Both should be the same

  # Case 1. n = 1.
  # Toy case
  N = 30
  filter = matrix(runif(N, -100, 100), nrow = N, ncol = 1)
  num_intervals = 4
  overlap = 30

  step_grid = construct_fixed_step_grid(filter,  num_intervals, overlap)
  res = mapperKD(k = 1, distance = matrix(rep(1,N**2), ncol = N), filter = filter, num_intervals = num_intervals, overlap = overlap, clustering_method = cluster_all)

  # Complete grid
  grid = as.matrix(expand.grid(lapply(num_intervals, function(i){1:i})))

  for(i in 1:nrow(grid))
  {
    current_id = grid[i,]

    # Searches for intersection using brute force. Saves as strings
    brute_results = c()

    for(j in 1:nrow(grid))
    {
      other_id = grid[j,]
      intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
      ahead = all(other_id >= current_id) & i != j
      if(length(intersection) > 0 & ahead)
        brute_results = c(brute_results, paste(other_id, sep = ','))
    }

    # Constructs possible intersections by step_grid
    step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], sep = ',')})

    expect_equal(length(setdiff(brute_results, step_results)), 0)

  }

  # Case 2. n = 1.
  # multiple Random cases

  num_ite = 100*percetange_of_test
  #num_ite = 1
  for(k in 1:num_ite)
  {

    N = sample(10:250, 1)
    filter =  matrix(runif(N, -100, 100), nrow = N, ncol = 1)
    num_intervals = sample(1:20, 1)
    overlap = sample(1:99, 1)

    step_grid = construct_fixed_step_grid(filter,  num_intervals, overlap)
    res = mapperKD(k = 1, distance = matrix(rep(1,N**2), ncol = N), filter = filter, num_intervals = num_intervals, overlap = overlap, clustering_method = cluster_all)

    # Complete grid
    grid = as.matrix(expand.grid(lapply(num_intervals, function(i){1:i})))

    for(i in 1:nrow(grid))
    {
      current_id = grid[i,]

      # Searches for intersection using brute force. Saves as strings
      brute_results = c()

      for(j in 1:nrow(grid))
      {
        other_id = grid[j,]
        intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
        ahead = all(other_id >= current_id) & i != j
        if(length(intersection) > 0 & ahead)
          brute_results = c(brute_results, paste(other_id, sep = ','))
      }

      # Constructs possible intersections by step_grid
      step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], sep = ',')})

      # Tests
      expect_equal(length(setdiff(brute_results, step_results)), 0)

    }

  }


})



context("MapperKD Unit Tests. Support Functions. Construct Step Grid. Dimension 2")
# Test for function: advance_coordinates
test_that("Unit Tests for: Construct Step Grid. Dimesnion 2", {


  # The structure of the test will be:
  # Sample points in an 2 dimensinal space. Invokes mapperKD and will only work with the: nodes_per_interval
  # attribute. Will check the intersection by grid search and by brute force. Both should be the same

  # Case 1. n = 2.
  # Toy case
  N = 30
  filter = cbind(runif(N, -100, 100), runif(N, -100, 100))
  num_intervals = c(4,4)
  overlap = c(30,55)

  step_grid = construct_fixed_step_grid(filter,  num_intervals, overlap)
  res = mapperKD(k = 2, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, num_intervals = num_intervals, overlap = overlap, clustering_method = cluster_all)

  # Complete grid
  grid = as.matrix(expand.grid(lapply(num_intervals, function(i){1:i})))

  for(i in 1:nrow(grid))
  {
    current_id = grid[i,]

    # Searches for intersection using brute force. Saves as strings
    brute_results = c()

    for(j in 1:nrow(grid))
    {
      other_id = grid[j,]
      intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
      ahead = all(other_id >= current_id) & i != j
      if(length(intersection) > 0 & ahead)
        brute_results = c(brute_results, paste(other_id, collapse = ','))
    }

    # Constructs possible intersections by step_grid
    step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], collapse = ',')})

    # Tests
    expect_equal(length(setdiff(brute_results, step_results)), 0)

  }

  # Case 2. n = 1.
  # multiple Random cases

  num_ite = 30*percetange_of_test
  #num_ite = 1
  for(k in 1:num_ite)
  {

    N = sample(10:250, 1)
    filter = cbind(runif(N, -100, 100), runif(N, -100, 100))
    filter_min = min(filter)
    filter_max = max(filter)
    filter_min = apply(filter,2, min)
    filter_max = apply(filter,2, max)
    num_intervals = sample(1:20, 2)
    overlap = sample(1:99, 2)


    step_grid = construct_fixed_step_grid(filter,  num_intervals, overlap)
    res = mapperKD(k = 2, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, num_intervals = num_intervals, overlap = overlap, clustering_method = cluster_all)

    # Complete grid
    grid = as.matrix(expand.grid(lapply(num_intervals, function(i){1:i})))

    for(i in 1:nrow(grid))
    {
      current_id = grid[i,]

      # Searches for intersection using brute force. Saves as strings
      brute_results = c()

      for(j in 1:nrow(grid))
      {
        other_id = grid[j,]
        intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
        ahead = all(other_id >= current_id) & i != j
        if(length(intersection) > 0 & ahead)
          brute_results = c(brute_results, paste(other_id, collapse = ','))
      }

      # Constructs possible intersections by step_grid
      step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], collapse = ',')})

      # Tests
      expect_equal(length(setdiff(brute_results, step_results)), 0)

    }
  }

})


context("MapperKD Unit Tests. Support Functions. Construct Step Grid. Dimension n")
# Test for function: advance_coordinates
test_that("Unit Tests for: Construct Step Grid. Dimesnion n", {


  # The structure of the test will be:
  # Sample points in a n dimensinal space. Invokes mapperKD and will only work with the: nodes_per_interval
  # attribute. Will check the intersection by grid search and by brute force. Both should be the same

  # Case 1. n = 3.
  # Toy case
  N = 30
  filter = cbind(runif(N, -100, 100), runif(N, -100, 100), runif(N, -100, 100))
  num_intervals = c(4,4,4)
  overlap = c(30,55, 70)

  step_grid = construct_fixed_step_grid(filter,  num_intervals, overlap)
  res = mapperKD(k = 3, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, num_intervals = num_intervals, overlap = overlap, clustering_method = cluster_all)

  # Complete grid
  grid = as.matrix(expand.grid(lapply(num_intervals, function(i){1:i})))

  for(i in 1:nrow(grid))
  {
    current_id = grid[i,]

    # Searches for intersection using brute force. Saves as strings
    brute_results = c()

    for(j in 1:nrow(grid))
    {
      other_id = grid[j,]
      intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
      ahead = all(other_id >= current_id) & i != j
      if(length(intersection) > 0 & ahead)
        brute_results = c(brute_results, paste(other_id, collapse = ','))
    }

    # Constructs possible intersections by step_grid
    step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], collapse = ',')})

    # Tests
    expect_equal(length(setdiff(brute_results, step_results)), 0)

  }

  # Case 2. n = random.
  # multiple Random cases

  num_ite = 10*percetange_of_test
  #num_ite = 1
  for(k in 1:num_ite)
  {
    n = sample(3:4, 1) # DImension
    N = sample(10:50, 1)

    filter = c()
    for(i in 1:n)
      filter = cbind(filter, runif(N, -100, 100))

    num_intervals = c()
    overlap = c()

    for(i in 1:n)
    {
      num_intervals = c(num_intervals, sample(2:4, 1))
      overlap = c(overlap, sample(1:75, 1))
    }


    step_grid = construct_fixed_step_grid(filter,  num_intervals, overlap)
    res = mapperKD(k = n, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, num_intervals = num_intervals, overlap = overlap, clustering_method = cluster_all)

    # Complete grid
    grid = as.matrix(expand.grid(lapply(num_intervals, function(i){1:i})))

    for(i in 1:nrow(grid))
    {
      current_id = grid[i,]

      # Searches for intersection using brute force. Saves as strings
      brute_results = c()

      for(j in 1:nrow(grid))
      {
        other_id = grid[j,]
        intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
        ahead = all(other_id >= current_id) & i != j
        if(length(intersection) > 0 & ahead)
          brute_results = c(brute_results, paste(other_id, collapse = ','))
      }

      # Constructs possible intersections by step_grid
      step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], collapse = ',')})

      # Tests
      expect_equal(length(setdiff(brute_results, step_results)), 0)

    }
  }

})




context("MapperKD Unit Tests. Construct Step Grid by Max Search")
# Test scnearios that respond with errors
test_that("MapperKD Unit Tests. Construct Step Grid by Max Search", {

  # Simple scenario
  max_search_possibilities = c(10)
  resp = construct_step_grid_by_max_search(max_search_possibilities)
  expect_equal(resp[,1], c(-10:-1,1:10))

  # Simple scenario 2
  max_search_possibilities = c(2,2)
  resp = construct_step_grid_by_max_search(max_search_possibilities)
  expect_equal(nrow(resp), prod(2*max_search_possibilities + 1) - 1)

  # Scenario with zero
  max_search_possibilities = c(0,5,0)
  resp = construct_step_grid_by_max_search(max_search_possibilities)
  expect_equal(resp[,2], c(-5:-1,1:5))


  # Random scenarios
  num_ite = 10*percetange_of_test
  #num_ite = 1
  for(k in 1:num_ite)
  {
    dim = sample(1:4, 1)
    max_search_possibilities = sample(1:10, dim)
    resp = construct_step_grid_by_max_search(max_search_possibilities)

    # Tests
    expect_equal(nrow(resp), prod(2*max_search_possibilities + 1) - 1) # Amount of elements
    expect_equal(sum(rowSums(resp == rep(0, dim)) == dim), 0) # No zero
    expect_equal(sum(duplicated(data.frame(resp))), 0) # No duplicated

  }


})





context("MapperKD Unit Tests. Extract GMM Intervals")
# Test scnearios that respond with errors
test_that("MapperKD Unit Tests. Extract GMM Intervals", {

  # Probar que todos los puntos se encuentran en al menos algun intervalo

  # Selcted scenarios
  # Scenario 1
  means = 0
  stds = 1
  width = 1
  min_interval = -1
  max_interval = 1
  resp = extract_gmm_intervals(means, stds, width, min_interval, max_interval)
  expect_equal(resp$start_locations, -1)
  expect_equal(resp$finish_locations, 1)

  # Scenario 2
  means = 0
  stds = 10
  width = 1
  min_interval = -1
  max_interval = 1
  resp = extract_gmm_intervals(means, stds, width, min_interval, max_interval)
  expect_equal(resp$start_locations, -1)
  expect_equal(resp$finish_locations, 1)



  # Scenario 3
  means = 0
  stds = 0.5
  width = 1
  min_interval = -1
  max_interval = 1
  resp = extract_gmm_intervals(means, stds, width, min_interval, max_interval)
  expect_equal(resp$start_locations, -1)
  expect_equal(resp$finish_locations, 1)

  # Scenario 4
  means = c(0,5)
  stds = c(1,4)
  width = 1
  min_interval = c(-1,3)
  max_interval = c(0,10)
  resp = extract_gmm_intervals(means, stds, width, min_interval, max_interval)
  expect_equal(resp$start_locations, c(-1,1))
  expect_equal(resp$finish_locations, c(1,10))

  # Scenario 5
  # Width to large, all intervals equal
  means = c(0,5,10)
  stds = c(1,1,1)
  width = 100
  min_interval = c(-1,3,8)
  max_interval = c(1,6,12)
  resp = extract_gmm_intervals(means, stds, width, min_interval, max_interval)
  expect_equal(resp$start_locations, c(-1,-1,-1))
  expect_equal(resp$finish_locations, c(12,12,12))


  # Width to small, all intervals are min and max
  means = c(0,5,10)
  stds = c(1,1,1)
  width = 0.00000001
  min_interval = c(-1,3,8)
  max_interval = c(1,6,12)
  resp = extract_gmm_intervals(means, stds, width, min_interval, max_interval)
  expect_equal(resp$start_locations, c(-1,3,8))
  expect_equal(resp$finish_locations, c(1,6,12))



})


context("MapperKD Unit Tests. Extract GMM Look Forward")
test_that("MapperKD Unit Tests. Extract GMM Look Forward", {

  # Selcted scenarios
  # Only one interval
  start_locations = 0
  finish_locations = 10
  resp = extract_gmm_look_forward(start_locations, finish_locations)
  expect_equal(resp, 0)

  # Two no overlap
  start_locations = c(0,5)
  finish_locations = c(2,7)
  resp = extract_gmm_look_forward(start_locations, finish_locations)
  expect_equal(resp, 0)

  # Two single point overlap
  start_locations = c(0,5)
  finish_locations = c(5,10)
  resp = extract_gmm_look_forward(start_locations, finish_locations)
  expect_equal(resp, 1)

  # Multiple intervals all overlap
  start_locations = c(0,0,0,0,0)
  finish_locations = c(1,1,1,1,1)
  resp = extract_gmm_look_forward(start_locations, finish_locations)
  expect_equal(resp, 4)

  # Realistic scenario
  start_locations = c(0,1,2,3)
  finish_locations = c(2.5,2.9,2.8,4)
  resp = extract_gmm_look_forward(start_locations, finish_locations)
  expect_equal(resp, 2)


})



context("MapperKD Unit Tests. Extract GMM Get Interval Function")
test_that("MapperKD Unit Tests. Extract GMM Get Interval Function", {

  # Single dimension, single interval
  all_start_locations = list()
  all_start_locations[[1]] = c(0)
  all_finish_locations = list()
  all_finish_locations[[1]] = c(10)

  fun = construct_gmm_get_interval_function(all_start_locations, all_finish_locations)
  expect_equal( fun(c(1))$min, 0)
  expect_equal(fun(c(1))$max, 10)

  # Single dimension, multiple interval
  all_start_locations = list()
  all_start_locations[[1]] = c(0,1,2)
  all_finish_locations = list()
  all_finish_locations[[1]] = c(10,11,12)

  fun = construct_gmm_get_interval_function(all_start_locations, all_finish_locations)
  expect_equal( fun(c(1))$min, 0)
  expect_equal(fun(c(1))$max, 10)
  expect_equal( fun(c(2))$min, 1)
  expect_equal(fun(c(2))$max, 11)
  expect_equal( fun(c(3))$min, 2)
  expect_equal(fun(c(3))$max, 12)

  # Multiple dimension, multiple interval
  all_start_locations = list()
  all_start_locations[[1]] = c(0,1,2)
  all_start_locations[[2]] = c(100,200,300)
  all_finish_locations = list()
  all_finish_locations[[1]] = c(10,11,12)
  all_finish_locations[[2]] = c(250,350,450)

  fun = construct_gmm_get_interval_function(all_start_locations, all_finish_locations)
  expect_equal( fun(c(1,1))$min, c(0,100))
  expect_equal(fun(c(1,1))$max, c(10,250))

  expect_equal( fun(c(2,1))$min, c(1,100))
  expect_equal(fun(c(2,1))$max, c(11,250))

  expect_equal( fun(c(3,1))$min, c(2,100))
  expect_equal(fun(c(3,1))$max, c(12,250))

  expect_equal( fun(c(2,2))$min, c(1,200))
  expect_equal(fun(c(2,2))$max, c(11,350))

  expect_equal( fun(c(3,2))$min, c(2,200))
  expect_equal(fun(c(3,2))$max, c(12,350))

  expect_equal( fun(c(3,3))$min, c(2,300))
  expect_equal(fun(c(3,3))$max, c(12,450))
})


context("MapperKD Unit Tests. Extract the GMM Components. One Dimension")
test_that("MapperKD Unit Tests. Extract the GMM Components. One Dimension", {


  # Single dimension. Single cluster
  fil = seq(-1, 1, by=0.1)
  filter = matrix(fil, nrow = length(fil), ncol = 1)
  resp = get_gmm_components(filter, width = 2)
  expect_equal(length(resp$intervals), 1)
  expect_equal(resp$intervals[1], 1)
  expect_equal(length(resp$step_grid), 0)
  expect_equal( resp$get_interval(1)$min, -1)
  expect_equal( resp$get_interval(1)$max, 1)



  # Single dimension. Two clusters. No overlap
  fil = c(seq(-2, -1, by=0.05), seq(1, 2, by=0.05))
  filter = matrix(fil, nrow = length(fil), ncol = 1)
  resp = get_gmm_components(filter, width = 1)
  expect_equal(length(resp$intervals), 1)
  expect_equal(resp$intervals[1], 2)
  expect_equal(length(resp$step_grid), 0)
  expect_equal( resp$get_interval(1)$min, -2)
  expect_equal( resp$get_interval(2)$max, 2)


  # Single dimension. Two clusters. Overlap
  fil = c(seq(-2, -1, by=0.05), seq(1, 2, by=0.05))
  filter = matrix(fil, nrow = length(fil), ncol = 1)
  resp = get_gmm_components(filter, width = 100) # Very width clusters
  expect_equal(length(resp$intervals), 1)
  expect_equal(resp$intervals[1], 2)
  expect_equal(length(resp$step_grid), 2)
  expect_equal( resp$get_interval(1)$min, -2)
  expect_equal( resp$get_interval(2)$max, 2)

  # Single dimension. Multiple clusters. All overlap
  fil = runif(200)
  filter = matrix(fil, nrow = length(fil), ncol = 1)
  resp = get_gmm_components(filter, width = 100) # Very width clusters

  expect_equal(resp$intervals- 1, max(resp$step_grid))

  # Random
  num_ite = 100*percetange_of_test

  for(i in 1:num_ite)
  {
    # Single dimension. Multiple clusters. All overlap
    fil  = runif(sample(20:200, 1))
    filter = matrix(fil, nrow = length(fil), ncol = 1)
    resp = get_gmm_components(filter, width = 100) # Very width clusters


    expect_equal(resp$intervals- 1, max(resp$step_grid,0))
  }








})


context("MapperKD Unit Tests. Extract the GMM Components. Two Dimension")
test_that("MapperKD Unit Tests. Extract the GMM Components. Two Dimension", {


  # Two dimensions. Single cluster both
  filter = cbind(seq(-1, 1, by=0.1), seq(10, 12, by=0.1))
  resp = get_gmm_components(filter, width = 2)
  expect_equal(length(resp$intervals), 2)
  expect_equal(resp$intervals[1], 1)
  expect_equal(resp$intervals[2], 1)
  expect_equal(length(resp$step_grid), 0)
  expect_equal( resp$get_interval(c(1,1))$min, c(-1,10))
  expect_equal( resp$get_interval(c(1,1))$max, c(1,12))



  # Two dimensions. Single cluster one, non overlapping clusters the other
  filter = cbind(seq(-1, 1, by=0.1), (c(seq(-2, -1, by=0.1), seq(1.1, 2, by=0.1))))
  resp = get_gmm_components(filter, width = 2)
  expect_equal(length(resp$intervals), 2)
  expect_equal(resp$intervals[1], 1)
  expect_equal(resp$intervals[2], 2)
  expect_equal(length(resp$step_grid), 0)
  expect_equal( resp$get_interval(c(1,1))$min, c(-1,-2))
  expect_equal( resp$get_interval(c(1,2))$max, c(1,2))


  # Two dimensions. Single cluster one, overlapping clusters the other
  filter = cbind(seq(-1, 1, by=0.1), (c(seq(-2, -1, by=0.1), seq(1.1, 2, by=0.1))))
  resp = get_gmm_components(filter, width = 100)
  expect_equal(length(resp$intervals), 2)
  expect_equal(resp$intervals[1], 1)
  expect_equal(resp$intervals[2], 2)
  expect_equal(nrow(resp$step_grid), 2)
  expect_equal(resp$step_grid[1,], c(0,-1))
  expect_equal(resp$step_grid[2,], c(0,1))
  expect_equal( resp$get_interval(c(1,1))$min, c(-1,-2))
  expect_equal( resp$get_interval(c(1,2))$max, c(1,2))


  # Two dimensions. Overlapping clusters both
  filter = cbind((c(seq(-5, -4, by=0.1), seq(3.1, 4, by=0.1))), (c(seq(-2, -1, by=0.1), seq(1.1, 2, by=0.1))))
  resp = get_gmm_components(filter, width = 100)
  expect_equal(length(resp$intervals), 2)
  expect_equal(resp$intervals[1], 2)
  expect_equal(resp$intervals[2], 2)
  expect_equal(nrow(resp$step_grid), 8)
  expect_equal( resp$get_interval(c(1,1))$min, c(-5,-2))
  expect_equal( resp$get_interval(c(2,2))$max, c(4,2))


  # Two dimensions. Multiple clusters. All overlap
  filter = cbind(runif(200), runif(200))
  resp = get_gmm_components(filter, width = 100) # Very width clusters

  expect_equal(prod(2*resp$intervals - 1) - 1, nrow(resp$step_grid))


  # Random
  num_ite = 100*percetange_of_test

  for(i in 1:num_ite)
  {
    # Two dimensions. Multiple clusters. All overlap
    n_elements = sample(20:200, 1)
    filter = cbind( runif(n_elements),  runif(n_elements))
    resp = get_gmm_components(filter, width = 100) # Very width clusters

    expect_equal(prod(2*resp$intervals - 1) - 1, nrow(resp$step_grid))

  }
})


context("MapperKD Unit Tests. Error Scenarios")
# Test scnearios that respond with errors
test_that("MapperKD Unit Tests. Error Scenarios", {

  # General parameters
  num_intervals = 2
  overlap = 20
  filter = c(1,2,3)

  # Bad filter
  filter = c(1,2,3)
  # --- FIlter One dimensional OK (k = 1)
  expect_true({mapperKD(k = 1, distance = matrix(rep(1,length(filter)**2), ncol = length(filter)), filter = filter, num_intervals = num_intervals, overlap = overlap, clustering_method = cluster_all); TRUE})
  # --- FIlter One dimensional not OK (k != 1)
  expect_error(mapperKD(k = 2, distance = matrix(rep(1,length(filter)**2), ncol = length(filter)), filter = filter, num_intervals = num_intervals, overlap = overlap, clustering_method = cluster_all))
  # --- FIlter two dimensional OK
  filter = cbind(c(1,2,3),c(4,5,6))
  expect_true({mapperKD(k = 2, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, num_intervals = c(num_intervals,num_intervals), overlap = c(overlap,overlap), clustering_method = cluster_all); TRUE})
  # --- FIlter two dimensional not OK
  expect_error(mapperKD(k = 1, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, num_intervals = num_intervals, overlap = overlap, clustering_method = cluster_all))
  expect_error(mapperKD(k = 3, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, num_intervals = num_intervals, overlap = overlap, clustering_method = cluster_all))

  # TODO: Finish
})









