library('mapperKD')

# Loads the data
meta_df = read.csv('data/data.csv', stringsAsFactors = FALSE)
distance_matrix = read.csv('data/matrix.csv', header = FALSE)



# Applies the Algorithm
one_skeleton_result = mapperKD(k = 1, # <- Number of dimensions
                                distance = distance_matrix, # <- Distance Matrix
                                filter = meta_df$day_ocurrence, # <- Filter
                                interval_scheme = "FIXED", # <- Interval Scheme
                                num_intervals = c(20), # <-number of intervals
                                overlap = c(30), # <- Percentage of overlap
                                clustering_method = function(x){hierarchical_clustering(x,  method = 'single', height = 0.05)})


# Plots the 1 Skeleton (graph)
plot_1_skeleton(one_skeleton_result)

# Plots the point intersection network
groups = sapply(meta_df$GROUP, toString)
plot_intersection_network(one_skeleton_result, groups = groups, max_node_size = 6)


# Plot intersection network over colombia
only_colombia =  (1:dim(meta_df)[1])[meta_df$lon < - 70]
plot_intersection_network_over_map(one_skeleton_result,
                                   lon  = meta_df$lon,
                                   lat = meta_df$lat,
                                   groups = groups,
                                   arrow_color = 'DESTINATION',
                                   arrow_transparency = 0.3,
                                   focus_on = only_colombia)

