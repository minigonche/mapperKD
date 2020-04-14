#' Biological Clusters
#' This module contains different cluster rutines designed to be used with genetic data.
#'
#' All clustering algorithms have to conform to the structure of the method explained in the file: general_clusters.R
#'

#' ----------------------------------
#' ---- Identity by Descend ---------
#' ----------------------------------

#' This clustering procedure uses the library: https://github.com/OJWatson/hmmibdr and its formats.

# get_genetic_ids_from_file
#' Extracts the genetic ids from a genotype file in HMMIBD format
#' @param input_file_location The location of the file
#' @return A vector with the corresponding ids
get_genetic_ids_from_file = function(input_file_location)
{

  # input_file_location = '/home/estudiante/Documentos/mapperKD/tests/testthat/test_files/pf3k_Ghana_13.txt'

  # Reads only the first lines to avoid loading the whole data
  input_con = file(input_file_location,"r")
  first_line = readLines(input_con, n=1)

  # Closes the connection
  close(input_con)

  # Extracts the ids
  ids = strsplit(first_line, '\t')[[1]]

  # Returns the ids
  return(ids[3:length(input_data)])

}

# filter_geotype_file
# TODO: Check that the given IDS are in the file
#' Filters a given genotype file in HMMIBD format
#' @param input_file_location The location of the input file
#' @param include_ids Vector with the ids to include.
#' @param output_file_location The location of where to save the filtered file
filter_geotype_file = function(input_file_location, include_ids, output_file_location)
{

  # input_file_location = '/home/estudiante/Documentos/mapperKD/tests/testthat/test_files/pf3k_Ghana_13.txt'

  # The method does not load the whole file, but rather reads line by line
  input_con = file(input_file_location,"r")
  output_con = file(output_file_location, "wt")


  # Reads the first line
  first_line = readLines(input_con, n=1)

  # Converts to columns names
  col_names = strsplit(first_line, '\t')[[1]]

  # Extracst the indices that will need to be copied
  col_indices = match(include_ids, col_names)

  if(length(col_indices) == 0)
    stop('None of the given ids where present in the given file')

  # Adds the first two columns (chromosome and position)
  col_indices = c(1,2,col_indices)


  # Constructs and writes the first line
  line_export = paste(col_names[col_indices], collapse = '\t')
  writeLines(line_export, output_con)

  # Iterates over the complete file and copies the lines
  while (length(current_line <- readLines(input_con, n = 1, warn = FALSE)) > 0) {

    # Divides line by the dilimeter
    current_line_split = strsplit(current_line, '\t')[[1]]

    # Constructs and writes the line
    line_export = paste(current_line_split[col_indices], collapse = '\t')
    writeLines(line_export, output_con)

  }

  # Closes connections
  close(input_con)
  close(output_con)

}


#' random_code
#' Support function for generating random leter codes
#' @param n length of the code
#' @return string of random characters
random_code = function(n = 10)
{
  response = paste(sample(LETTERS, n, TRUE),  collapse='')
  return(response)
}


#' excecute_ibd
#' Method that excecutes the Identity by Descend algorithm over the received files.
#' NOTE: All the locations of the given file should either be realtive or start with "/". This method missinterprets
#' the symbol "~" and will not find the files.
#' @param sample_file_location location of the sample file
#' @param freqency_file_location location of the allele frequency file
#' @return list with two elements:
#'         - segments: dataframe resulting from parsing the output file <filename>.hmm.txt.
#'         - fract: dataframe resulting from parsing the output file <filename>.hmm_fract.txt
#'        For details on the meaning of the columns of this two dataframes (or files) please go to: https://github.com/glipsnort/hmmIBD#output-files
excecute_ibd = function(sample_file_location, freqency_file_location)
{
  # Loads the HMMIBD library
  require('hmmibdr')

  # Creates the temporal file pattern
  temp_files = tempfile(pattern = random_code(10))

  # Excecutes IBD on files
  response = hmm_ibd(input_file = sample_file_location, allele_freqs =  freqency_file_location, output_file = temp_files)

  return(response)
}


input_file_location = '/home/minigonche/Documents/projects/mapperKD/tests/testthat/test_files/pf3k_Ghana_13.txt'
#input_file_location = 'pf3k_Ghana_13.txt'
filtered_file_location = '/home/minigonche/Documents/projects/mapperKD/tests/testthat/test_files/pf3k_Ghana_13_filtered.txt'
#filtered_file_location = 'pf3k_Ghana_13_filtered.txt'
allele_freqs =  "/home/minigonche/Documents/projects/mapperKD/tests/testthat/test_files/freqs_pf3k_Cambodia_13.txt"
#allele_freqs =  "freqs_pf3k_Cambodia_13.txt"

include_ids = c( "PF0008-C", "PF0028-C", "PF0037-C")
filter_geotype_file(input_file_location, include_ids, filtered_file_location)

res = excecute_ibd(filtered_file_location, allele_freqs)






