#' Download all files from osf directory 
#' This function is intended to run inside dplyr do() verb. Downloads data locally
#' The function was written by Filip Děchtěrenko for another project.
#'
#' @param df row with directory on osf repository
#' @param local_data_pth Path where the data should be saved
#' @param should_overwrite whether the data should be overwritten. Default is True
#'
#' @return
#' @export
#'
#' @examples
#' local_data_pth <- file.path("data","Source")
#' create_local_structure(local_data_pth)
#' data_files <- 
#'   weissman_project %>% 
#'   osf_ls_files() %>% 
#'   filter(name == "Source") %>%
#'   osf_ls_files() 
#'   
#'   data_files %>% 
#'   group_by(name) %>% # for each experiment type
#'     do(download_files(.,local_data_pth))
download_files <- function(df, local_data_pth, should_overwrite = T) {
  # we need to set correct class as the current version of osfr does not works with dplyr properly
  class(df) <- c("osf_tbl_file","osf_tbl", class(df)) 
  stopifnot(nrow(df) == 1)
  df %>% 
    osf_ls_files() %>% 
    rowwise() %>% 
    do(osf_retrieve_file(.$id) %>% 
         osf_download(path = file.path(local_data_pth, df$name,.$name), 
                      overwrite = should_overwrite))
}

#' Create local file structure
#' As we have four experiments, we create individual directory for each of them
#' The function was written by Filip Děchtěrenko for another project.
#' 
#' @param pth path, in which the file structure will be created
#'
#' @return nothing, just the side effects
#' @export
#'
#' @examples
#' local_data_pth <- file.path("data","Source")
#' create_local_structure(local_data_pth) 
#'
create_local_structure <- function(pth) {
  # simple function for test-and-create behavior
  verbose_create <- function(f) {
    if(!dir.exists(f)) {
      message(sprintf("Directory %s does not exist, creating..", f))
      dir.create(f, recursive = T)  
    }
  }
  
  source_dir <- file.path(pth, "Source")
  raw_dir <- file.path(pth, "Raw")
  processed_dir <- file.path(pth, "Processed")
  
  verbose_create(source_dir)
  verbose_create(raw_dir)
  verbose_create(processed_dir)
}

#' Removes local data 
#' Just a simple wrapper around unlink function
#' The function was written by Filip Děchtěrenko for another project.
#'
#' @param local_data_pth path to downloaded data
#'
#' @return
#' @export
#'
#' @examples
#' local_data_pth <- file.path("data","Source")
#' create_local_structure(local_data_pth)
#' remove_local_data(local_data_pth)
remove_local_data <- function(local_data_pth) {
  unlink(local_data_pth, recursive = T)
}

#' Reads in and merge local csv files
#' The function reads in and merge individual locale csv files
#' into one tibble from different subdirectories. The function also
#' saves directory, subdirectory and file names as a variable.
#' 
#' @param pattern The pattern to look for when listing files in the locale directory.
#' @param path The path to the locale directory that contains the subdirectories.
#' @param subfolder_name A character vector of the subfolder names to look into.
#' @param exclude A character string. Filenames containing this string will not be read in.
#' @param sep Used as delim in read_delim.
#' 
#' @return All files that meets the criteria merged into a tibble from the specified subdirectory.
read_plus <- function(pattern, path, include = NULL, sep) {
  files <- tibble(list = list.files(path = path, pattern = pattern, full.names = T, recursive = T))
  
  if(!is.null(include)){
  files <- filter(files, str_detect(list, include))
  }
  
  files %>%
  pull(list) %>% 
  map_dfr(.,
          ~ read_csv(.x) %>% 
            mutate(filename = str_remove_all(.x, ".*/|.csv")))
}

#' Function to not include a vector in another vector
#' 
#' Retrieved from https://stackoverflow.com/questions/5831794/opposite-of-in
#' 
`%ni%` <- Negate(`%in%`)

#' Function to caluclate Bayes factors
#' 
## The function is retrieved from 
## https://link.springer.com/article/10.3758/s13423-017-1266-z

Bf <- function(sd, obtained, dfdata, meanoftheory, sdtheory, dftheory, tail = 2)
  
{
  
  area <- 0
  
  normarea <- 0
  
  theta <- meanoftheory - 10 * sdtheory
  
  incr <- sdtheory/200
  
  for (A in -2000:2000){
    
    theta <- theta + incr
    
    dist_theta <- dt((theta-meanoftheory)/sdtheory, df=dftheory)
    
    if(identical(tail, 1)){
      
      if (theta <= 0){
        
        dist_theta <- 0
        
      } else {
        
        dist_theta <- dist_theta * 2
        
      }
      
    }
    
    height <- dist_theta * dt((obtained-theta)/sd, df = dfdata)
    
    area <- area + height * incr
    
    normarea <- normarea + dist_theta*incr
    
  }
  
  LikelihoodTheory <- area/normarea
  
  Likelihoodnull <- dt(obtained/sd, df = dfdata)
  
  BayesFactor <- LikelihoodTheory/Likelihoodnull
  
  BayesFactor
  
}