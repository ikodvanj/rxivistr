#' @title Search Rxivist
#' @description Retrieves a articles with matching description
#'
#' @param search_phrase Search phrase used for finding articles. Can be left empty.
#' @param from Defines what timeframe to search. Can be alltime, ytd, lastmonth, day, week, month, year. Lastmonth and ytd can only be used with sortby set as downloads
#' @param category Filters out results not related to the specified category. Category list can be viewed with category_list function. If filtering for a single category, this category can be specified as a string. If filtering for multiple categories, argument must be specified as a vector, e.g. category = c("zoology","biophysics").
#' @param sortby Sorts the results based on the number of downloads or twitter statistics.
#' @param limit Number of results to retrieve.
#'
#' @return Returns a data frame with articles matching specified search criteria.
#'
#' @importFrom curl has_internet
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' rxivist_search(search_phrase = "T-cells", from = "alltime", sortby = "twitter", limit = 5)
rxivist_search <- function(search_phrase = "", from = "alltime", category = "", sortby = "downloads", limit = 20) {
  ## check internet connection
  if(!(has_internet())){
    stop("No internet connection.")
  }

  ## check arguments
  check_args(from, category, sortby)

  url <- paste0("https://api.rxivist.org/v1/papers?q=", search_phrase, "&timeframe=", from, "&sortby=", sortby,
                "&page_size=", limit, paste0("&category=", category))
  fetch <- GET(url)
  results <- fromJSON(rawToChar(fetch$content))[["results"]]
  return(results)
}



#' @title Category_list
#' @description Lists all categories
#'
#' @return Returns a list with all article categories.
#'
#' @importFrom curl has_internet
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' category_list()
category_list <- function(){
  if(!(has_internet())){
    stop("No internet connection.")
  }
  return(fromJSON(rawToChar(GET("https://api.rxivist.org/v1/data/categories")[["content"]])))
}


#' @title rxivist_stats
#' @description Returns statistics on number of articles indexed by rxivist.
#'
#' @return Returns a list with rxivist stats.
#'
#' @importFrom curl has_internet
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' rxivist_stats()
rxivist_stats <- function(){
  if(!(has_internet())){
    stop("No internet connection.")
  }
  return(fromJSON(rawToChar(GET("https://api.rxivist.org/v1/data/stats")[["content"]])))
}


#' @title article_details
#' @description Retrieves data about a single paper and all of its authors
#'
#' @param ID Rxivist paper ID.
#'
#' @return Returns a list with details about the specified article.
#'
#' @importFrom curl has_internet
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' article_details(72514)
article_details <- function(ID) {

  ## check internet connection
  if(!(curl::has_internet())){
    stop("No internet connection.")
  }

  ## check arguments
  if(missing(ID)){stop("ID is missing.")}

  url <- paste0("https://api.rxivist.org/v1/papers/",ID)
  fetch <- GET(url)
  results <- fromJSON(rawToChar(fetch$content))
  return(results)
}



#' @title article_downloads
#' @description Returns monthly download statistics for articles.
#'
#' @param ID Rxivist paper ID.
#'
#' @return Returns a data frame with information about monthly downloads for the specified article.
#'
#' @importFrom curl has_internet
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' article_downloads(72514)
article_downloads <- function(ID) {

  ## check internet connection
  if(!(curl::has_internet())){
    stop("No internet connection.")
  }

  ## check arguments
  if(missing(ID)){stop("ID is missing.")}

  url <- paste0("https://api.rxivist.org/v1/downloads/",ID)
  fetch <- GET(url)
  results <- fromJSON(rawToChar(fetch$content))
  return(results$results)
}



#' @title authors_rank
#' @description Returns top 200 authors in specified category.
#'
#' @param category Filters out results not related to the specified category. Category list can be viewed with category_list function. If filtering for a single category, this category can be specified as a string. If filtering for multiple categories, the argument must be specified as a vector, e.g. category = c("zoology","biophysics"). If it is left as default, the function will return top 200 authors overall.
#'
#' @return Returns a data frame with the top 200 authors.
#'
#' @importFrom curl has_internet
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' authors_rank("plant-biology")
authors_rank <- function(category = ""){
  ## check internet connection
  if(!(curl::has_internet())){
    stop("No internet connection.")
  }

  ## check arguments
  check_args(category = category)

  url <- paste0("https://api.rxivist.org/v1/authors?", paste0("&category=", category))

  fetch <- GET(url)
  results <- fromJSON(rawToChar(fetch$content))[["results"]]
  return(results)

}


#' @title author
#' @description Provides information about the specified author.
#'
#' @param ID Rxivist author ID.
#'
#' @return Returns a data frame with information about the specified author.
#'
#' @importFrom curl has_internet
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' author(206102)
author <- function(ID){

  ## check internet connection
  if(!(curl::has_internet())){
    stop("No internet connection.")
  }

  ## check arguments
  if(missing(ID)){stop("ID is missing.")}

  url <- paste0("https://api.rxivist.org/v1/authors/",ID)
  fetch <- GET(url)
  results <- fromJSON(rawToChar(fetch$content))
  return(results)

}



#' @title check_args
#' @description Check arguments.
#'
#' @param from from argument
#' @param category category argument
#' @param sortby sortby argument
#'
#' @return Stops main function.
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#'
check_args <- function(from, category, sortby){
  if(!missing(from)){
    if(!(from %in% c("day", "month", "year", "ytd", "alltime"))){
      stop("from should be day, month, year or ytd")
    }
  }
  if(!missing(sortby)){
    if(!(sortby %in% c("twitter", "downloads"))){
      stop("sortby should be twitter or downloads")
    }
  }
  if(!missing(category)){
    categories <- fromJSON(rawToChar(GET("https://api.rxivist.org/v1/data/categories")[["content"]]))
    if(!(category %in% categories$results | category == "")){
      stop("Category should be ", categories)
    }
  }
}
