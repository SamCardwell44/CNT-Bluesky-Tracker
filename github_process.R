install_packages <- function(){
  install.packages("curl")
  install.packages("jsonlite")
  install.packages("httr2")
  install.packages("atrrr")
  install.packages("dplyr")
  install.packages('readxl')
  install.packages('writexl')
}

load_packages <- function(){
  library(curl)
  library(httr2)
  library(atrrr)
  library(dplyr)
  library(readxl)
  library(writexl)
}

#install_packages()
load_packages()

auth_code <- Sys.getenv("BLUESKY_AUTH_CODE")


#use main account for auth
auth("climatenewstracker.org", auth_code, overwrite = TRUE)


get_info <- function(handle, limitnum = 10000, previous_data = NULL, retry_limit = 3, delay_sec = 5) {
  retries <- 0
  
  while (retries <= retry_limit) {
    result <- tryCatch({
      handle <- as.character(handle)
      
      # Get followers
      cat("Getting followers \n")
      followers <- get_followers(actor = handle, limit = limitnum)
      follower_count <- as.numeric(nrow(followers))
      if (is.na(follower_count)) follower_count <- 0
      if (follower_count == 0 && !is.null(previous_data)) {
        message("Follower count is 0, using previous day's value for ", handle)
        follower_count <- previous_data$followers
      }
      
      # Get posts
      cat("Getting posts \n")
      posts <- get_skeets_authored_by(actor = handle, limit = limitnum)
      post_count <- as.numeric(nrow(posts))
      if (is.na(post_count)) post_count <- 0
      
      #engagement columns are reply_count, repost_count, like_count, quote_count, bookmark_count
      cat("Getting engagement \n")
      replies <- sum(posts$reply_count, na.rm = TRUE)
      reposts <- sum(posts$repost_count, na.rm = TRUE)
      likes <- sum(posts$like_count, na.rm = TRUE)
      quotes <- sum(posts$quote_count, na.rm = TRUE)
      bookmarks <- sum(posts$bookmark_count, na.rm = TRUE)
      
      #mentions of CNT, Climate News Tracker, @climatenewstracker.org
      cat("Getting mentions \n")
      mentions <- dplyr::bind_rows(search_post(q= '@climatenewstracker.org', limit = 500), 
                                   search_post( q= '"Climate News Tracker"', limit = 500)) |> 
        dplyr::distinct() |>
        #filter for posts $indexed_at today
        filter(as.Date(indexed_at) == Sys.Date())
      
      mentions_count <- nrow(mentions)
      #make a list of handles that mentioned today as a string
      mentioned_handles <- paste(unique(mentions$author_handle), collapse = ", ")
        
      return(list(
        followers = paste(followers$actor_handle, collapse = ", "),
        follower_count = follower_count,
        posts = post_count,
        replies = replies,
        reposts = reposts,
        likes = likes,
        quotes = quotes,
        bookmarks = bookmarks,
        mentions = mentions_count,
        mentioned_by = mentioned_handles
      ))
      
    }, error = function(e) {
      message("Error in get_info for handle ", handle, ": ", e$message)
      return(NULL)  # Indicate failure
    })
    
    # If successful, return the result
    if (!is.null(result)) {
      return(result)
    }
    
    # If failed, retry
    if (retries < retry_limit) {
      retries <- retries + 1
      message(paste("Retrying (", retries, "/", retry_limit, ")...", sep = ""))
      Sys.sleep(delay_sec)  # Add delay before retrying
    } else {
      message("Max retries reached for handle ", handle, ". Returning default values.")
      break  # Exit the loop
    }
  }
  
  # Fail-safe return after all retries
  return(list(followers = NA,
              follower_count = NA,
              posts = NA,
              replies = NA,
              reposts = NA,
              likes = NA,
              quotes = NA,
              bookmarks = NA,
              mentions = NA,
              mentioned_by = NA))
}

daily_data <- get_info(handle = "climatenewstracker.org", limitnum = 1000)
cat("Info gathered \n")
#update the spreadsheet which has one row for each day, and then all the metrics as columns (If it does not exist create it)
update_data <- function(daily_data, filename = "bluesky_metrics"){
  current_date <- as.character(Sys.Date())
  filepath <- paste0(filename, ".csv")
  
  if (!file.exists(filepath)) {
    
    message("CSV does not exist. Creating ", filepath)
    
    baseline_row <- data.frame(
      date = current_date,
      followers = daily_data$followers,
      follower_count = daily_data$follower_count,
      new_followers = daily_data$followers,
      notable_followers = "",
      posts = daily_data$posts,
      replies = daily_data$replies,
      reposts = daily_data$reposts,
      likes = daily_data$likes,
      quotes = daily_data$quotes,
      bookmarks = daily_data$bookmarks,
      mentions = daily_data$mentions,
      mentioned_by = daily_data$mentioned_by,
      stringsAsFactors = FALSE
    )
    
    write.csv(
      baseline_row,
      filepath,
      row.names = FALSE
    )
    
    message("Created ", filepath)
    
    return(baseline_row)
  } 
  
  existing_data <- read.csv(filepath, stringsAsFactors = FALSE)
  cat("Calculating data difference \n")
  #for followers, likes, reposts, quotes, bookmarks, set the value to the difference from the previous row
  followers <- daily_data$followers
  new_followers <- daily_data$follower_count - tail(existing_data$follower_count, 1)
  
  new_replies <- daily_data$replies - tail(existing_data$replies, 1)
  new_reposts <- daily_data$reposts - tail(existing_data$reposts, 1)
  new_likes <- daily_data$likes - tail(existing_data$likes, 1)
  new_quotes <- daily_data$quotes - tail(existing_data$quotes, 1)
  new_bookmarks <- daily_data$bookmarks - tail(existing_data$bookmarks, 1)
  
  
  #for posts, mentions, set the value to the daily total
  new_posts <- daily_data$posts
  
  new_mentions <- daily_data$mentions
  mentioned_by <- daily_data$mentioned_by
  
  #set empty mentioned by to NA
  if (mentioned_by == "") {
    mentioned_by <- NA
  }
  
  
  #get the new followers that were not in the previous followers row
  previous_followers <- ifelse(
    is.na(tail(existing_data$followers, 1)),
    "",
    as.character(tail(existing_data$followers, 1))
  )
  
  new_followers <- setdiff(
    strsplit(daily_data$followers, ", ")[[1]],
    strsplit(previous_followers, ", ")[[1]]
  )
  #get notable follower list by looping through new followers and getting their follower count
  notable_followers <- c()
  cat("Calculating notable followers \n")
  for (follower in new_followers) {
    follower_info <- get_followers(follower, 1000)
    follower_count <- nrow(follower_info)
    if (!is.null(follower_info) && !is.na(follower_count) && follower_count > 100) {
      notable_followers <- c(notable_followers, paste(follower, "(", follower_count, ")", sep = ""))
    }
  }
  
  #write all this to a new row
  cat("Writing data \n")
  new_row <- data.frame(
    date = current_date,
    followers = followers,
    follower_count = daily_data$follower_count,
    new_followers = paste(new_followers, collapse = ", "),
    notable_followers = paste(notable_followers, collapse = ", "),
    posts = new_posts,
    replies = new_replies,
    reposts = new_reposts,
    likes = new_likes,
    quotes = new_quotes,
    bookmarks = new_bookmarks,
    mentions = new_mentions,
    mentioned_by = mentioned_by
  )
  
  #if today's date already exists, overwrite that row
  existing_date <- which(existing_data$date == current_date)
  
  if (length(existing_date) > 0) {
    cat("Overwriting today's data \n")
    existing_data[existing_date, ] <- new_row
  } else {
    cat("Creating new row \n")
    existing_data <- rbind(existing_data, new_row)
  }
  
  #write csv
  write.csv(existing_data, paste0(filename, ".csv"), row.names = FALSE) 
  
  
}
update_data(daily_data, filename = "bluesky_metrics")
cat("Process complete \n")
