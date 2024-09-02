#' Inject Random Missingness
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param prop A numeric value indicating what proportion of the data should be
#'   injected with missingness.
#' @param vars An optional vector of variable names that should be injected with
#'   missingness. If \code{vars} is not specified, the function will default to
#'   injecting missingness across the entire dataset.
#' @param method A character value indicating whether to determine random
#'   proportion of missingness by column or row. Options include \code{"row"} to
#'   inject missingness by row or \code{"column"} to inject missingness by
#'   column.
#'
#' @export
inject_na <- function(
    data = .,
    prop = .10,
    vars = NULL,
    method
) {
  
  # If no variables are specified, then get all variables in dataframe
  names <- if (is.null(vars)) {
    
    names(data)
    
  } else {
    
    vars
    
  }
  

  # METHOD - BY ROW ---------------------------------------------------------

  if (method == "row") {
    
    # For each row in the dataframe
    na_df <- lapply(
      seq(nrow(data)),
      function(i) {
        
        # Sample one row with replacement
        row <- data[i,]
        
        # Randomly select n% of the columns to be NA
        row[sample(names,
                   size = round(prop * length(names)))] <- NA
        
        # Return
        return(row)
        
      }
      
    )
    
    # Reduce
    na_df <- purrr::reduce(
      na_df,
      rbind
    )
    
  }
  
  

  # METHOD - BY COLUMN ------------------------------------------------------

  if (method == "column") {
    
    na_df <- data
    
    for (i in names) {
      
      # Number of rows to set as NA
      num_na <- round(prop * nrow(na_df))
      
      # Randomly select rows to set as NA
      na_rows <- sample(
        seq_len(nrow(na_df)),
        size = num_na
      )
      
      # Set the selected rows as NA for the current column
      na_df[na_rows, i] <- NA
      
    }
    
  }
  
  

  # RETURN ------------------------------------------------------------------

  # Return
  return(na_df)
  
}
