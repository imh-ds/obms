#' Inject Random Noise
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param iter A numeric value indicating how many iterations of noise injection
#'   should be applied. The default is \code{1}.
#' @param vars An optional vector of variable names that should be injected with
#'   random noise. If \code{vars} is not specified, the function will default to
#'   injecting random noise across the entire dataset.
#' @param noise_value A numeric value indicating the absolute range of noise
#'   from \code{0} to \code{X} to be added or subtracted as noise. The default
#'   is \code{1}.
#' @param digits A numeric value indicating the number of digits to round to.
#'
#' @export
inject_noise <- function(
    data = .,
    iter = 1,
    vars = NULL,
    noise_value = 1,
    digits = NA
) {
  
  # If no variables are specified, then get all variables in dataframe
  names <- if (is.null(vars)) {
    
    names(data)
    
  } else {
    
    vars
    
  }
  
  
  # Iterate noise injection n times
  for (n in iter) {
    
    # Loop over each column in the dataframe
    for (col_name in names) {
      
      # Check if the column is numeric
      if (is.numeric(data[[col_name]])) {
        
        # Loop over each value in the column
        for (i in 1:length(data[[col_name]])) {
          
          # Get the range of values in the column
          min_val <- min(data[[col_name]], na.rm = TRUE)
          max_val <- max(data[[col_name]], na.rm = TRUE)
          
          # If the value is NA, skip to the next iteration
          if (is.na(data[i, col_name])) {
            next
          }
          
          # Randomly add noise
          else {
            
            data[i, col_name] <- data[i, col_name] + runif(1,
                                                           min = -noise_value,
                                                           max = noise_value)
            
            # Cap ceiling
            if (data[i, col_name] > max_val) {
              data[i, col_name] <- max_val
            }
            
            # Cap floor
            if (data[i, col_name] < min_val) {
              data[i, col_name] <- min_val
            }
            
            # If rounding
            if (!is.na(digits)) {
              
              data[i, col_name] <- round(data[i, col_name],
                                         digits = digits)
              
            }
            
          }

          
        }
      }
    }
    
  }
  
  
  # Return the modified dataframe
  return(data)
  
}