#' Total Unduplicated Reach & Frequency Analysis
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param vars An optional vector of variable names that should be included in
#'   the TURF analysis. If \code{vars} is not specified, the function will
#'   default to running TURF across the entire dataset.
#'
#' @export
turf_analysis <- function(
    data,
    vars = NULL
){
  
  # If variables are specified, select those variables.
  # Otherwise, use specified data.
  if(!is.null(vars)){
    
    df <- data %>% 
      dplyr::select(
        dplyr::all_of(vars)
      )
    
  } else {
    
    df <- data
    
  }
  
  # Create working copy for TURF
  df_reach <- df
  
  # Create list to save
  reach_list <- vector("list",
                       ncol(df_reach))
  
  # Update DF reach
  for(i in seq_along(reach_list)){
    
    # Get Reach proportions
    reach_df <- dplyr::summarise(df_reach, 
                                 dplyr::across(
                                   .cols = dplyr::everything(), 
                                   .fns = function(x) mean(x,
                                                           na.rm = TRUE)))
    
    # Get name of max reach col
    max_col <- names(reach_df)[which.max(reach_df)]
    
    # Get reach
    reach_prop <- reach_df[[max_col]]
    reach_n <- reach_prop * nrow(df_reach)
    
    # Get Individual Item Reach
    ind_reach <- df %>% 
      dplyr::summarise(
        dplyr::across(
          .cols = dplyr::all_of(max_col),
          .fns = function(x) mean(x,
                                  na.rm = TRUE))) %>% 
      dplyr::pull(max_col)
    ind_n <- ind_reach * nrow(df)
    
    # Save
    reach_list[[i]] <- data.frame(
      Item = max_col,
      Individual_Reach_Prop = ind_reach,
      Individual_Reach_N = ind_n,
      Unduplicated_Reach_N = reach_n
    )
    
    # Update df reach
    df_reach <- df_reach %>% 
      dplyr::filter(!!rlang::sym(max_col) == 0)
    
    # Check if df_reach is empty or no one is reached, if TRUE, break loop.
    if(nrow(df_reach) == 0 | sum(df_reach) == 0){
      # Trim the list to the actual length
      reach_list <- reach_list[seq_len(i)]
      break
    }
    
  }
  
  # Reduce
  reach_tab <- purrr::reduce(reach_list,
                             rbind)
  
  # Get absolute reach of items not contributing
  non_reach <- df %>% 
    dplyr::select(-(base::unique(reach_tab$Item))) %>% 
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = mean(x, na.rm = TRUE)
      )
    )
  
  # Adjust table
  reach_tab_nons <- data.frame(Item = colnames(non_reach),
                               Individual_Reach_Prop = t(non_reach)) %>% 
    dplyr::mutate(Individual_Reach_N = Individual_Reach_Prop * nrow(df),
                  Unduplicated_Reach_N = 0) %>% 
    dplyr::arrange(dplyr::desc(Individual_Reach_Prop))
  
  # Compile together
  reach_table <- rbind(reach_tab,
                       reach_tab_nons) %>% 
    dplyr::mutate(Cumulative_Reach_N = cumsum(Unduplicated_Reach_N),
                  Cumulative_Reach_Prop = cumsum(Unduplicated_Reach_N) / nrow(df),
                  Unduplicated_Reach_Prop = Cumulative_Reach_Prop - lag(Cumulative_Reach_Prop,
                                                                        default = 0),
                  Total_Engagements = cumsum(Individual_Reach_N),
                  Avg_Engagements = Total_Engagements / nrow(df),
                  Frequency = Total_Engagements / Cumulative_Reach_N) %>% 
    dplyr::relocate(
      Unduplicated_Reach_Prop,
      .after = Unduplicated_Reach_N
    )
  rownames(reach_table) <- NULL
  
  # Return
  return(reach_table)
  
}



#' Total Unduplicated Reach & Frequency Optimizer
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param vars An optional vector of variable names that should be included in
#'   the TURF analysis. If \code{vars} is not specified, the function will
#'   default to running TURF across the entire dataset.
#' @param combos A vector of numeric values indicating the range of feature
#'   combinations to include in the TURF analysis. The default is \code{c(2:3)}.
#'   
#' @export
turf_optimize <- function(
    data,
    vars = NULL,
    combos = c(2:3)
){
  
  # If variables are specified, select those variables.
  # Otherwise, use specified data.
  if(!is.null(vars)){
    
    data <- data %>% 
      dplyr::select(
        dplyr::all_of(vars)
      )
    
  }
  
  
  # Create list to store TURF combo results
  turf_list <- lapply(
    combos,
    function(k){
      
      # Get every combination
      combinations <- utils::combn(
        colnames(data),
        k
      )
      
      # Get length
      combination_list <- lapply(
        seq(length(combinations)/k),
        function(c) {
          
          # Current combo iteration
          cb <- combinations[,c]
          
          # Get the relevant TURF results
          cb_turf <- turf_analysis(data = data,
                                   variables = cb)[1:k,]
          
          # Grab item combinations in TURF
          item_combinations <- cb_turf[["Item"]]
          
          # Initialize a data frame
          cb_df <- data.frame(Reach = cb_turf["Cumulative_Reach_Prop"][k,],
                              Engagements = cb_turf["Avg_Engagements"][k,],
                              Frequency = cb_turf["Frequency"][k,])
          
          # Create a list of columns
          cols <- lapply(1:k, function(i) {
            col_name <- paste0("Item_", sprintf("%01d", i))
            setNames(item_combinations[i], col_name)
          })
          
          # Bind all columns together into a data frame
          cb_df <- cbind(cb_df,
                         data.frame(t(unlist(cols))))
          
        }
      )
      
      # Reduce
      combo_table <- purrr::reduce(combination_list,
                                   rbind) %>% 
        dplyr::arrange(dplyr::desc(Reach))
      
    }
    
  )
  
  # Save to TURF list
  names(turf_list) <- paste0("Combo_",
                             combos)
  
  # Return
  return(turf_list)
  
}
