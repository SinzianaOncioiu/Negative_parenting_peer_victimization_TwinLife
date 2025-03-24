#------------FUNCTIONS WE ARE GOING TO USE------------

#count the number of missing for each variable
availb.count   <- function(df) { 
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= 0, nvalid, NA)}

#atleast n that are not missing
miss.n   <- function(df, n) { 
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, 999, NA)}

#mean across several waves with min number of availabe datapoints
mean.n   <- function(df, n) { 
  means <- apply(as.matrix(df), 1, mean, na.rm = TRUE)
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, means, NA)}

sum.n   <- function(df,n) { 
  sums <- apply(as.matrix(df), 1, sum, na.rm = TRUE)
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, sums, NA)}

#difference between two columns
diff.n <- function(df, cols, n) {
  # Ensure exactly two columns are selected
  if (length(cols) != 2) stop("Please select exactly two columns.")
  
  # Extract the two columns
  selected_data <- df[, cols]
  
  # Calculate row-wise differences (first column - second column)
  diffs <- selected_data[, 1] - selected_data[, 2]
  
  # Count the number of non-NA values in each row
  nvalid <- apply(selected_data, 1, function(row) sum(!is.na(row)))
  
  # Return differences if the number of valid values is >= n
  ifelse(nvalid >= n, diffs, NA)
}


#atleast n that are not missing
miss.n   <- function(df, n) { 
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, 999, NA)}

#mean across several waves with min number of availabe datapoints
mean.n   <- function(df, n) { 
  means <- apply(as.matrix(df), 1, mean, na.rm = TRUE)
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, means, NA)}

# max on a row 

max.row  <- function(df, a, z, n) { 
  maxim <- apply(df[, a:z], 1, max, na.rm = TRUE)
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, maxim, NA)}

# fequency for categorical variables, including the amount of missing data (n, %)
MISSDATA.count   <- function(df) { 
  nvalid <- apply(as.matrix(df), 2, function(df) tab1(df))
  print( nvalid)}

# Count missing data in a entire data base

propmiss <- function(dataframe,order=TRUE) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      Ntot=length(x),
      nMiss=sum(is.na(x)),
      nValid=length(x) - sum(is.na(x)),
      propMiss=round(100*sum(is.na(x))/length(x),digit=2)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  if (order==FALSE){
    return(d[, ])
  } else { 
    return(d[order(d$propMiss), ])
  }
}

#changing column data to row data
process_parenting <- function(data, col1, col2, target_col, reference_cols) {
  # Iterate through rows in steps of 3
  for (i in seq(1, nrow(data), by = 3)) {
    z <- i + 2  # Third row index in the group
    if (z <= nrow(data)) {  # Ensure `z` is within bounds
      rev <- -1 * data[z, col1]
      dif_fam <- sum(data[i, col1], rev, na.rm = TRUE)
      dif_fam2 <- sum(data[i + 1, col1], rev, na.rm = TRUE)
      
      # Assign values to the target column based on conditions
      if (dif_fam == 0) data[i, target_col] <- data[z, reference_cols[1]]
      if (dif_fam2 == 0) data[i + 1, target_col] <- data[z, reference_cols[2]]
    }
  }
  return(data)
}

#reversing function (when x is between 1 and 3)
reverse_score <- function(x) {
  ifelse(x %in% 1:3, 4 - x, NA)
}
#we choose which columns to reverse
columns_to_reverse <- c("int0106", "int0107", "int0106t", "int0107t", "int0106u", "int0107u")
#applying the function and changing names:
library(dplyr)

parent_items1 <- parent_items1 %>%
  mutate(across(all_of(columns_to_reverse), ~ reverse_score(.), .names = "{.col}_rev2"))


###################