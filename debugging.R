col_means <- function(df) {
  if (is.data.frame(df) != TRUE) stop("Argument must be a data frame")
  if (min(dim(df)) == 0) stop("Data frame supplied has dimension of length 0") 
  
  try(
    df <- lapply(df, as.numeric)
  )
  
  numeric <- vapply(df, is.numeric, logical(1))
  
  notNumeric <- length(numeric) - sum(numeric)
  if(notNumeric > 0){
    warning("Dropped ", notNumeric, " non-numeric columns.")
  }
  numeric_cols <- df[numeric]

  data.frame(lapply(numeric_cols, mean))
}


col_means(mtcars)
col_means(mtcars[, 0])
col_means(mtcars[0, ])
col_means(mtcars[, "mpg", drop = F])
col_means(1:10)

col_means(as.matrix(mtcars))
col_means(as.list(mtcars))


mtcars2 <- mtcars
mtcars2[-1] <- lapply(mtcars2[-1], as.character)
col_means(mtcars2)



lag <- function(x, n = 1L) {
  if(!is.atomic(x)) stop("Input supplied was not atomic vector.")
  if(n > length(x)) stop("Lag is greater than length of vector.")
  if(n==0) warning("Lag of 0 supplied, original vector returned.")
  
  
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}


