#MANIPULATING LISTS
##############################################
# Question 4
#Any: takes list and predicate and returns TRUE if
# predicate is true for any of inputs
Any <- function(x, f){
  Reduce(`||`, f(x), init=TRUE)
}

#All: same as any but for all inputs
All <- function (x,f){
  Reduce(`&&`, f(x), init=TRUE)
}

# Question 5
# Span: list x and predicate f, return location of 
# longest sequential run where predicate is true
# Hint: rle()
span <- function(x,f){
  #Get length and values of runs
  rle <- rle(vapply(x, f, logical(1)))
  
  #get max length of TRUE run and find position
  which(rle$values) %>>%
  {max(rle$lengths[.])} %>>%
  {Position(function(i) rle$values[i] == TRUE &&
             rle$lengths[i] == . ,
           1:length(rle$values) )} %>>%

  #use reduce to get array of accumulated lengths
  #to there and return max +1
  {max(Reduce(`+`, rle$length[1:.-1] , 
         accumulate = TRUE, init = 0))+1} 
}

# Test
stopifnot(
  span(c(1,2,3,4,4,4), 
       function(x) x == 4) 
  == 4,
  span(c(1,2,2,2,2,1,2,2), 
       function(x) x%%2 == 0)
  == 2,
  span(c(1,4,1,2,2,2,4,4,2,4,4), 
       function(x) x == 4)
  == 7
)

#################################################
#MATHEMATICAL FUNCTIONALS

#Question 1
#arg_max: take fn and vector, return elements of
#inputs with highest fn outputs

arg_max <- function(x,f){
  fx <- f(x)
  which(fx == max(fx)) %>%
  x[.]
}

#Tests
stopifnot(
  arg_max(0:5, 
          function(x) x^2)
  == 5,
  arg_max(-5:5,
          function(x) x^2)
  == c(-5,5)
)

################################################
#A Family of Functions
#Question 1
#Implement smaller and larger. Given 2 inputs, return
#smaller and larger. Include na.rm = TRUE

smaller <- function(x,y, na.rm = TRUE){
   if(na.rm && (is.na(x) || is.na(y))){
    rm_na(x,y)
   }else(
    min(c(x,y))
  )
}

rm_na <- function(x,y){
  if(is.na(x) && is.na(y)){
    Inf
  } else if (is.na(x)){
    y
  } else{
    x
  }
}

#Test
stopifnot(
  smaller(1,2) 
  == 1,
  smaller(1, NA) 
  == 1,
  smaller(NA,1) 
  == 1,
  smaller(1, smaller(NA, NA, na.rm = TRUE), 
          na.rm = TRUE)
  == 1
)

#Use Smaller to implement min() and pmin()
Min <- function(x, na.rm = TRUE){
  stopifnot(is.numeric(x))
  Reduce(smaller,x, init = Inf)
}

stopifnot(
  Min(c(1,2,3,4)) == 1,
  is.na(Min(c(1,2,3,NA)))
)