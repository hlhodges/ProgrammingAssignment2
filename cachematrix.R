## makeCacheMatrix creates an object that has functionality
## to set value of and return both a matrix and its inverse
## and to store the inverse

## POST-CONDITION: returns a matrix-like object composed of
## 2 lists of 2 functions each

makeCacheMatrix <- function(x = matrix()) {
     cached_inverse <- NULL
     set_matrix <- function(input_matrix) {
          x <<- input_matrix
          ## re-set the cached inverse because a new matrix is being set
          cached_inverse <<- NULL
     }
     get_matrix <- function() x
     set_inverse <- function(input_inverse) cached_inverse <<- input_inverse
     get_inverse <- function() cached_inverse
     # first element of output object
     column_1 <- list(setm = set_matrix, getm = get_matrix)
     # second element output object
     column_2 <- list(seti = set_inverse, geti = get_inverse)
     # output
     matrix_of_lists <- c( column_1, column_2 )
     matrix_of_lists
}


## cacheSolve returns the inverse of the matrix associated with
## makeCacheMatrix which, if not cached, is calculated via the
## function elements of makeCacheMatrix

## PRE-CONDITION: input must be an instance of makeCacheMatrix
cacheSolve <- function(matrix_of_func, ...) {

     inverse = matrix_of_func$geti()
     if( !is.null( inverse ) ) # if inverse is cached
     {
          message("Retrieving cached inverse...")
          return(inverse)
     }

     # if index is not cached...
     # PRE-CONDITION: matrix must be invertible
     # POST-CONDITION: the inverse is cached
     inverse <- solve(matrix_of_func$getm())
     matrix_of_func$seti(inverse)
     inverse
}
## rounds very small elements down to 0
## simplifies examination of matrices with small values
## of many significant figures
show_zeros <- function(x){
     threshold <<- 0.0001

     ## if all elements are larger than threshold, return input
     ## else set each element < threshold to zero and return

     if( !all( a <- abs(x) > threshold ) )
          x[!a] <- 0

     x
}