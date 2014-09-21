
# The solution is prepared based on the example for Assignment 2.
## The function "makeCacheMatrix" creates a matrix and cache its inverse.
## This is done by using <<- operator to assign the inverse of the matrix 
#to "mem_var_a" in an enviornment which can be accessed from the outside. 


makeCacheMatrix <- function(x = matrix()) {
  
    mem_Var_a <- NULL
    
    set <- function(y) {
      x <<- y
      mem_Var_a<<- NULL
    }
    get <- function() x
    set_matrix_inverted <- function(solve)  mem_Var_a <<- solve
    get_matrix_Invereted <- function() mem_Var_a
    list(set = set, get = get,
         set_matrix_inverted = set_matrix_inverted,
         get_matrix_Invereted = get_matrix_Invereted)
}
 ## The function "cacheSolve" first checks whether the inverse matrix 
 ## is already  available in the "mem_var_a" by testing the "if" condition.
 ## If available, it will return the message "getting cached data" and display
 ## the result. If not available it will compute and display the result.

cacheSolve <- function(x, ...) {

    mem_Var_a <- x$get_matrix_Invereted()
    
    if(!is.null(mem_Var_a)) {
      message("getting cached data")
      return(mem_Var_a)
    }
    
    data <- x$get()
    mem_Var_a <- solve(data, ...)
    x$set_matrix_inverted(mem_Var_a)
    mem_Var_a

}
