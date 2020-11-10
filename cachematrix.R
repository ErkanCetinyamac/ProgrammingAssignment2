## Matrix inversions are usually inconvenient processes when it comes to large scale amount of data.

## Therefore with these functions we cache them rather than recomputed.

## This function creates a special "matrix" object that 
#can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    invert_Mtx <- NULL
    set_Mtx <- function (y) {
        x <<- y
        invert_Mtx <<- NULL
    }
    
    
    get_Mtx <- function () {
        x
    }

    set_Inverse <- function(inv) {
        invert_Mtx <<- inv
    }
    
    get_Inverse <- function() {
        invert_Mtx
    }
    
    list (set_Mtx = set_Mtx, get_Mtx = get_Mtx, set_Inverse = set_Inverse, get_Inverse = get_Inverse)
  
    }






## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    
    
    invert_Mtx <- x$get_Inverse()
    
    if(!is.null(invert_Mtx)) {
        
        message("displaying cached inverse..")
        
        return(invert_Mtx)
    }
    
    my_data <- x$get_Mtx()
    invert_Mtx <- solve(my_data, ...)
    x$set_Inverse(invert_Mtx)
    invert_Mtx
    
       
}
