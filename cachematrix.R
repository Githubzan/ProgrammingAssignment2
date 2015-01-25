# Step 1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # sets x equal to an empty matrix
        Inv <- NULL
        # sets the inverse equal to NULL
        set <- function(y){
                x <<- y
                # set function assigns the argument of the function to equal x
                Inv <<- NULL
                # Post calling the set function, the Inverse ought to be re-set to NULL. Necessary in case we redefine the matrix. 
                
        }
        get <- function() x
        # get function actually returns the matrix
        setInverse <- function(solve) Inv <<- solve
        # setInverse supersedes the previous value of Inv and assigns the argument of the function to Inverse
        getInverse <- function() Inv
        # getInverse now returns the Inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        #  list creation of the functions
}


# Step 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        # retrieves the most recent value for the inverse
        if(!is.null(Inv)){
                message("getting cached data")
                return(Inv)
                # If the value of Inverse is not null (in other words, Inverse has already been calculated), then 
                #cacheSolve returns that value
        }
        # Otherwise, if the value of Inverse is NULL, then you retrieve matrix x and calculate the inverse
        #with the solve() function
        message("recently calculating data")
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        # Sets Inverse to the most recent calculated value
        Inv #Finally, it returns the new Inverse value
}

# Acceptance test

mat<-makeCacheMatrix() # Initialize
class(mat)             # Check the class of the object (list)
class(mat$set)         # Check that set is a function
mat$set(matrix(15:18,2,2)) # set the function with the simplest 2x2 matrix
mat$get()                # returns the above 'get'
cacheSolve(mat)          # returns the invertible matrix (where the first row equals -9 ; 8.5 and the 2nd row equals 8 ; -7.5 )


