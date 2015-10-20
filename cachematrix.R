#makeCacheMatrix() takes an matrix as its only argument and returns a "CacheMatrix"
#object. If the argument x is not specified, an empty matrix is used as a default value.
#A "CacheMAtrix"object is a named list of four functions:
#get(), set(), getinverse(), setinverse()

#get() function has no arguments and returns the matrix 

#set() is used to assign the matrix. It takes the matrix as its only argument.

#getinverse() is used to retrieve the cached inverse matrix (if available)
#It has no arguments and returns the cached matrix, if it is availabale

#setinverse() is used to cache the inverse matrix
#It takes a matrix to be cached as its only argument.

#Once a "CacheMatrix" object is created, the four functions that it contains can be called
# by the names with the $ operator, for example:

# some_matrix <- matrix(c(1,2,3,4), nrow=2, ncol=2)
# x <- makeCacheMatrix()
# x$set(some_matrix)
# x$get()


#Please note that getinverse() and setinverse() do NOT calculate the inverse matrix, they
#just "save" and "load" the cached value. The inverse matrix is calculated in cacheSolve()




makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL #when we initalize a new CacheMAtrix object, the inverse is not specified

    set <- function (y) {
        x <<-y 
        inv <<- NULL #every time we change our matrix, the cached inverse is discarded
    }
    #note that <<- is used rather than <- , because the objects being assigned
    #belong to the parent environment (that of makeCacheMatrix())
    
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse 
    
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
    
}


# The cacheSolve function takes a "CacheMatrix" object as its first argument x.

#If any other arguments are specified, they are all passed on the solve() function via the ... argument

#The function first checks whether there is a cached value for the inverse, and if so,
#returns the cached value

#Otherwise, the function calls the get() function to retrieve the value of the matrix,
#calculates the inverse with solve() function, calls setmean() to cache the inverse
# and then returns the inverse it calculated.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()
    
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    
    
}
