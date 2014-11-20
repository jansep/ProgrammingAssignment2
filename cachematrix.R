##-----------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------
## Programming Assignment 2: Lexical Scoping
## The two functions below are used to create, store, retrieve and modify a matrix and cache 
## the inverse of the matrix in memory for retrieval.   
##-----------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------
## makeCacheMatrix sets up the functions needed to create (set) and retrieve (get) the matrix 
## and to create (setinverse) and retrieve (getinverse)the matrix inverse
##-----------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	## Set a new matrix  and clear the cache
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	## Get/display the matrix
	get <- function() x
	## Store the inverse in cache
	setinverse <- function(solve) m <<- solve
	## Get/display the inverse matrix
	getinverse <- function() m

	## Vector of all the functions above
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##-----------------------------------------------------------------------------------
## cacheSolve determines if a the inverse of a matrix is cached.
## If it is cached the inverse is returned, otherwise the inverse is calculated
## and cached. 
##-----------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
	m <- x$getinverse()

	## Check to see if the inverse is already cached.
	## If so, returned the cached value
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	## Create the cache
	## Get the matrix
	data <- x$get()

	## Calculate the inverse
	m <- solve(data, ...)

	## Assign the inverse to the cache
	x$setinverse(m)

	## Display the inverse
	m
}
