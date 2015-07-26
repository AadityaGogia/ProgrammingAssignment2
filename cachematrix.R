## This program takes advantage of lexical scoping in R by calculating the inverse of a matrix only once (unless the matrix is changed).




## The makeCacheMatrix function returns a list of functions that can be used to interact with a matrix. 
## These include setting the matrix (set), retrieving the matrix (get),
## setting the inverse of the matrix (setinverse) and retrieving the inverse of the matrix (getinverse).
makeCacheMatrix <- function(x = matrix()) 
{
	#initializing the inverse of the matrix x
	inverse <- NULL

	#the function to set a new matrix x
	set <- function(y)
	{
		#assigning new value to x in the parent environment as well as the current environment
		x <<- y
		#assigning NULL to inverse so that the program knows that the inverse must be calculated, now that the matrix has been set
		inverse <<- NULL
	}
	
	#the function to retrieve the matrix
	get <- function()
	{
		x
	}

	#the function to set the inverse of the matrix
	setinverse <- function(i)
	{
		inverse <<- i
	}
	
	#the function to get the inverse of the matrix
	getinverse <- function()
	{
		inverse
	}

	#returning a list of all the functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## The cacheSolve function calculates and returns the inverse of the matrix IF it has not already been calculated, 
## otherwise it obtains the cached version of the inverse of the matrix and returns the same. 
cacheSolve <- function(x, ...) 
{
	#getting the inverse of the matrix
	i <- x$getinverse()

	#checking if the inverse of the matrix is NULL (i.e. whether the inverse of the matrix has been calculated before) 
	if(!is.null(i))
	{
		#if reached here then the inverse of the matrix is not NULL
		
		#acknowledge that the inverse of the matrix is being taken from the cache (i.e. not being calculated)
		print("Getting cached data")
		#return the inverse of the matrix
		return(i)
	}
	
	#if reached here then the inverse of the matrix is NULL (i.e. it has not been calculated yet)

	#get the matrix
	data <- x$get()
	
	#get the inverse of the matrix
	i <- solve(data)

	#set the inverse of the matrix
	x$setinverse(i)

	#return the calculated inverse of the matrix
	i
}
