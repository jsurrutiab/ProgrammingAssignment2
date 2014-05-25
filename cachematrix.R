##the function makeCacheMatrix create a special object(a list ) that contains a squared matrix
## cacheSolve calculates the inverse matrix of the matrix stored in the list, this function checks if that inverse 
##has already calculate if this is true so it takes that inverse from cache, in other case then cacheSolve calculate the inverse
## for matrix that was used as argument in makeCacheMatrix and stores it in cache

  
##function makeCacheMatrix takes as argument a squared matrix
##and generates a list that contains four elements: set, get, getInverse, setInverse


makeCacheMatrix <- function(x = matrix()) {
	m<-NULL

	set<-function(y){

	x<<-y
	m<<-NULL
}
        get<-function() x
	setInverse<-function(solve)  m<<-solve
	getInverse<-function() m
	list(set=set, get=get,
	setInverse=setInverse,
	getInverse=getInverse	)

}


##function cacheSolve takes as argument a matrix squared also
##this funcion checks if the x$getInverse value is NULL, if it is true 
##that means the inverse of that matrix has not calculated yet then in m stores 
##the inverse of matrix x, finally that inverse is stored in cache and the m value is showed
##for the case when x$getInverse is not NULL then the inverse is took from cache

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)

}
	
	mat<-x$get()
	m<-solve(mat, ...)
	x$setInverse(m)
	m
}
