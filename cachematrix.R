## Two functions makeCacheMatrix and cacheSolve for inverse of the matrix and its retrieval from cache if its available
## Write a short comment describing this function
##This function supplies setter and getter methods for the
###original matrix and the inverse
makeCacheMatrix <- function(x = matrix()) {
matrix <- NULL
setMatrix <- function(y) {
x <<- y
matrix <<- NULL
}
getMatrix <- function() x
setMatrixInverse <- function(cacheMatrix) matrix <<- cacheMatrix
getMatrixInverse <- function() matrix
list(setMatrix = setMatrix, getMatrix = getMatrix,
setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}
## Write a short comment describing this function
## This function verifies if matrix available in the cache ,
## if its available in the cache then return cache object, other wise calculates
cacheSolve <- function(x, ...) {
matrix <- x$getMatrixInverse()
if(!is.null(matrix)) {
message("getting cached data")
return(matrix)
}
data <- x$getMatrix()
cacheMatrix <- solve(data, ...)
x$setMatrixInverse(cacheMatrix)
cacheMatrix
}