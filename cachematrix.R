# cacheMatrix.R
# Caching the Inverse of a Matrix
# 
# @author: viperwyman
# @version: 1.2 (trying to commit and push to github)
# based on cacheMean.R by RPeng
#
# Background:
#   Matrix inversion is usually a costly computation and there may be some benefit 
#   to caching the inverse of a matrix rather than compute it repeatedly (there are 
#   also alternatives to matrix inversion that we will not discuss here). 
#
#   Computing the inverse of a square matrix can be done with the solve function in R. 
#   For example, if X is a square invertible matrix, then solve(X) returns its inverse.
#   This solution assumes that the matrix supplied is always invertible.
#
# This file contains a pair of functions that cache the inverse of a matrix.
#
# - makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
#
# - cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix. 
#               If the inverse has already been calculated (and the matrix has not changed),
#               then the cachesolve retrieves the inverse from the cache.

#
# makeCacheMatrix: creates an object that consists of 4 methods (set, get, setInverse, and
#                  getInverse,) a matrix, and the inverse of that matrix, if calculated.
#                  For example, consider a 2x2 matrix:
#                  | 3    3.5 |
#                  | 3.2  3.6 |
#
# To create this matrix in R as kmat:
# kmat<-matrix(c(3,3.2,3.5,3.6),ncol=2)
#
# Next, calling makeCacheMatrix to store this matrix:
#
# prog2<-makeCacheMatrix(kmat)
#
# yields an object, prog2, which has the 4 methods listed above plus the 2x2 matrix kmat.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() {
		x
	}

        setInverse <- function(solve) {
		inv <<- solve
	}

        getInverse <- function() {
		inv
	}

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# Given an object of the type created by the makeCacheMatrix() function, compute 
# and cache the inverse of the stored matrix. If the inverse has been stored 
# previously, that cached value is returned without computation.
#
# Calling cacheSolve(prog2) on the above created matrix caches the correct inverse matrix,
# which is 
#            | -9   8.75 |
#            |  8  -7.5  |
#
# Any additional arguments needed by the solve() function that may have been provided in th
# initial cacheSolve() call would be passed through to solve(), as seen in the line
#
# inv<-solve(data, ...)
#
# Note that, if an incorrect value has been stored previously by force, this function
# doesn't distinguish that and will return the bad value. For example, after creating
# prog2 as above, if this is called:

# badMat<-matrix(c(1,1,1,1),ncol=2)
# prog2$setInverse(badMat) 
# 
# then prog2$getInverse() will return the badMat matrix.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
	inv<-x$getInverse()

	# if the inverse has been cached, return it
	if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
	}

	# if the inverse hasn't been cached, the code will continue here:
        data <- x$get()
        inv <- solve(data, ...)

	# set the inverse in the initial object
        x$setInverse(inv)

	# return the inverse
        inv
}
