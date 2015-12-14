rm(list=ls())

# e.g. Find the minimum of f(x) = x^2/10 -2*sin(x) in [0, 4].
f = function(x) {
  return(x^2/10 - 2*sin(x))
}

# Golden section search works for a unimodal function f().
#
# Here is the code without any graphing.
golden.section.search = function(f, lower, upper, tol=0.001, ...) {
  phi = (1 + sqrt(5))/2 # golden.ratio (allows cutting 1/2 of calls to f())
  repeat {
    range = upper - lower
    lower.middle = upper - (phi - 1)*range
    upper.middle = lower + (phi - 1)*range
    if (range < tol) {
      return(lower)
    }
    if (f(lower.middle, ...) < f(upper.middle, ...)) {
      upper = upper.middle
    } else {
      lower = lower.middle
    }
  }
}

# Here is the code again, this time code added to make a graph and
# show the progress of the search.
golden.section.search = function(f, lower, upper, tol=0.001, ...) {
  phi = (1 + sqrt(5))/2 # golden.ratio (allows cutting 1/2 of calls to f())
  curve(f, lower, upper) # for graph
  i = 0 # for graph
  repeat {
    range = upper - lower
    lower.middle = upper - (phi - 1)*range
    upper.middle = lower + (phi - 1)*range
    if (i < 4) { # for graph
      points(x=c(lower, lower.middle, upper.middle, upper), y=c(i,i,i,i), col=i+1)
    }
    if (range < tol) {
      abline(v=lower, col="orange") # for graph
      return(lower)
    }
    cat(sep="", "lower=", lower, ", lower.middle=", lower.middle,
        ", upper.middle=", upper.middle, ", upper=", upper, "\n")
    if (f(lower.middle, ...) < f(upper.middle, ...)) {
      upper = upper.middle
    } else {
      lower = lower.middle
    }
    i = i + 1 # for graph
    scan(what=character(), n=1, quiet=TRUE) # Require "Enter" to move.
  }
}

g = golden.section.search(f, 0, 4)
print(g)
