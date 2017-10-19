#' Genetic Algorithm for Subset Selection
#'
#' This function identifies an optimal subset of a fixed size from a finite sequence (\code{1:n}).
#' A distributed multiple-population genetic algorithm (GA) is used to do subset selection
#' based on the maximization of a user-supplied fitness (objective) function.
#'
#' @param n 'integer'.
#'   Maximum permissible index, that is, the size of the integer sequence.
#'   The function chooses a subset of integers from \code{1:n}.
#' @param k 'integer'.
#'   Number of indices to choose, that is, the size of the subset.
#' @param Fitness 'function'.
#'   Fitness (or objective) function, any allowable \R function which
#'   takes as its first and second argument
#'   the binary \code{string} representing a potential solution and
#'   the maximum permissible index (\code{n}), respectively.
#'   The fitness function returns a single numerical value describing its \dQuote{fitness} score.
#'   Note that the \code{\link{DecodeChromosome}} function is provided to decode the binary string,
#'   see \sQuote{Examples} section.
#' @param ...
#'   Additional arguments to be passed to the fitness function.
#' @param popSize 'integer'.
#'   Population size
#' @param migrationRate 'numeric'.
#'   Proportion of individuals that should migrate between islands.
#' @param migrationInterval 'integer'.
#'   Number of iterations at which exchange of individuals takes place.
#'   This interval between migrations is called an \emph{epoch}.
#' @param pcrossover 'numeric'.
#'   Probability of crossover between pairs of chromosomes.
#' @param pmutation 'numeric'.
#'   Probability of mutation in a parent chromosome.
#' @param elitism 'integer'.
#'   Number of chromosomes to survive into the next generation.
#' @param maxiter 'integer'.
#'   Maximum number of iterations to run before the GA search is halted.
#' @param run 'integer'.
#'   Number of consecutive generations without any improvement in the
#'   \dQuote{best} fitness value before the GA is stopped.
#' @param suggestions 'matrix'.
#'   Initial population
#' @param parallel 'logical' or 'integer'.
#'   Whether to use parallel computing.
#'   This argument can also be used to specify the number of cores (and islands) to employ;
#'   by default, this is taken from \code{\link[parallel]{detectCores}}.
#' @param seed 'integer'.
#'   Random number generator state, used to replicate the results.
#'
#' @details The fitness function (see \code{Fitness} argument) is
#'   solved using the \code{\link[GA]{gaisl}} function in the \pkg{GA} package (Scrucca, 2013; Scrucca, 2016).
#'   The function implements the islands GAs approach.
#'   Independent GAs are configured to use integer chromosomes,
#'   where indices are represented as binary strings using \href{https://en.wikipedia.org/wiki/Gray_code}{Gray} encoding;
#'   linear-rank selection; uniform crossover; and uniform mutation.
#'
#' @return Returns a 'list' with components:
#'   \describe{
#'     \item{\code{call}}{original call which can be used for later re-use.}
#'     \item{\code{solution}}{a 'matrix' representation of the best solution found.
#'       Each row represents a unique solution giving the best fitness at the final iteration.
#'       More than one row indicates a non-unique solution.
#'       The number of columns is equal to the subset size (\code{k}).}
#'     \item{\code{ga_output}}{output from the GA,
#'       see \code{\link[=gaisl-class]{gaisl-class}} for format description.}
#'     \item{\code{ga_time}}{time required to run the GA,
#'       see \code{\link{system.time}} for details.}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references
#'   Scrucca, Luca, 2013, GA: A Package for Genetic Algorithms in R:
#'   Journal of Statistical Software, v. 53, no. 4, p. 1-37.
#'
#'   Scrucca, Luca, 2017, On some extensions to GA package: hybrid optimisation,
#'   parallelisation and islands evolution: The R Journal, v. 9, no. 1, 187-206.
#'
#' @keywords optimize
#'
#' @export
#'
#' @examples
#' # Choose the 4 smallest numbers from a list of 100 values
#' # genearated from a standard uniform distribution.
#' k <- 4
#' n <- 100
#' numbers <- sort(runif(n))
#' Fitness <- function(string, n, numbers) {
#'   idxs <- DecodeChromosome(string, n)
#'   score <- -sum(numbers[idxs])
#'   return(score)
#' }
#' \dontrun{
#' out <- FindOptimalSubset(n, k, Fitness, numbers, elitism = 1, seed = 321)
#' print(out[["solution"]])
#' plot(out[["ga_output"]])
#' summary(out[["ga_output"]])
#' print(out[["ga_output"]]@fitnessValue)
#' }
#'

FindOptimalSubset <- function(n, k, Fitness, ..., popSize=50L, migrationRate=0.1,
                              migrationInterval=10L, pcrossover=0.8, pmutation=0.1,
                              elitism=0L, maxiter=100L, run=maxiter, suggestions=NULL,
                              parallel=FALSE, seed=NULL) {

  # check arguments
  checkmate::assertInt(n, lower=2)
  checkmate::assertInt(k, lower=1, upper=n - 1)
  checkmate::assertFunction(Fitness)
  checkmate::assertInt(popSize, lower=1)
  checkmate::assertNumber(migrationRate, lower=0, upper=1, finite=TRUE)
  checkmate::assertInt(migrationInterval, lower=1)
  checkmate::assertNumber(pcrossover, lower=0, upper=1, finite=TRUE)
  checkmate::assertInt(elitism, lower=0, upper=popSize)
  checkmate::assertInt(maxiter, lower=1)
  checkmate::assertInt(run, lower=1, upper=maxiter)
  checkmate::assertMatrix(suggestions, null.ok=TRUE)
  checkmate::qassert(parallel, c("b1", "x1"))
  checkmate::assertInt(seed, null.ok=TRUE)

  # calculate number of bits
  nBits <- length(GA::decimal2binary(n)) * k

  # set number of islands
  if (is.logical(parallel)) {
    numIslands <- if (parallel) parallel::detectCores() else 4L
  } else if (is.numeric(parallel)) {
    numIslands <- parallel
  }

  # solve genetic algorithm
  ga_time <- system.time({
    ga_output <- GA::gaisl(type="binary",
                           fitness=Fitness,
                           n=n,
                           ...,
                           nBits=nBits,
                           population=function(object) {
                             .Population(object, n=n)
                           },
                           crossover=function(object, parents) {
                             .Crossover(object, parents, n=n)
                           },
                           mutation=function(object, parent) {
                             .Mutate(object, parent, n=n)
                           },
                           popSize=popSize,
                           numIslands=numIslands,
                           migrationRate=migrationRate,
                           migrationInterval=migrationInterval,
                           pcrossover=pcrossover,
                           pmutation=pmutation,
                           elitism=elitism,
                           maxiter=maxiter,
                           run=run,
                           suggestions=suggestions,
                           parallel=parallel,
                           seed=seed)
  })


  # decode solution
  FUN <- function(i) sort(DecodeChromosome(i, n))
  m <- t(apply(ga_output@solution, 1, FUN))
  solution <- m[!duplicated(m), , drop=FALSE]

  # bundle output
  return(list(call=match.call(),
              solution=solution,
              ga_output=ga_output,
              ga_time=ga_time))
}

.BuildChromosomes <- function(x, n, k) {
  FUN <- function(i) sort(sample(seq_len(n), k, replace=FALSE))
  return(t(vapply(seq_len(x), FUN, rep(0, k))))
}

.Population <- function(object, n) {
  k <- object@nBits / length(GA::decimal2binary(n))
  pop <- .BuildChromosomes(object@popSize, n, k)
  if (object@popSize < choose(n, k)) {
    dups <- which(duplicated(t(apply(pop, 1, sort))))
    i <- 1L
    repeat {
      len <- length(dups)
      if ((len) == 0) break
      pop[dups, ] <- .BuildChromosomes(len, n, k)
      dups <- which(duplicated(t(apply(pop, 1, sort))))
      if ((i <- i + 1L) > 1000) stop("Runnaway loop")
    }
  }
  FUN <- function(i) EncodeChromosome(i, n)
  population <- t(apply(pop, 1, FUN))
  return(population)
}

.Mutate <- function(object, parent, n) {
  FUN <- function(i) sort(DecodeChromosome(i, n))
  m <- t(apply(object@population, 1, FUN))
  encoded_parent <- object@population[parent, ]
  decoded_parent <- DecodeChromosome(encoded_parent, n)
  idxs <- seq_len(n)[-decoded_parent]
  j <- sample(seq_along(decoded_parent), size=1)
  i <- 1L
  repeat {
    x <- decoded_parent
    x[j] <- sample(idxs, size=1)
    x <- sort(x)
    if (!any(apply(m, 1, function(y) identical(x, y)))) break
    if ((i <- i + 1L) > 1000) stop("Runnaway loop")
  }
  mutated <- EncodeChromosome(x, n)
  return(mutated)
}

.Crossover <- function(object, parents, n) {
  fitness_parents <- object@fitness[parents]
  encoded_parents <- object@population[parents, , drop=FALSE]
  FUN <- function(i) DecodeChromosome(i, n)
  decoded_parents <- t(apply(encoded_parents, 1, FUN))
  len <- ncol(decoded_parents)

  combo <- unique(as.vector(decoded_parents))
  c1 <- sort(sample(combo, len))
  c2 <- sort(sample(combo, len))
  decoded_children <- matrix(c(c1, c2), nrow=2, ncol=len, byrow=TRUE)

  FUN <- function(i) sort(DecodeChromosome(i, n))
  m <- t(apply(object@population, 1, FUN))
  fitness_children <- rep(as.numeric(NA), 2)
  FUN <- function(i) identical(i, decoded_children[1, ])
  fitness_children[1] <- object@fitness[which(apply(m, 1, FUN))[1]]
  FUN <- function(i) identical(i, decoded_children[2, ])
  fitness_children[2] <- object@fitness[which(apply(m, 1, FUN))[1]]

  FUN <- function(i) EncodeChromosome(i, n)
  encoded_children <- t(apply(decoded_children, 1, FUN))
  return(list(children=encoded_children, fitness=fitness_children))
}


#' Encode and Decode an Integer Chromosome
#'
#' Functions for encoding and decoding an integer chromosome,
#' a set of integer parameter values that define a proposed solution to the problem
#' that a genetic algorithm is trying to solve.
#'
#' @param x 'numeric'.
#'   Binary representation, a vector of \code{0}s and \code{1}s, of the integer chromosome.
#' @param n 'integer'.
#'   Maximum permissible number in the integer chromosome.
#'
#' @return
#'   \code{EncodeChromosome} returns a 'numeric' vector of \code{0}s and \code{1}s, that is,
#'     the binary representation of the integer chromosome.
#'
#'   \code{DecodeChromosome} returns a 'numeric' vector of
#'     parameter values in the integer chromosome.
#'
#' @export
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords internal
#'
#' @examples
#' string <- EncodeChromosome(c(4, 196, 67), 255)
#' print(string)
#' print(DecodeChromosome(string, 255))
#'

EncodeChromosome <- function(x, n) {
  len <- length(GA::decimal2binary(n))
  FUN <- function(i) GA::binary2gray(GA::decimal2binary(i, len))
  return(unlist(lapply(x, FUN)))
}

#' @rdname EncodeChromosome
#' @param y 'numeric'.
#'   Integer chromosome, a vector of parameter values.
#' @export

DecodeChromosome <- function(y, n) {
  len <- length(GA::decimal2binary(n))
  FUN <- function(i) GA::binary2decimal(GA::gray2binary(y[i:(i + len - 1L)]))
  return(vapply(seq(1, length(y), by=len), FUN, 0))
}
