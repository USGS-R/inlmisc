#' Genetic Algorithm for Subset Selection
#'
#' This function identifies an optimal subset of a fixed size \code{k} from a finite sequence of length \code{n}.
#' A distributed multiple-population genetic algorithm (GA) is used to
#' do subset selection based on the maximization of a user-supplied fitness function.
#'
#' @param n 'integer'.
#'   Maximum permissible index, that is, the length of the finite sequence (\code{1:n}).
#'   The GA chooses a subset from this sequence.
#' @param k 'integer'.
#'   Number of indices to choose, that is, the fixed size of the subset.
#' @param Fitness 'function'.
#'   Fitness function, also known as the objective function, is any allowable \R function which
#'   takes as its first argument the binary \code{string} representing a potential solution.
#'   And as its second argument the maximum permissible index, \code{n}.
#'   Use the \code{\link{DecodeChromosome}(string, n)} command to decode the binary \code{string}.
#'   The fitness function returns a single numerical value describing its fitness score.
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
#'   \dQuote{best} fitness score before the GA is stopped.
#' @param suggestions 'matrix'.
#'   Binary representation of chromosomes to be included in the initial population.
#' @param parallel 'logical' or 'integer'.
#'   Whether to use parallel computing.
#'   This argument can also be used to specify the number of cores (and number of islands) to employ;
#'   by default, this is taken from \code{\link[parallel]{detectCores}}.
#' @param seed 'integer'.
#'   Random number generator state, used to replicate the results.
#'
#' @details The fitness function (see \code{Fitness} argument) is
#'   solved using the \code{\link[GA]{gaisl}} function in the \pkg{GA} package (Scrucca, 2013; Scrucca, 2016).
#'   The function implements an islands evolution model
#'   (Cohoon and others, 1987; Luke, 2013, p. 103-104; Scrucca, 2016, p. 197-200).
#'   Independent GAs are configured to use integer chromosomes;
#'   that is, indices are represented as binary strings using \href{https://en.wikipedia.org/wiki/Gray_code}{Gray} encoding.
#'   GA operators include linear-rank selection, uniform crossover, and uniform mutation.
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
#'   Cohoon, J.P., Hegde, S.U., Martin, W.N., and Richards, D., 1987,
#'   Punctuated Equilibria: A Parallel Genetic Algorithm,
#'   in Genetic Algorithms and their Applications:
#'   Proceedings of the Second International Conference on Genetic Algorithms,
#'   Grefenstette, J.J., Lawrence Earlbaum Associates, p. 155-161.
#'
#'   Luke, Sean, 2015, Essentials of metaheuristics (2nd ed.): Lulu, 263 p.,
#'   available for free at \url{https://cs.gmu.edu/~sean/book/metaheuristics/}.
#'
#'   Scrucca, Luca, 2013, GA: A Package for Genetic Algorithms in R:
#'   Journal of Statistical Software, v. 53, no. 4, p. 1-37.
#'
#'   Scrucca, Luca, 2017, On some extensions to GA package: hybrid optimisation,
#'   parallelisation and islands evolution: The R Journal, v. 9, no. 1, p. 187-206.
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
#' out <- FindOptimalSubset(n, k, Fitness, numbers, elitism = 1, run = 10, seed = 321)
#' print(out[["solution"]])
#' plot(out[["ga_output"]])
#' summary(out[["ga_output"]])
#' print(out[["ga_output"]]@fitnessValue)
#' }
#'

FindOptimalSubset <- function(n, k, Fitness, ..., popSize=100L,
                              migrationRate=0.1, migrationInterval=10L,
                              pcrossover=0.8, pmutation=0.1, elitism=0L,
                              maxiter=1000L, run=maxiter, suggestions=NULL,
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

  # set number of islands
  if (is.logical(parallel)) {
    numIslands <- if (parallel) parallel::detectCores() else 4L
  } else if (is.numeric(parallel)) {
    numIslands <- parallel
  }

  # calculate number of bits in the binary string representing the chromosome
  nBits <- ceiling(log2(n + 1)) * k

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

.BuildChromosomes <- function(size, n, k) {
  v <- seq_len(n)
  if (n < size * k) v <- rep(v, length.out=size * k)
  v <- sample(v, size * k)
  m <- matrix(v, nrow=size, ncol=k)
  m <- t(apply(m, 1, sort))
  return(m)
}

.Population <- function(object, n) {
  k <- object@nBits / ceiling(log2(n + 1))
  pop <- .BuildChromosomes(object@popSize, n, k)
  if (object@popSize < choose(n, k)) {
    i <- 0L
    repeat {
      if ((i <- i + 1L) > 1000) stop("Runnaway loop")
      dups <- which(duplicated(pop) | apply(pop, 1, function(v) any(duplicated(v))))
      if ((ndups <- length(dups)) == 0) break
      pop[dups, ] <- .BuildChromosomes(ndups, n, k)
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
  i <- 0L
  repeat {
    if ((i <- i + 1L) > 1000) stop("Runnaway loop")
    x <- decoded_parent
    x[j] <- sample(idxs, size=1)
    x <- sort(x)
    if (!any(apply(m, 1, function(y) identical(x, y)))) break
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
#' Functions for encoding and decoding a chromosome represented by integer values.
#' Where a chromosome is a set of numbers that defines a proposed solution to the
#' problem that a genetic algorithm is trying to solve.
#'
#' @param x 'numeric'.
#'   Integer representation of chromosome, a vector of integer values.
#' @param n 'integer'.
#'   Maximum permissible number in the integer chromosome,
#'   used to calculate the fixed length of a binary string.
#' @param y 'numeric'.
#'   Binary representation of chromosome, a vector of \code{0}s and \code{1}s.
#'
#' @return
#'   \code{EncodeChromosome} returns a 'numeric' vector of \code{0}s and \code{1}s.
#'
#'   \code{DecodeChromosome} returns a 'numeric' vector of integers.
#'
#' @export
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{FindOptimalSubset}}
#'
#' @keywords internal
#'
#' @examples
#' string <- EncodeChromosome(c(41, 796, 382), 1000)
#' print(string)
#' print(DecodeChromosome(string, 1000))
#'

EncodeChromosome <- function(x, n) {
  len <- ceiling(log2(n + 1))
  FUN <- function(i) GA::binary2gray(GA::decimal2binary(i, len))
  return(unlist(lapply(x, FUN)))
}

#' @rdname EncodeChromosome
#' @export

DecodeChromosome <- function(y, n) {
  len <- ceiling(log2(n + 1))
  FUN <- function(i) GA::binary2decimal(GA::gray2binary(y[i:(i + len - 1L)]))
  return(vapply(seq(1, length(y), by=len), FUN, 0))
}
