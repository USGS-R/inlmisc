#' Find Optimal Subset Using a GA
#'
#' Find optimal subset of a fixed size \code{k} from a finite sequence of length \code{n}.
#' A distributed multiple-population genetic algorithm (GA) is used to do
#' subset selection based on the maximization of a user-supplied fitness function.
#'
#' @param n 'integer' count.
#'   Maximum permissible index, that is, the length of the finite sequence (\code{1:n}).
#'   The GA chooses a subset from this sequence.
#' @param k 'integer' count.
#'   Number of indices to choose, that is, the fixed size of the subset.
#' @param Fitness 'function'.
#'   Fitness function, also known as the objective function, is any allowable \R function which
#'   takes as its first argument the binary \code{string} representing a potential solution.
#'   And as its second argument the maximum permissible index, \code{n}.
#'   Use the \code{\link{DecodeChromosome}(string, n)} command to decode the binary \code{string}.
#'   The fitness function returns a single numerical value describing its fitness.
#'   Recall that the GA searches for a maximum fitness value.
#' @param ...
#'   Additional arguments to be passed to the fitness function.
#' @param popSize 'integer' count.
#'   Population size
#' @param migrationRate 'numeric' number.
#'   Proportion of individuals that should migrate between islands.
#' @param migrationInterval 'integer' count.
#'   Number of iterations at which exchange of individuals takes place.
#'   This interval between migrations is called an \emph{epoch}.
#' @param pcrossover 'numeric' number.
#'   Probability of crossover between pairs of chromosomes.
#' @param pmutation 'numeric' number.
#'   Probability of mutation in a parent chromosome.
#' @param elitism 'integer' count.
#'   Number of chromosomes to survive into the next generation.
#' @param maxiter 'integer' count.
#'   Maximum number of iterations to run before the GA search is halted.
#' @param run 'integer' count.
#'   Number of consecutive generations without any improvement in the
#'   \dQuote{best} fitness value before the GA is stopped.
#' @param suggestions integer 'matrix'.
#'   Integer chromosomes to be included in the initial population.
#'   See returned \code{solution} component for a suggested value for this arugment.
#' @param parallel 'logical' flag or 'integer' count.
#'   Whether to use parallel computing.
#'   This argument can also be used to specify the number of cores
#'   (and number of islands) to employ; by default,
#'   this is the number of physical CPUs/cores.
#'   The \pkg{parallel} and \pkg{doParallel} packages must be
#'   installed for parallel computing to work.
#' @param monitor 'function'.
#'   Function that takes as input the current state of the \code{\link[=gaisl-class]{gaisl-class}} object,
#'   and is run at each epoch of the islands GA search.
#' @param seed 'integer' count.
#'   Random number generator state for random number generation, used to replicate the results.
#'   The \pkg{doRNG} package must be installed if using parallel computing.
#'
#' @details The fitness function (see \code{Fitness} argument) is solved using
#'   the \code{\link[GA]{gaisl}} function in the \pkg{GA} package (Scrucca, 2013, 2016).
#'   The function implements an islands evolution model (first proposed by Cohoon and others, 1987).
#'   to maximize a fitness function using islands genetic algorithms (ISLGAs)
#'   (Luke, 2013, p. 103-104; Scrucca, 2016, p. 197-200).
#'   Independent GAs are configured to use integer chromosomes
#'   represented with a binary codification, linear-rank selection,
#'   uniform crossover, and uniform mutation.
#'
#' @return Returns a 'list' with components:
#'   \describe{
#'     \item{\code{call}}{original call which can be used for later re-use.}
#'     \item{\code{solution}}{a 'matrix' representation of the best solution found.
#'       Each row represents a unique solution giving the best fitness at the final iteration.
#'       More than one row indicates a non-unique solution.
#'       The number of columns is equal to the subset size \code{k}.}
#'     \item{\code{ga_output}}{output from the ISLGAs,
#'       see \code{\link[=gaisl-class]{gaisl-class}} for format description.}
#'     \item{\code{ga_time}}{time required to run the ISLGAs,
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
#' # Problem: Choose the 4 smallest numbers from a list of 100 values
#' # genearated from a standard uniform distribution.
#' k <- 4
#' n <- 100
#' seed <- 123
#' set.seed(seed); numbers <- sort.int(runif(n))
#' Fitness <- function(string, n, numbers) {
#'   idxs <- DecodeChromosome(string, n)
#'   -1 * sum(numbers[idxs])
#' }
#' \dontrun{
#' out <- FindOptimalSubset(n, k, Fitness, numbers, elitism = 1,
#'                          run = 10, monitor = GA::gaislMonitor,
#'                          seed = seed)
#' plot(out[["ga_output"]])
#' summary(out[["ga_output"]])
#' print(out[["solution"]])
#' print(out[["ga_output"]]@fitnessValue)
#' }
#'

FindOptimalSubset <- function(n, k, Fitness, ..., popSize=100,
                              migrationRate=0.1, migrationInterval=10,
                              pcrossover=0.8, pmutation=0.1, elitism=0,
                              maxiter=1000, run=maxiter, suggestions=NULL,
                              parallel=TRUE, monitor=NULL, seed=NULL) {

  # check arguments
  checkmate::assertInt(n, lower=2)
  checkmate::assertInt(k, lower=1, upper=n - 1)
  checkmate::assertFunction(Fitness)
  checkmate::assertInt(popSize, lower=1)
  checkmate::assertNumber(migrationRate, lower=0, upper=1, finite=TRUE)
  checkmate::assertInt(migrationInterval, lower=1)
  checkmate::assertNumber(pcrossover, lower=0, upper=1, finite=TRUE)
  checkmate::assertNumber(pmutation, lower=0, upper=1, finite=TRUE)
  checkmate::assertInt(elitism, lower=0, upper=popSize)
  checkmate::assertInt(maxiter, lower=1)
  checkmate::assertInt(run, lower=1, upper=maxiter)
  checkmate::assertMatrix(suggestions, min.rows=1, min.cols=1, null.ok=TRUE)
  checkmate::qassert(parallel, c("B1", "X1[0,)"))
  checkmate::assertFunction(monitor, null.ok=TRUE)
  if (is.null(monitor)) monitor <- FALSE
  checkmate::assertInt(seed, null.ok=TRUE)

  # set number of islands
  if (is.logical(parallel))
    numIslands <- if (parallel) parallel::detectCores(logical=FALSE) else 4L
  else
    numIslands <- parallel

  # calculate number of bits in the binary string representing the chromosome
  nBits <- .CountBits(n) * k

  # format suggested chromosomes
  if (!is.null(suggestions)) {
    m <- suggestions
    if (any(m > n)) stop("'suggestions' element value(s) greater than 'k'")
    if (k < ncol(m)) {
      m <- m[, seq_len(k), drop=FALSE]
    } else if (k > ncol(m)) {
      set.seed(seed)
      m <- cbind(m, t(apply(m, 1, function(x) sample(seq_len(n)[-x], k - ncol(m)))))
    }
    suggestions <- t(apply(m, 1, function(x) EncodeChromosome(x, n)))
    stopifnot(nBits == ncol(suggestions))
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
                           monitor=monitor,
                           seed=seed)
  })

  # decode solution
  m <- t(apply(ga_output@solution, 1, function(i) {
    sort.int(DecodeChromosome(i, n))
  }))
  solution <- m[!duplicated(m), , drop=FALSE]

  # bundle output
  list("call"      = match.call(),
       "solution"  = solution,
       "ga_output" = ga_output,
       "ga_time"   = ga_time)
}

.Population <- function(object, n) {
  k <- object@nBits / .CountBits(n)
  BuildChromosomes <- function(x) sample.int(n, k)
  m <- do.call("rbind", lapply(seq_len(object@popSize), BuildChromosomes))
  t(apply(m, 1, function(i) EncodeChromosome(i, n)))
}

.Mutate <- function(object, parent, n) {
  encoded_parent <- object@population[parent, ]
  decoded_parent <- DecodeChromosome(encoded_parent, n)
  idxs <- seq_len(n)[-decoded_parent]
  m <- t(apply(object@population, 1, function(i) sort.int(DecodeChromosome(i, n))))
  j <- sample(seq_along(decoded_parent), size=1)
  i <- 0L
  repeat {
    if ((i <- i + 1) > 100) stop("Runnaway loop during mutation")
    x <- decoded_parent
    x[j] <- sample(idxs, size=1)
    x_sorted <- sort.int(x)
    if (!any(apply(m, 1, function(y) identical(y, x_sorted)))) break
  }
  EncodeChromosome(x, n)
}

.Crossover <- function(object, parents, n) {
  fitness_parents <- object@fitness[parents]
  encoded_parents <- object@population[parents, , drop=FALSE]
  decoded_parents <- t(apply(encoded_parents, 1, function(i) {
    DecodeChromosome(i, n)
  }))
  p1 <- decoded_parents[1, ]
  p2 <- decoded_parents[2, ]
  c1 <- p1
  c2 <- p2
  is <- seq_along(p1) %in% sample.int(length(p1), round(length(p1) / 2))  # uniform
  i1 <- is & !p2 %in% p1
  i2 <- is & !p1 %in% p2
  c1[i1] <- p2[i1]
  c2[i2] <- p1[i2]
  decoded_children <- rbind(c1, c2)
  encoded_children <- t(apply(decoded_children, 1, function(i) {
    EncodeChromosome(i, n)
  }))
  m <- t(apply(object@population, 1, function(i) sort.int(DecodeChromosome(i, n))))
  FindFitness <- function(child) {
    object@fitness[which(apply(m, 1, function(i) identical(i, child)))[1]]
  }
  fitness_children <- c(FindFitness(sort.int(c1)), FindFitness(sort.int(c2)))
  list("children"=encoded_children, "fitness"=fitness_children)
}


#' Encode and Decode an Integer Chromosome
#'
#' Functions for encoding and decoding a chromosome represented by integer values.
#' Where a chromosome is a set of numbers that defines a proposed solution to the
#' problem that a genetic algorithm is trying to solve.
#'
#' @param x 'integer' vector.
#'   Integer representation of chromosome.
#' @param n 'integer' count.
#'   Maximum permissible number in the integer chromosome,
#'   used to calculate the bit width of a binary string.
#' @param y 'integer' vector.
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
#' @seealso \code{\link{FindOptimalSubset}}, \code{\link[GA]{binary2decimal}}
#'
#' @keywords internal
#'
#' @examples
#' x <- c(41, 796, 382)
#' y <- EncodeChromosome(x, 1000)
#' print(y)
#' x <- DecodeChromosome(y, 1000)
#' print(x)
#'

EncodeChromosome <- function(x, n) {
  width <- .CountBits(n)
  unlist(lapply(x, function(i) {
    GA::decimal2binary(i, width)
  }))
}

#' @rdname EncodeChromosome
#' @export

DecodeChromosome <- function(y, n) {
  width <- .CountBits(n)
  vapply(seq(1, length(y), by=width), function(i) {
    GA::binary2decimal(y[i:(i + width - 1)])
  }, 0)
}


# Count number of bits in a number

.CountBits <- function(n) {
  as.integer(floor(log2(n)) + 1)
}
