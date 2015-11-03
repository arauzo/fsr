# Prototype to implement FS in R
# ------------------------------
# Example:
#  tab <- read.table('sum.table', header=T)
#  source('prototype.R')
#  breadthFirstSearch(tab, 'SUM', c('x1', 'x2', 'r1','r2'), corrFeatureSetEval)
#  *** It selects just x1 and x2 as these are the only relevant features
# 
# Other tests: 
#  tab2 <- read.table('xor.table', header=T)
#  sequentialForwardSelectionSearch(tab, 'SUM', c('x1', 'x2', 'r1','r2'), corrFeatureSetEval)
#  LVF(tab2, 'XOR', c('x1', 'x2', 'r1','r2'), corrFeatureSetEval, 20, 0.65)  
#  LVW(tab, 'SUM', c('x1', 'x2', 'r1','r2'), corrFeatureSetEval, 20, 1)
#  SA(tab, 'SUM', c('x1', 'x2', 'r1','r2'), corrFeatureSetEval, 5, 3)
#  SA(tab2, 'XOR', c('x1', 'x2', 'r1','r2'), corrFeatureSetEval, 5, 3)
#  GA(tab2, 'XOR', c('x1', 'x2', 'r1','r2'), corrFeatureSetEval, 3, 5)
#
# Copyright 2015 Antonio Arauzo-Azofra and collaborators. All rights reserved.
# (It is expected to be free software, but not yet)
#


#
# --- Measures (feature set merit -> they should be maximized)
#

corrFeatureSetEval <- function(data, response, featureSet) {
    # Get the correlation coefficient of featureSet with response in data
    formula <- as.formula(paste(response, "~", paste(featureSet, collapse="+")))
    model <- lm(data=data, formula)
    value <- summary(model)[[8]]
    value
}



#
# --- Meta-heuristics ---
#
GA <- function(data, response, features, featureSetEval, generations, popSize, 
               cxProb=0.7, mutProb=0.1) {
    # Includes elitism. 
    # Precondition: cxProb+mutProb <= 1

    # Genetic Algorithm 
    nFeatures <- length(features)
    nCrossover <- as.integer(popSize * cxProb / 2)
    nMutation <- as.integer(popSize * mutProb)
#    nRest <- popSize - nCrossover*2 - nMutation - 1
#    cat('nCX=', nCrossover, "\n")
#    cat('nMut=', nMutation, "\n")
#    cat('nRest=', nRest, "\n")
    
    bestSet <- NULL
    bestValue <- NULL
    bestMask <- NULL

    # Generate and evaluate initial population
    pop <- c()
    fitnesses <- c()
    for (i in 1:popSize) {
        mask <- sample(c(TRUE,FALSE), nFeatures, replace=TRUE)
        set <- features[mask]
        # Ensure at least one feature is taken
        if (length(set) < 1) {
            mask[sample(nFeatures, 1)] <- TRUE
            set <- features[mask]
        }
        
        value <- featureSetEval(data, response, set)
        pop[[i]] <- mask
        fitnesses[i] <- value
#        cat(set, value)

        # Update best
        if ( is.null(bestValue) || 
             (value > bestValue) || 
             (value == bestValue && length(set) < length(bestSet))
           ) {
            bestValue <- value
            bestSet <- set
            bestMask <- mask
#            cat(" (best)")
        }
#        cat("\n")

    }
    
    for (i in 1:length(pop)) {
        cat(i,":", pop[[i]], fitnesses[[i]], "\n")
    }
    
#Pasar a data.frame, no practico: 
#    pop <- cbind( as.data.frame( do.call(rbind, pop) ), fitnesses) 
# print population
#    colnames(pop) <- c(features, 'fitness')
#    cat("Population:\n")
#    print(pop)

    # Generations
    for (gen in 1:generations) {
        offspring <- c()
        offspringFitnesses <- c()

        # Selection with Crossover, Mutation, Cloning and Elitism
        i <- 1

        # - Crossover
        if (nCrossover >= 1) {
            for (cx in 1:nCrossover) {
                # Selection, Copy & Crossover                
                a <- sample(1:nFeatures, 1)
                b <- sample(1:nFeatures, 1)
                if (a > b) {
                    aux <- a
                    a <- b
                    b <- aux
                }
                dad <- tournamentSelection(pop, fitnesses)
                mom <- tournamentSelection(pop, fitnesses)
                a <- 1
                cat("b:", b, "\n")
                cat("dad:", dad, pop[[dad]], "\n")
                cat("mom:", mom, pop[[mom]], "\n")
                if (a == 1) {
                    if (b == nFeatures) {
                        offspring[[i]] <- pop[[mom]]
                        offspring[[i+1]] <- pop[[dad]]
                    }
                    else {
                        offspring[[i]] <- c(pop[[mom]][1:b], pop[[dad]][(b+1):nFeatures])
                        offspring[[i+1]] <- c(pop[[dad]][1:b], pop[[mom]][(b+1):nFeatures])
                    }
#c(dad[1:(a-1)], mom[a:b], dad[
                    cat("OFF :", offspring[[i]], "\n")
                    cat("OFF1:", offspring[[i+1]], "\n")                    

                    # Fix if empty features and evaluate
                    for (j in i:(i+1)) {
                        set <- features[offspring[[j]]]
                        # Ensure at least one feature is taken
                        if (length(set) < 1) {
                            offspring[[j]][sample(nFeatures, 1)] <- TRUE
                            set <- features[offspring[[j]]]
                        }
                        #XXX Eval here
                    }

                }
                
                
                #print(pop)
                # Crossover
#                cat(i, " F\n")
#                cat(i+1, " M\n")
                i <- i + 2
            }
        }
        
        # - Mutate
        if (nMutation >= 1) {
            for (mut in 1:nMutation) {
#                cat(i, " x\n")
                i <- i + 1
            }
        }
        
        # - Clone
        while (i < popSize) {
#            cat(i, " c\n")
            i <- i + 1
        }

        # - Elitism        
#        offspring[[popSize]] <- bestMask
#        offspringFitnesses[[popSize]] <- bestValue

        # Evaluation
        # Best record
    }

    # Update best
    if ( (value > bestValue) || 
         (value == bestValue && length(set) < length(bestSet))
       ) {
        bestValue <- value
        bestSet <- set
#        cat("(best)")
    }

    #bestSet
    offspring
}

tournamentSelection <- function(pop, fitnesses, size=4, probBest=0.6) {
    # size: tournament size
    # probBest: probability of the best winning the tournament
    # Returns the index of selected individual
    competitors <- sample(1:length(pop), size)
    bestCompetitor <- which.max(fitnesses[competitors])

    if (runif(1) < probBest) {
#        cat("B=", competitors[bestCompetitor], "\n")
        return(competitors[bestCompetitor])
    }
    else {
        randomIndex <- sample(1:(size-1), 1)
        if (randomIndex == bestCompetitor) {
            randomIndex <- randomIndex + 1
        }
#        cat("O=", competitors[randomIndex], "\n")
        return(competitors[randomIndex])
    }
}


SA <- function(data, response, features, featureSetEval, iterations, neighbours, mu=0.3, prob=0.8) {
    # SimmulatedAnnealing
    # Initial temperature adaptative to random solution measure value
    #  (mu is magnitude of change accepted with prob probability)
 
    nFeatures <- length(features)

    # Initial set
    actualMask <- sample(c(TRUE,FALSE), nFeatures, replace=TRUE)
    actualSet <- features[actualMask]
    # Ensure at least one feature is taken
    if (length(actualSet) < 1) {
        actualMask[sample(nFeatures, 1)] <- TRUE
        actualSet <- features[actualMask]
    }
    actualValue <- featureSetEval(data, response, actualSet)

#    cat(actualSet, actualValue, "\n")

    t0 <- t <- actualValue * -mu / log(prob)
    bestSet <- actualSet
    bestValue <- actualValue

    for (i in 1:iterations) {

        for (j in 1:neighbours) {
            # Generate neighbour by mutation of one feature inclusion or not
            mask <- actualMask
            position <- sample(1:nFeatures, 1)
            mask[position] <- ! mask[position]
            set <- features[mask]
            # Ensure at least one feature is taken
            if (length(set) < 1) {
                mask[sample(nFeatures, 1)] <- TRUE
                set <- features[mask]
            }
            
            # Evaluate            
            value <- featureSetEval(data, response, set)
            dif <- value - actualValue
#            cat(set, value)

            # Accept according to Metropolis criterion
            if ( exp(dif / t) > runif(1) ) {
                # Accept
                actualMask <- mask
                actualSet <- set
                actualValue <- value
#                cat("(accepted)")

                # Update best
                if ( (value > bestValue) || 
                     (value == bestValue && length(set) < length(bestSet))
                   ) {
                    bestValue <- value
                    bestSet <- set
#                    cat("(best)")
                }
            }
#            cat("\n")
        }

        # Cauchy annealing
        t <- t0 / i 
    }

    bestSet
}



#
# --- Complete ---
#

breadthFirstSearch <- function(data, response, features, featureSetEval) {
    # This is the exhaustive search in the order most effective for FS (we can make it stop when
    # adding more features it is slow but have reviewed all sets with less features)
    bestSet <- NULL
    bestValue <- NULL

    # Queue the root
    queue <- list()
    queue[[length(queue)+1]] <- list(list(), as.list(features))

    while (length(queue) > 0) {
        # Pop an unvisited node
        node <- queue[[1]]
        trunk <- node[[1]]
        branches <- node[[2]]
        queue[[1]] <- NULL

        for (i in seq(along=branches)) {
            set <- c(trunk, branches[[i]])

            # Evaluate and check if better
            value <- featureSetEval(data, response, set)
#            cat("Set:")
#            cat(paste(set), " ", value, "\n")
            if (is.null(bestValue) || value > bestValue) {
                bestValue <- value
                bestSet <- set
            }

            # Generate branch nodes if remaining features to combine
            n <- length(branches)
            if (i < n) {
                queue[[length(queue)+1]] <- list(set, branches[(i+1):n])
            }
        }
    }
    bestSet
}



#
# --- Greedy ---
#

sequentialForwardSelectionSearch <- function(data, response, features, featureSetEval) {
    # AKA: Hill-climbing, set-cover
    bestSet <- NULL
    bestValue <- NULL

    selected <- list()
    for (i in seq(along=features)) {
        # Find the best feature to be included in this step forward
        bestFeat <- NULL
        bestFeatValue <- NULL
        for (i in seq(along=features)) {
            feat <- features[[i]]
            if (! feat %in% selected) {
                value <- featureSetEval(data, response, c(selected, feat))
#                cat(feat, " - ", value, "\n")
                if (is.null(bestFeatValue) || value > bestFeatValue) {
                    bestFeatValue <- value
                    bestFeat <- feat
                }
            }
        }

        # Add the selected feature and update best set record
        selected[[length(selected)+1]] <- bestFeat
        if (is.null(bestValue) || bestValue < bestFeatValue) {
            bestValue <- bestFeatValue
            bestSet <- selected
        }
#        cat(paste(selected), " ", bestFeatValue, "\n")
    }
    bestSet
}


listWithoutElement <- function(l, element) {
    # Return a copy of list l with element removed
    result <- list()
    for (i in seq(along=l)) {
        if (l[[i]] != element) {
            result[[length(result)+1]] <- l[[i]]
        }
    }
    result
}

sequentialBackwardSelectionSearch <- function(data, response, features, featureSetEval) {
    selected <- as.list(features)
    bestSet <- selected
    bestValue <- featureSetEval(data, response, selected)

    for (i in seq(length(features)-1)) {
        # Find the best feature to be removed in this step backward
        bestFeat <- NULL
        bestFeatValue <- NULL
        for (i in seq(along=selected)) {
            feat <- selected[[i]]
            value <- featureSetEval(data, response, listWithoutElement(selected, feat))
            if (is.null(bestFeatValue) || bestFeatValue < value) {
                bestFeatValue <- value
                bestFeat <- feat
            }
        }

        # Remove the selected feature and update best set record
        selected <- listWithoutElement(selected, bestFeat)
        if (is.null(bestValue) || bestValue < bestFeatValue) {
            bestValue <- bestFeatValue
            bestSet <- selected
        }
    }
    bestSet
}



#
# --- Probabilistic ---
#

LVF <- function(data, response, features, featureSetEval, iterations, measureThreshold) {
    bestSet <- features
    bestValue <- featureSetEval(data, response, features)
    
    for (i in 1:iterations) {
        set <- sample(features, length(bestSet))
        mask <- sample(c(TRUE,FALSE), length(set), replace=TRUE)
        mask[sample(length(set), 1)] <- TRUE # Ensure at least one feature is taken
        set <- set[mask]
        value <- featureSetEval(data, response, set)

#        cat(set, "->", value, "\n")

        if (value > bestValue || (length(set) < length(bestSet) && value >= measureThreshold)) {
            bestValue <- value
            bestSet <- set
#           cat(bestSet, " (best)\n")
        }

    }

    bestSet
}

LVW <- function(data, response, features, featureSetEval, iterations, measureThreshold) {
    bestSet <- NULL
    bestValue <- NULL
    
    for (i in 1:iterations) {
        mask <- sample(c(TRUE,FALSE), length(features), replace=TRUE)
        mask[sample(length(features), 1)] <- TRUE # Ensure at least one feature is taken
        set <- features[mask]
        value <- featureSetEval(data, response, set)
#        cat(set, "->", value, "\n")

        if (is.null(bestValue) ||
            (value > bestValue || (length(set) < length(bestSet) && value >= bestValue))
           ) {
            bestValue <- value
            bestSet <- set
#            cat(bestSet, " (best)\n")
        }
    }

    bestSet
}



# --- Queue ---
#queue = function() {
#### creates a queue object
#    e = new.env()
#    q = list()
#    assign("q", q, envir=e)
#    class(e) = c("queue","environment")
#    e
#}

#enqueue.queue = function(eq, v) {
#    ## add the value to the list
#    q = c(v, get("q", envir=eq))
#    ## stick the list back in the environment
#    assign("q", q, envir=eq)
#}

#dequeue.queue = function(eq) {
#    ## get the queue, check if anything on it:
#    q = get("q", envir=eq)
#    if (length(q)==0) {
#        stop("Attempt to take element from empty queue")
#    }

#    ## take the last value
#    v = q[[length(q)]]

#    ## take the last value off:
#    #q[[length(q)]] <- NULL
#    if (length(q)==1) {
#        assign("q", list(), eq)
#    }
#    else {
#        assign("q", q[1:(length(q)-1)], eq)
#    }
#    v
#}

#empty.queue = function(eq) {
#    q = get("q", envir=eq)
#    length(q)==0
#}

