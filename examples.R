##
## Play-along Examples for "Enhance Your R Performance and Flexibility with Rcpp"
## Jon Meek - March 2019
##


## Load packages
##
library(Rcpp)
library(microbenchmark)
library(ggplot2)

options(width=100) # Make display wide for benchmark results



## Install missing packages if needed (you may already have Rcpp)
##
packages <- c('devtools', 'microbenchmark')
install.packages(packages)



## Some Base R Examples

########### Slide 3 ###########

## Allocate a 23 million point vector
##
vlength <- 23e6
vec <- vector(mode = 'numeric', length = vlength)
str(vec)

incVal1 <- 1

## Use a loop to increment every element
##
t_start <- proc.time()
for (i in 1:length(vec)) {
    vec[i] <- vec[i] + incVal1
}
proc.time() - t_start
str(vec)

## Did that take a long time (> 5 s)? What version of R are you using?
##
R.version.string

## If version is < 3.4.0 enable the 'Just In Time' byte-code compiler
## then try the above for loop again

library(compiler)
enableJIT(3)



########### Slide 4 ###########

## Do the loop another way
##
t_start <- proc.time()
vec[1:length(vec)] <- vec[1:length(vec)] + incVal1
proc.time() - t_start
str(vec)

## Use vectorized R method to increment every element
##
t_start <- proc.time()
vec <- vec + incVal1
proc.time() - t_start
str(vec)


########### Slide 6 ###########

## Let's try Rcpp - Inline mode

cppFunction('NumericVector incrementVector(double Increment, NumericVector TheData) {
   int n = TheData.size();       // C++ way to get length of the vector
   for (int i = 0; i < n; ++i) {
     TheData[i] += Increment;
   }
   return TheData;
}')


## Use our simple in-line C++ function to increment every element
##
t_start <- proc.time()
vec <- incrementVector(incVal1, vec)
proc.time() - t_start
str(vec)


########### Slide 8 ###########

## Now use a better benchmark method, skip the two slow methods...

## Base R - Fast Method
##
mb_res1 <- microbenchmark(vec <- vec + incVal1)
mb_res1
str(vec)  ## Note that we got another 100 increments


## Rcpp
##
mb_res2 <- microbenchmark(vec <- incrementVector(incVal1, vec))
mb_res2
str(vec)


## Compute the speedup factor for Rcpp vs vectorized Base R
##
median(mb_res1$time) / median(mb_res2$time)

## Compute the speedup percentage for Rcpp vs vectorized Base R
##
100 * (median(mb_res1$time) - median(mb_res2$time)) / median(mb_res1$time)


## Combine the benchmark results
mb_res <- rbind(mb_res1, mb_res2)
mb_res

########### Slide 10 ###########

## Boxplot
str(mb_res)

ggplot(mb_res) +
  geom_jitter(aes(group = expr, x = expr, y = time / 1e6), colour  = "dodgerblue", size = 0.8, shape=19) +
  geom_boxplot(aes(group = expr, x = expr, y = time / 1e6), fill = "lightblue", alpha=0.2, outlier.size=0, notch = FALSE) +
    xlab('') + ylab('Time, ms') +
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))


########### Slide 11 ###########

## Built-in microbenchmark plot
autoplot(mb_res)


## ------------------


########### Slide 17 ###########

##
## Large Data Set Example (if you have the 133 MB events.rds file)
##

## Adjust myPath as needed

myPath <- '~/wpl/talks/rvar-201903'

eventsDataFile <- paste0(myPath, '/events.rds')
codeFile       <- paste0(myPath, '/concurrent_activity.cpp')

events <- readRDS(eventsDataFile)
str(events)

## Compile  C++ code from a file
## rebuild & showOutput are optional and mostly useful when messing with compilers and optimization flags
##

sourceCpp(rebuild = TRUE, showOutput = TRUE, file=codeFile)

## Number of one second wide bins
##
timerange_s <- max(events$StartSecond) - min(events$StartSecond) + 1

## Run the C++ code with timing
##
t_end_prep <- proc.time()
cca <- concurrentEstimatedThroughput(timerange_s, events$StartSecond, events$Duration, events$BytesRecv)
t_end_loop <- proc.time()

t_end_loop - t_end_prep


########### Slide 18 ###########

str(cca) # Result vector

## Convert to data frame with relative time in seconds and throughput in Mbps
##
cca         <- 8 * cca  / 1e6                # 8 bits / byte  - Mbps
cca_df      <- data.frame(Throughput = cca)  # Make the vector a data frame
cca_df$Time <- seq(1:nrow(cca_df)) - 1       # Add relative time column

str(cca_df)

ggplot(cca_df) +
    geom_point(aes(x = Time, y = Throughput), size = 0.6, color = 'blue', shape = 19) +
    xlab('Time, seconds') + ylab('Throughput, Mbps')

## ------------------
## ------------------

## Run it 100 times, if you wish

mb_cca <-  microbenchmark(
    concurrentEstimatedThroughput(timerange_s, events$StartSecond, events$Duration, events$BytesRecv)
)

mb_cca

autoplot(mb_cca)

## ------------------
## ------------------

## The BaseR for loop method - Warning this will likely require 10 minutes to a full hour !

## Number of one second wide bins
##
timerange_s <- max(events$StartSecond) - min(events$StartSecond) + 1

ccuBaseR <- vector(mode = 'numeric', length = timerange_s)

t_start <- proc.time()

## The Loop
for (i in 1:nrow(events)) {
    idx <- events$StartSecond[i] + 1       # Start index; R index starts at 1
    if (events$Duration[i] > 1) {                      # Does event span multiple bins?
        idt <- as.integer(ceiling(events$Duration[i])) # Event duration in bins
        bytes_per_second <- events$BytesRecv[i] / idt
        k <- idx + idt - 1                               # Final index to be incremented
        if ((k) > timerange_s) {                         # Don't go past end of vector
            idt <- timerange_s - idx
            k <- idx + idt
        }
        ## ccuBaseR[idx:k] <- ccuBaseR[idx:k] +  bytes_per_second  # Vectorized bin increments
        for (j in idx:k) {
            ccuBaseR[j] <- ccuBaseR[j] + bytes_per_second          # An inner loop, how bad is it?
        }
    } else {
        ccuBaseR[idx] <- ccuBaseR[idx] + events$BytesRecv[i]       # Single bin to be incremented
    }
}

proc.time() - t_start

## ------------------
## ------------------

## Are the results identical?

d_cca <- ccuBaseR - cca

summary(d_cca)

