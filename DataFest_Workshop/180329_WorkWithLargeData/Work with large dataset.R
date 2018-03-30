## Getting Data into R
file.info(list.files("dir",full.names = TRUE))
setwd("~/Desktop/Data Fest/Workshop")
system.time({flights = read.csv("2008.csv.bz2")})
system.time({flights = read.csv("2008.csv.bz2",comment.char = "",stringsAsFactors = FALSE)})
library(data.table)
system.time({flights = fread("2008.csv")})

class(flights) <- "data.frame"
head(flights)
dim(flights)
save(flights, file = "origin.Rdata")
library(pryr)
flights_sub <- flights %>% sample_frac(0.2)
nrow(flights)
nrow(flights_sub)
object.size(flights)
object_size(flights_sub)
load('origin.RData')

gc()
rm(flights)
gc() ## Memory check


p = progress_estimated(50, min_time = 0)
for(i in 1:50){
  Sys.sleep(0.1)
  p$tick()$print()
}

install.packages("microbenchmark")
library(microbenchmark)
d = abs(rnorm(1000))
r = microbenchmark(exp(log(d)/2),d^0.5,sqrt(d),times = 1000)
print(r)


install.packages("rbenchmark")
library(rbenchmark)
benchmark(
  exp(log(d)/2),
  d^0.5,
  sqrt(d),
  replications = 1000,
  order = "relative"
)

install.packages("profvis")
library(profvis)
set.seed(20180329)
flights_small = flights %>% sample_n(100000)
profvis({
  m = lm(AirTime ~ Distance, data = flights_small)
  plot(AirTime ~ Distance, data = flights_small)
  abline(m, col = "red")
})
addr = data.frame(name = c("Alice","Alice", "Bob","Bob"),
                  email= c("alice@company.com","alice@gmail.com",
                           "bob@company.com","bob@hotmail.com"),
                  stringsAsFactors = FALSE)

phone = data.frame(name = c("Alice","Alice", "Bob","Bob"),
                   phone= c("919 555-1111", "310 555-2222", 
                            "919 555-3333", "310 555-3333"),
                   stringsAsFactors = FALSE)

library(dplyr)
full_join(addr, phone, by="name")

A = data.frame(common="C", A_values=1:1000)
B = data.frame(common="C", B_values=1:1000)

inner_join(A,B) %>% tbl_df()
inner_join(A,B)
