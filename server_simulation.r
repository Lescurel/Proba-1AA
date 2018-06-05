# priority
priority <- function(x, p_array){
  stopifnot(sum(p_array) == 1)
  acc = 0
  n = 0
  for(p in p_array){
    acc = acc + p
    n = n + 1
    if(x < acc){
      return(n)
    }
  }
}

#create the treatment times
treatment <- function(mu, t, index){
  return(data.frame(time = runif(mu*t, 0, t), type = "service", priority = NA, server = index))
}

# add the new servers treatment time to the queries
add_treatment <- function(queries, server_list, t){
  idx <- 1
  for(mu in server_list){
    queries <- rbind(queries, treatment(mu, t, idx))
    idx = idx + 1
  }
  return(queries)
}

main <- function(){
}

lambda <- 2
mu <- 1.9
t <- 100
n_server <- 2
p1 <- 1/3
p2 <- 1/3
p3 <- 1/3
p <- c(p1, p2, p3)
N_max <- 10
queue <- rep(0, length(p))
server_stratum <- 1
server_list <- rep(mu, n_server)
index_servers <- rep(1, n_server)
queue_state_over_time = rep(rep(0, length(p)), lambda*t)


queries <- data.frame(time = sort(runif(lambda*t, 0, t)), type = 'query', priority = sapply(runif(lambda*t), priority, p), server=NA)
# time of requests treated
queries <- add_treatment(queries, server_list, t)

# sort the data by time of arrival
queries <- queries[order(queries$time),]

# Allocation for plotting
queries_lost = rep(0, length(queries[,1]))

# main loop
server_level <- 1
q_lost <- 0
for(row in 1:nrow(queries)){

  # setting the logic to fill the queue
  if(queries$type[row] == "query"){
    # all the servers aren't processing a query
    if(server_level <= n_server){
      server_level <- server_level + 1
      # checking if there's a higher priority query in the queue
      # If thats the case, we put out a query from the queue to treatment, and we add the query in the queue 
      # As we do a -1/+1 in the queue, we don't need to check if the queue is full
      if(sum(queue[1:(queries$priority[row])-1]) > 0){
        # we grab the first non-zero index
        p = which(queue != 0, arr.ind = T)[1]
        queue[p] = queue[p] - 1
        # and we increment our queue with the freshly baked query
        queue[queries$priority[row]] <- queue[queries$priority[row]] + 1
      }else{
        # the query is immediatly treated
        
      }
    }else{ # we're forced to add the query in the queue (no servers available to treat our query)
      # we check if the queue isn't full 
      if(sum(queue) < N_max){
        queue[queries$priority[row]] <- queue[queries$priority[row]] + 1
      }else{ # we lose the query!
        q_lost = q_lost + 1
      }
    }
  }else if(queries$type[row] == "service"){ # we treat the end of service
    # we check if the server was treating a query
    if(queries$server[row] < server_level){
      # if the queue is not empty, we grab the highest priority query to treat it
      if(sum(queue) > 0){
        p = which(queue != 0, arr.ind = T)[1]
        queue[p] = queue[p] - 1
      }else{ #else we do nothing, but we notify that we're available by decreasing the server_level
        if(server_level > 1){
          server_level <- server_level - 1
        }
      }
    }# we don't treat if the server_level is out of reach
  }else{
    # if the type filed of the data frame contains something we don't know, throw an error
    stop(paste("Unknown type ", queries$type[row], "! Data is probably ill formated.", sep = ""))
  }
  
  ####################
  # Plotting section #
  ####################
  queries_lost[row] <- q_lost
  queue_state_over_time[row] <- sum(queue)
}

plot(queue_state_over_time, type="l")
# plot(queries_lost, type = "l")


