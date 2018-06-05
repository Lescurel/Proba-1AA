# Imports
library(ggplot2)

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



simulation <- function(lambda = 2,
                       mu = 1.9,
                       t = 100,
                       N_max = 10, 
                       n_server = 1, 
                       l_priority = c(1/3, 1/3, 1/3),
                       plot = FALSE){
  # checking if the sum of the probablity's priorities = 1
  stopifnot(sum(l_priority) == 1)
  
  # allocate the queue 
  queue <- rep(0, length(l_priority))
  # allocate a simple list of servers (just the parmater of the exponential function)
  server_list <- rep(mu, n_server)
  # used to get a label on which end time of service is to which server (cf add treatment)
  index_servers <- rep(1, n_server)
  
  # We can simulate n event of an exponantial law with a Uniform law
  queries <- data.frame(time = sort(runif(lambda*t, 0, t)), type = 'query', priority = sapply(runif(lambda*t), priority, l_priority), server=NA)
  # time of requests treated
  queries <- add_treatment(queries, server_list, t)
  
  # sort the data by time of arrival
  queries <- queries[order(queries$time),]
  
  ###########################
  # Allocation for plotting #
  ###########################
  if(plot){
    queries_lost <- rep(0, length(queries[,1]))
    len_data = length(queries[,1])*(length(l_priority)+1)
    queue_state_over_time <- data.frame(time = rep(NA, len_data) , value = rep(NA, len_data), category = rep(NA, len_data) ) 
  }
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
    # For Plotting     #
    ####################
    if(plot){
      queries_lost[row] <- q_lost
      # updating data.frame at each loop is terribly inneficient (should use data.table and set)
      queue_state_over_time$value[row] <- sum(queue)
      queue_state_over_time$category[row] <- "toutes priorités"
      queue_state_over_time$time[row] <- queries$time[row]
      for(priority in 1:length(l_priority)){
        idx = row + (priority*length(queries[,1]))
        queue_state_over_time$value[idx] <- queue[priority]
        queue_state_over_time$category[idx] <- paste("priorité ", priority, sep = "")
        queue_state_over_time$time[idx] <- queries$time[row]
      }
    }
  }
  
  
  ####################
  # Plotting Section #
  ####################
  if(plot){
    plot(x = queries$time, y=queries_lost, xlab = "temps", ylab="requetes perdues" , col="red", type = "l")
    plot_queue_state <- ggplot(data = queue_state_over_time, aes(x=time, y=value)) + geom_line(aes(colour=category))
    print(plot_queue_state)
    
    ####################
    #      Stats       #
    ####################
    print("STATISTIQUES")
    print("############")
    
    indexes <- which(queue_state_over_time$category == "toutes priorités")
    mean <- mean(queue_state_over_time$value[indexes])
    total_queries = sum(queue_state_over_time$value[indexes])
    print(paste("[ SIMULATION ] Moyenne d'utilisation de la queue : ", mean, sep = ""))
    for(priority in 1:length(l_priority)){
      indexes <- which(queue_state_over_time$category == paste("priorité ", priority, sep = ""))
      mean <- mean(queue_state_over_time$value[indexes])
      print(paste("[ SIMULATION ] Moyenne du nombre de requete de priorité ", priority, " dans la queue : ", mean, sep = ""))
    }
    if(n_server == 1){
      rho <- lambda/mu
      steps <- lambda*t
      simulation_avg_queries = total_queries/steps
      simulation_drop_rate = sum(q_lost)/steps
      theoretical_avg_queries = (((1-(rho^(N_max-1))) / (1-rho)) - N_max*(rho^(N_max))) * (rho / (1-(rho^(N_max+1))))
      theoretical_drop_rate = ((1-rho) / (1-rho^(N_max+1))) * rho^(N_max)
      print("Moyenne du nombre de requêtes dans le système :")
      print(paste("[ SIMULATION ] ", simulation_avg_queries, sep = ""))
      print(paste("[ THEORIQUE  ] ", theoretical_avg_queries, sep = ""))
      print("Moyenne du nombre de requêtes perdues : ")
      print(paste("[ SIMULATION ] ", simulation_drop_rate, sep = ""))
      print(paste("[ THEORIQUE  ] ", theoretical_drop_rate, sep = ""))
    }
  }
  


}

