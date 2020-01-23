library(tidyverse)


# day 1 -------------------------------------------------------------------

df <- read.table(pipe("pbpaste"), sep = "\t", header = F)

fuel <- function(c = df) {
  c %>% mutate(V1 = floor(.[[1]] / 3) - 2) %>% summarise(sum(V1))
}
fuel(df)
#3405721

fuelforfuel <- function(x) {
  success <- FALSE
  y <- 0
  while (!success) {
    # do something
    x <- max(floor(x / 3) - 2, 0)
    # check for success
    success <- (x <= 0)
    y <- x + y
  }
  return(y)
}
fuelforfuel(100756)
#50346
sum(sapply(df$V1, fuelforfuel))
#5105716


# day 2 -------------------------------------------------------------------

df <- readClipboard()
df <- as.numeric(unlist(strsplit(df, ',')))
df[2] <- 12
df[3] <- 2
for (i in seq(from = 1, to = length(df), by = 4)) {
  if (df[i] == 1) {
    df[df[i + 3] + 1] <- df[df[i + 1] + 1] + df[df[i + 2] + 1]
  } else if (df[i] == 2) {
    df[df[i + 3] + 1] <- df[df[i + 1] + 1] * df[df[i + 2] + 1]
  } else if (df[i] == 99) {
    break
  }
}

#19690720
dforig <- readClipboard()
dforig <- as.numeric(unlist(strsplit(dforig, ',')))
# df <- dforig

possibilities <- expand.grid(x = seq(1, 100, 1), y = seq(1, 100, 1))
#around 7 minute 40 estimated run time
start_time <- Sys.time()
x <- 1
while (!success) {
  # do something
  df <- dforig
  df[2] <- possibilities[x, ]$x
  df[3] <- possibilities[x, ]$y
  for (i in seq(from = 1,
                to = length(df),
                by = 4)) {
    if (df[i] == 1) {
      df[df[i + 3] + 1] <- df[df[i + 1] + 1] + df[df[i + 2] + 1]
    } else if (df[i] == 2) {
      df[df[i + 3] + 1] <- df[df[i + 1] + 1] * df[df[i + 2] + 1]
    } else if (df[i] == 99) {
      break
    }
  }
  # check for success
  success <- (df[1] == 19690720)
  x <- x + 1
}
end_time <- Sys.time()
end_time - start_time
#successful values
df[2:3]
#8609

# day 3 -------------------------------------------------------------------
library(retistruct)
dforig <- read.table(pipe("pbpaste"), sep = ",", header = F)
a <- as.character(unlist(dforig[1, ], use.names = FALSE))
a <- data.frame(a) %>% separate(a, into = c("direction", "distance"),
                             sep = "(?<=[A-Za-z])(?=[0-9])")
b <- as.character(unlist(dforig[2, ], use.names = FALSE))
b <- data.frame(b) %>% separate(b, into = c("direction", "distance"),
                             sep = "(?<=[A-Za-z])(?=[0-9])")

instructiontovector <- function(a) {
  a$distance <- as.numeric(a$distance)
  df <- data.frame("x" = as.numeric(), "y" = as.numeric())
  
  if (a[1, 1] == "R") {
    df[1, 1] <- 0 + a[1, 2]
    df[1, 2] <- 0
  } else if (a[1, 1] == "L") {
    df[1, 1] <- 0 - a[1, 2]
    df[1, 2] <- 0
  } else if (a[1, 1] == "U") {
    df[1, 2] <- 0 + a[1, 2]
    df[1, 1] <- 0
  } else if (a[1, 1] == "D") {
    df[1, 2] <- 0 - a[1, 2]
    df[1, 1] <- 0
  }
  
  for (i in 2:nrow(a)) {
    if (a[i, 1] == "R") {
      df[i, 1] <- df[i - 1, 1] + a[i, 2]
      df[i, 2] <- df[i - 1, 2]
    } else if (a[i, 1] == "L") {
      df[i, 1] <- df[i - 1, 1] - a[i, 2]
      df[i, 2] <- df[i - 1, 2]
    } else if (a[i, 1] == "U") {
      df[i, 2] <- df[i - 1, 2] + a[i, 2]
      df[i, 1] <- df[i - 1, 1]
    } else if (a[i, 1] == "D") {
      df[i, 2] <- df[i - 1, 2] - a[i, 2]
      df[i, 1] <- df[i - 1, 1]
    }
  }
  return(df)
}

#then check each dont intersect at interior point using line.line
a1 <- instructiontovector(a)
b1 <- instructiontovector(b)

distance <- Inf
for (i in 1:(nrow(a1) - 1)) {
  for (j in 1:(nrow(a1) - 1)) {
    intersection <- line.line.intersection(unlist(a1[j, ])
                                           ,
                                           unlist(a1[j + 1, ])
                                           ,
                                           unlist(b1[i, ])
                                           ,
                                           unlist(b1[i + 1, ]),
                                           interior.only = T)
    
    if (!is.na(intersection[1])) {
      dist <- abs(intersection[1] - 0) + abs(intersection[2] - 0)
      
      if (dist < distance) {
        success <- bind_rows(a1[j, ], a1[j + 1, ], b1[i, ], b1[i + 1, ])
        distance <- pmin(dist, distance)
      }
    }
  }
} 

#part 2, this time optimise for minimise steps (cum sum of row)
distance <- Inf
for (i in 1:(nrow(a1) - 1)) {
  for (j in 1:(nrow(a1) - 1)) {
    intersection <- line.line.intersection(unlist(a1[j,])
                                           ,
                                           unlist(a1[j + 1,])
                                           ,
                                           unlist(b1[i,])
                                           ,
                                           unlist(b1[i + 1,]),
                                           interior.only = T)
    
    if (!is.na(intersection[1])) {
      dist <- sum(a[1:(j), 2]) + sum(b[1:(i), 2]) +
        abs(sum(intersection - a1[j, ])) +
        abs(sum(intersection - b1[i, ]))
      
      if (dist < distance) {
        success <- bind_rows(a1[j,], a1[j + 1,], b1[i,], b1[i + 1,])
        distance <- pmin(dist, distance)
        i2 <- paste(i)
        j2 <-  paste(j)
      }
    }
  }
}

# day 4 -------------------------------------------------------------------
#147981-691423
range <- 147981:691423
success <- as.numeric()

for (i in 1:length(range)) {
  digits <- as.numeric(unlist(str_split(range[i], "")))
  
  if (digits[2] < digits[1]) {
    next
  } else if (digits[3] < digits[2]) {
    next
  } else if (digits[4] < digits[3]) {
    next
  } else if (digits[5] < digits[4]) {
    next
  } else if (digits[6] < digits[5]) {
    next
  } else if (length(unique(digits)) == 6) {
    next
  } else {
    success <- c(success, as.numeric(paste(digits, collapse = "")))
  }
}
}
#1790
#part 2
success <- as.numeric()
#digits <- as.numeric(unlist(str_split(155566,"")))
for (i in 1:length(range)) {
  digits <- as.numeric(unlist(str_split(range[i], "")))
  
  if (digits[2] < digits[1]) {
    next
  } else if (digits[3] < digits[2]) {
    next
  } else if (digits[4] < digits[3]) {
    next
  } else if (digits[5] < digits[4]) {
    next
  } else if (digits[6] < digits[5]) {
    next
  } else if (length(unique(digits)) == 6) {
    next
  } else {
    k = 1
    while (k < 6) {
      state = TRUE
      n = 0
      while (state & k < 6) {
        state = digits[k] == digits[k + 1]
        if (state) {
          n = n + 1
        }
        k = k + 1
      }
    }
    if (n == 1) {
      success <- c(success, as.numeric(paste(digits, collapse = "")))
    }
  }
}
}

# day 5  -------------------------------------------------------------------
df <- readClipboard()
df <- as.numeric(unlist(strsplit(df, ',')))

interpret_instruction <- function(instruction){
  instruction <- as.numeric(unlist(str_split(instruction, "")))
  #add leading zeros
  instruction <- c(rep(0,5-length(instruction)),instruction)  
  return(list(
    op_code = as.numeric(paste0(instruction[4], instruction[5])),
    param_a = instruction[3],
    param_b = instruction[2],
    param_c = instruction[1]
  ))
}

eval_mode <- function(parameter, program, position){
  
  if (parameter == 0) {
    return(program[program[position+1]+1])
  } else if (parameter == 1) {
    return(program[position+1])
  }
}

intcode_computer = function(program = df, input = 1, position = 1){
  output = list()
  n_output <- 1
  while (position < length(program)) {
  instruction <- interpret_instruction(program[position])
  #df[position:(position+5)]
  if (instruction$op_code == 1) {
    
    program[program[position+3]] <- eval_mode(instruction$param_a,program, position) + eval_mode(instruction$param_b,program, position+1)
    position <- position + 5
    
  } else if (instruction$op_code == 2) {
    
    program[program[position+3]] <- eval_mode(instruction$param_a,program, position) * eval_mode(instruction$param_b,program, position+1)
    position <- position + 5
    
  } else if (instruction$op_code == 3) {
    
    program[program[position+1]] <- input
    position <- position + 2
    
  } else if (instruction$op_code == 4) {
    
    output[[n_output]] = instruction$param_a
    print(output)
    position <- position + 2
    n_output <- n_output + 1
    
  } else if (instruction$op_code == 99) {
    print(output)
  } else {
    print(position)
    stop("Error")
  }
  }
  
  return(program)
}
a <- intcode_computer(df)

#part 2

intcode_computer <- function(program = df, input = 5, position = 1){
  output <- list()
  n_output <- 1
  while (position < length(program)) {
    print(position)
    instruction <- interpret_instruction(program[position])
    print(instruction)
    if (instruction$op_code == 1) {
      
      program[program[position+3]+1] <- eval_mode(instruction$param_a,program, position) + eval_mode(instruction$param_b,program, position+1)
      position <- position + 4
      
    } else if (instruction$op_code == 2) {
      
      program[program[position+3]+1] <- eval_mode(instruction$param_a,program, position) * eval_mode(instruction$param_b,program, position+1)
      position <- position + 4
      
    } else if (instruction$op_code == 3) {
      
      program[program[position+1]+1] <- input
      position <- position + 2
      
    } else if (instruction$op_code == 4) {
      
      output[[n_output]] <- eval_mode(instruction$param_a,program, position)
      position <- position + 2
      n_output <- n_output + 1
      
    } else if (instruction$op_code == 5) {
      
      if(eval_mode(instruction$param_a,program, position) != 0){
        
        position <- eval_mode(instruction$param_b,program, position+1) + 1
        
      } else {
        
        position <- position + 3
        
      }
      
    } else if (instruction$op_code == 6) {
      
      if(eval_mode(instruction$param_a,program, position) == 0){
        
        position <- eval_mode(instruction$param_b,program, position+1) + 1
        
      } else {
        
        position <- position + 3
        
      }
      
      
    } else if (instruction$op_code == 7) {
      
      program[program[position + 3] + 1] <- ifelse(eval_mode(instruction$param_a,program, position) < eval_mode(instruction$param_b,program, position+1), 1, 0)
      position <- position + 4
      
    } else if (instruction$op_code == 8) {
      
      program[program[position + 3] + 1] <- ifelse(eval_mode(instruction$param_a,program, position) == eval_mode(instruction$param_b,program, position+1), 1, 0)
      position <- position + 4
      
    } else if (instruction$op_code == 99) {
      print(output)
      break
    } else {
      print(position)
      stop("Error")
    }
  }
  return(program)
}

a <- intcode_computer(df)


# day 6 -------------------------------------------------------------------

df <- readClipboard()
df <- data.frame(matrix(unlist(
  strsplit(df, ')'))
  ,nrow = 1013
  ,byrow = T),stringsAsFactors = F)
colnames(df) <- c("body","orbiter")

output <- list()
for (i in 1:nrow(df)){
  b <- df[i,1]
  find <- TRUE
  n <- 0
  while (find) {
  nextorbit <- df[which(df$orbiter == b),] 
  if (all(is.na(nextorbit))) {
  output[[i]] <- n  
  find = FALSE  
  } else {
  b <- nextorbit[,1] 
  n = n + 1
  }
  }
}
#add all the 'direct' orbits (double counting in my eyes)
sum(unlist(output))+1013

#part 2
#find nearest common body
outputYOU <- list()
b <- df[which(df$orbiter == "YOU"),1] 
find <- TRUE
n <- 1
i = 1
while (find) {
  nextorbit <- df[which(df$orbiter == b),] 
  (all(is.na(nextorbit)))
  if (all(is.na(nextorbit))) {
    outputYOU[[i]] <- c(b,n) 
    find = FALSE  
  } else {
    outputYOU[[i]] <- c(b,n) 
    b <- nextorbit[,1] 
    n = n + 1
    i = i + 1
  }
}

outputSAN <- list()
b <- df[which(df$orbiter == "SAN"),1] 
find <- TRUE
n <- 1
i = 1
while (find) {
  nextorbit <- df[which(df$orbiter == b),] 
  (all(is.na(nextorbit)))
  if (all(is.na(nextorbit))) {
    outputSAN[[i]] <- c(b,n) 
    find = FALSE  
  } else {
    outputSAN[[i]] <- c(b,n) 
    b <- nextorbit[,1] 
    n = n + 1
    i = i + 1
  }
}

outputYOU <- data.frame(matrix(unlist(
  outputYOU)
  ,nrow = length(outputYOU)
  ,byrow = T), stringsAsFactors = F)
colnames(outputYOU) <- c("body", "valueY")
outputYOU$valueY <- as.numeric(outputYOU$valueY)
outputSAN <- data.frame(matrix(unlist(
  outputSAN)
  ,nrow = length(outputSAN)
  ,byrow = T), stringsAsFactors = F)
colnames(outputSAN) <- c("body", "valueS")
outputSAN$valueS <- as.numeric(outputSAN$valueS)

#find common with lowest value
inner_join(outputYOU, outputSAN, by = "body") %>% mutate(value = valueY + valueS)
#take away one for getting to orbit near SAN and take away another as the move towards the commonality is counted from both sides
285-2


# day 8 -------------------------------------------------------------------

df <- readClipboard()
df <- as.numeric(unlist(strsplit(df,split = "")))
#it says 25 tall but thats irrelevant, if we need a layer to be 25*6 thats 150
matrix <- matrix(df,nrow = 100, ncol = 150, byrow = T)
#for each layer of 6 rows, count 0s
count <- rowSums(matrix(df==0,nrow = 100, ncol = 150, byrow = T))
#Looks like layer 16, at 6 zeros

#count number of 1s layer 16 and multiply with 2s
sum(matrix[16,]==1)*
sum(matrix[16,]==2)
