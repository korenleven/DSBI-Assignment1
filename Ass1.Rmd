---
title: "Assignment 1"
output: html_notebook
---
### Submitters:
Koren Levenbrown 206159428

Matan Bruker 308469915

## EX.1 Part A

```{r}
df = read.csv('hw1_dataset1.csv')
min_p <- min(df$partic)
max_p <- max(df$partic)
paste0('Partic min: ', min_p)
paste0('Partic max: ', max_p)

min_max_norm <- function(v, min, max) {
  new_min = 1
  new_max = 10
  ((v - min) / (max - min)) * (new_max - new_min) + new_min
}

df$partic = sapply(df$partic, min_max_norm, min = min_p, max = max_p)
paste0('Normalized partic in 1952: ', df[df$year == 1952,]$partic)
df
```

## EX.1 Part B

General WMA function (Not used here)

```{r}
wma_f <- function(column, weights) {
  n_weights = length(weights)
  n_rows = length(column)
  col_result <- numeric(n_rows - n_weights)
  for(i in 1:n_weights) {
    col_w = column * weights[i]
    col_result = col_result + col_w[i:(n_rows - n_weights -1 + i)]
  }
  new_col = numeric(n_rows)
  new_col[n_weights + 1 : length(col_result)] <- col_result
  new_col[1: n_weights] <- 0/0
  new_col
}
```


```{r}
original_mconvict <- df$mconvict

w1 = 0.18
w2 = 0.22
w3 = 0.6

mconvict_w1 <- df$mconvict * w1
mconvict_w2 <- df$mconvict * w2
mconvict_w3 <- df$mconvict * w3
n_rows <- length(mconvict_w1)


mconvict_WMA3 <- mconvict_w1[1:(n_rows - 3)] + mconvict_w2[2:(n_rows - 2)] + mconvict_w3[3:(n_rows - 1)]
df$mconvict[4:n_rows] <- mconvict_WMA3
df$mconvict[1:3] <- 0/0

paste0('Smoothed mconvict in 1952: ', df[df$year == 1952,]$mconvict)
paste0('Smoothed mconvict in 1946: ', df[df$year == 1946,]$mconvict)
df$mconvict
```

graph plot:

```{r}
plot(df$year, original_mconvict, type="l", col="blue", ylab="mconvict", xlab="year")
lines(df$year,df$mconvict, col="red")
legend("topright", c("Original","Smoothed"), fill=c("blue","red"))
```

## EX.2

```{r}
entropy_func <- function(column) {
  table = table(column)
  probs = table/ sum(table)
  entropy = 0
  for (p in probs) entropy = entropy - p * log2(p)
  entropy
}

max_entropy_func <- function(column) {
  n_classes <- length(unique(column))
  max_entropy <- -n_classes * (1 / n_classes) * log2(1 / n_classes)
}

df = read.csv('hw1_dataset2.csv')
paste0('gender entropy: ', entropy_func(df$Gender))
paste0('fbs entropy: ', entropy_func(df$fbs))


max_slope_entropy = max_entropy_func(df$slope)
paste0('Max slope entropy: ', max_slope_entropy)
```

```{r}

conditional_entropy_func <- function(df1) {
  t <- table(df1)
  probsYX <- t / margin.table(t)
  probsYgivenX <- prop.table(t,1)
  
  pp <- -log2(probsYgivenX)
  pp[pp == Inf] = 0 # checkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
  pp <- pp * probsYX
  
  hYgivenX <- margin.table(pp)
  hYgivenX
}

sub_df <- df[,c('slope', 'prediction')]
h_pred_given_slope <- conditional_entropy_func(sub_df)

paste0('Conditional entropy predication given slope: ', h_pred_given_slope)
```

```{r}
MI_func <- function(df1) {
  x = df1[,c(1)]
  y = df1[,c(2)]
  
  px = table(x) / margin.table(table(x))
  py = table(y) / margin.table(table(y))

  pxpy = outer(px, py, FUN=function(x,y) x*y)
  probsYX <- table(df1) / margin.table(table(df1))
  tmp <- probsYX * log2(probsYX / pxpy)
  MI <- margin.table(tmp)
  MI
}


h_pred <- entropy_func(df$prediction)

sub_df <- df[,c('Gender', 'prediction')]
h_pred_given_slope <- conditional_entropy_func(sub_df)

IM_pred_slope <- h_pred - h_pred_given_slope
paste0('IM by entropies subtraction: ', IM_pred_slope)
IM_pred_slope <- MI_func(sub_df)
paste0('IM by IM formula: ', IM_pred_slope)
```