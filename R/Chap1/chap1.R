# chapter 1

# 00: Obtain the string that arranges letters of the string “stressed” in reverse order (tail to head).

s <- c("stressted", "mit")
sapply(lapply(strsplit(s, ""), rev), function(x) paste0(x, collapse = ""))

# 01: Obtain the string that concatenates the 1st, 3rd, 5th, and 7th letters in the string “schooled”.
s <- "schooled"
sapply(strsplit(s, ""), function(x) paste(x[c(1,3,5,7)], collapse = ""))

s |> strsplit("") |> unlist() |> .[c(TRUE, FALSE)] |> paste(collapse = "")

# 02: Obtain the string “schooled” by concatenating the letters in “shoe” and “cold” one after the other from head to tail.

s <- c("shoe", "cold")
paste0(matrix(unlist(strsplit(s, "")), nrow = 2, byrow = TRUE), collapse = "")

# 03: Split the sentence “Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics”. into words, and create a list whose element presents the number of alphabetical letters in the corresponding word.
s <- "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics"

as.vector(sapply(strsplit(s, " "), nchar))

# 04: Split the sentence “Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can”. into words, and extract the first letter from the 1st, 5th, 6th, 7th, 8th, 9th, 15th, 16th, 19th words and the first two letters from the other words. Create an associative array (dictionary object or mapping object) that maps from the extracted string to the position (offset in the sentence) of the corresponding word.

s <- "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can"
ss <- strsplit(s, " ") |> unlist()
ids <- c(1,5,6,7,8,9,15,16,19)

res <- 1:length(ss)
for (i in 1:length(ss)) {
  names(res)[i]<- substring(ss[i], 1, ifelse(i %in% ids, 1, 2))
}
res

# sample solution
s |> strsplit(" ") |> unlist() -> ss
ss <- 1:length(ss) %>% purrr::map(function(i) {
    setNames(i, substring(ss[i], 1, ifelse(i %in% ids, 1, 2)))
}) %>% unlist()


# 05: Implement a function that obtains n-grams from a given sequence object (e.g., string and list). Use this function to obtain word bi-grams and letter bi-grams from the sentence “I am an NLPer”
s <- "I am an NLPer"

ngram <- function(x) {
  x <- x[x != " "]
  res <- list()
  for (i in 1:(length(x)-1)) {
    res[[i]] <- x[i:(i+1)]
  }
  res
}

# word bi-grams
ngram(unlist(strsplit(s, " ")))

# letter bi-grams
ngram(unlist(strsplit(s, " ")))

# example solution
n_gram <- function(input_seq, n = 2) {
    if (n == 1) return(input_seq)
    embed(input_seq, n)[, n:1] %>% asplit(1)
}

n_gram(unlist(strsplit(s, " ")))
n_gram(unlist(strsplit(s, "")))

# 06: Let the sets of letter bi-grams from the words “paraparaparadise” and “paragraph” $X$ and $Y$, respectively. Obtain the union, intersection, difference of the two sets. In addition, check whether the bigram “se” is included in the sets $X$ and $Y$

X <- "paraparaparadise"
Y <- "paragraph"

# union
Xs <- sapply(ngram(unlist(strsplit(X, ""))), function(x) paste(x, collapse = ""))
Ys <- sapply(ngram(unlist(strsplit(Y, ""))), function(x) paste(x, collapse = ""))

union(Xs, Ys)
intersect(Xs, Ys)
setdiff(Xs, Ys)

# included
grepl("se", X); grepl("se", Y)

# 07: Implement a function that receives arguments, x, y, and z and returns a string “{y} is {z} at {x}”, where “{x}”, “{y}”, and “{z}” denote the values of x, y, and z, respectively. In addition, confirm the return string by giving the arguments x=12, y="temperature", and z=22.4.

fun <- function(x, y, z) {
  print(paste0(y, " is ", z, " at ", x))
}

fun(12, "temperature", 22.4)


# 09: 
s <- "I couldn’t believe that I could actually understand what I was reading : the phenomenal power of the human mind "

random <- function(x) {
  res <- character(length(x))
  for (i in 1:length(x)) {
    if (nchar(x[i]) <= 4) {
      res[i] <- x[i]
    } else {
      xx <- unlist(strsplit(x[i], ""))
      len_xx <- length(xx)
      res[i] <- paste0(c(xx[len_xx], sample(xx[c(2:(len_xx-1))]), xx[1]), collapse = "")
    }
  }
  return(paste(res, collapse = " "))
}

random(unlist(strsplit(s, " ")))

