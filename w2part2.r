#Basic Operators
y <- 4 +4
y

X = 4
x

x <- 4
y <- 2
x %% y
x + y
x - y
x * y
x / y
x ^ y

# Logical Operators
x <- 4
y <- 2
x >= y
x <= y
x < y
x > y
x != y
x == y

# OR operator(|) and AND operator(&)
x <- 4
y <- 2
(x > y) & (y < x)
(x > y) | (y < x)

c(F,F,F,T,F,F) |c(F,F,F,F,F,F)
x <- c(F,F,F,T,F,F) || c(F,F,F,F,F,F)
x

T | NULL
T || NULL

any(c(F,F,F,F,F,F))
all(c(F,F,F,F,F))

z <- "this"
y <- "that huu"
paste(z,y)

#Other Operators
1:10
"a" %in% c("x", "p","c")
c("x", "p","c") %in% "p"

matrix(c(2,3,4,5), nrow = 2) %in% "a"

matrix (c(2,2,3,3), nrow = 2) %*% matrix(c(4,2,5,3), nrow = 2) 
matrix (c(2,2,3,3), nrow = 2) %*% c(6,6)

x <- c(1:10)
y <- x %% 2
z <- y == 0
x[z]

u <- 4
v <- 8
u ^ v
u * v
u / v
u - v
u + v


x <- c(2,3,4,5,6)
y <- c("a","c","d","e")
x
y






