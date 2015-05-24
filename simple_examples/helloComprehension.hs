-- Remove vohels from "Hello World"
[ x | x <- "Hello World" , not (elem "aeiou" x)]
