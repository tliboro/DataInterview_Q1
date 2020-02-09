rm(list=ls())

library(data.table)
library(magrittr)
library(dplyr)

a <- seq(1:10)
b <- c(3000, 1000, 900, 500, 400, 100, 90, 50, 40)
c <- c(a,b) %>% sort(., decreasing = T)

rn <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", 'IX', 'X', 'XL', 'L', 'XC', 'C', 'CD', 'D', 'CM', 'M', 'MMM') %>% rev(.)

combination <- cbind(c, rn) %>% as.data.table %>% 
	set_colnames(., c("Value", "RoNo")) %>%
	.[, Value := as.integer(Value)]

findLetter <- function(val) {
	loc <- which(combination$Value == val)
	combination$RoNo[loc]
}



first.iteration <- 1
finalWord <- 0
outcome <- function(enter) {

	x <- lapply(combination$Value, function(val) {
		ans <- enter - val 
	} )
	
	y <- unlist(x) 
	new.val <- y[y >= 0] %>% min 
	
	if(new.val == 0) {
		ifelse(first.iteration == 1, finalWord <<- findLetter(enter), finalWord <<- c(finalWord, findLetter(enter - new.val) ) )
	}
	else if (new.val > 0 ) {
		diff <- as.integer(enter) - as.integer(new.val)
		ifelse(first.iteration == 1, finalWord <<- findLetter(diff), finalWord <<- c(finalWord, findLetter(enter - new.val)) )
		first.iteration <<- 2
		outcome(new.val)
	 }
	paste0(finalWord, collapse = "")

}
outcome(11)
