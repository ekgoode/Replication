library(ggplot2)
library(dplyr)
cols <- c("White","Black","Male","Female","Less than HS", "HS", "Some College"
          , "Bachelors Degree", "Graduate Degree", "Schizophrenia", "Bipolar", "Depression"
          , "Anxiety", "Stress")

df <- as.data.frame(matrix(sample(0:1, 1000 * length(cols), replace = TRUE), nrow = 1000, ncol = length(cols)))
df$id <- seq(1,1000,by=1)
colnames(df) <- cols

# User would put in something like 
bycols <- c("White", "Black", "Male", "Female", "Less than HS", "HS", "Some College", "Bachelors Degree", "Graduate Degree")
forcols <- c("Schizophrenia", "Bipolar", "Depression", "Anxiety", "Stress")

plot_inolv <- function(df,cx,cy,style="red") {
  
  # Summary: A function for quick heat maps in R.
  # 
  # Inputs:
  #      df <- A dataframe where each row is a unique record and each column is
  #              a binary variable.
  #      cx <- A list of columns that will appear on the x-axis (columns to cut
  #               by)
  #      cy <- A list of columns that will appear on the y-axis (primary categories)
  #
  # Output:
  #      A heatmap where x=cx and y=cy. Each cell will display the fraction of 
  #        cy who have a 1 for cx.
  
  if (style == "red") {
    lcolor = "white"
    hcolor = "red"
    tcolor = "black"
  }
  result <- c()
  
  for (column in cy) {
    
    denominator <- nrow(df[df[column] == 1,])
    for (metric in cx) {
      numerator <- nrow(df[df[column] == 1 & df[metric] == 1,])
      stat <- c(column, metric, numerator/denominator)
      result <- rbind(result, stat)
    }
  }
  
  result <- as.data.frame(result)
  colnames(result) <- c("cy","cx","value")
  result$value <- as.numeric(result$value)
  
  plt <- ggplot(data=result, aes(x=cx,y=cy,fill=value)) +
    geom_tile(color="black") +
    geom_text(aes(label = round(value,2)), color=tcolor, size= 4) +
    scale_fill_gradient(low=lcolor,high=hcolor) +
    labs(x="",y="") +
    coord_fixed() +
    theme(axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.position = "none")
  
  return(plt)
}

test <- plot_inolv(df,forcols,bycols)
test
