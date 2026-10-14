setwd("~/be623_assignment5")
# Loading the dplyr and ggplot2 packages for data and graphs
library(dplyr)
library(ggplot2)

# Create a data frame 
circ_df <- data.frame(
  sample = paste0("s", 1:12),
  Gene = c(rep("PER2", 6), rep("BMAL1", 6)),
  ZT = rep(c(0, 4, 8, 12, 16, 20), 2),
  Expression = c(1.8, 3.2, 6.5, 9.4, 7.1, 3.6, 
                 8.6, 5.9, 2.7, 1.5, 3.4, 6.8
                 )
)
# Displaying the structure of the data frame.
str(circ_df)

# Selecting the only row of PER2 gene
per2_rows <- circ_df[circ_df$Gene == "PER2", ]
per2_rows

# Calculating the mean, median and standard deviation of PER2.
mean(per2_rows$Expression)
median(per2_rows$Expression)
sd(per2_rows$Expression)

#save the data frame as a cvs file without row numbers
write.csv(circ_df, "circadian_expression.csv", 
          row.names = FALSE)
# Reading the saved cvs file
circ_reloaded <- read.csv("circadian_expression.csv")
# Check whether the original and reloaded expression values are identical
identical(circ_df$Expression, circ_reloaded$Expression)

# select PER2 and sort the expression from highest to lowest(desc. order).
per2_sort <- circ_df %>%
  filter(Gene == "PER2") %>%
  arrange(desc(Expression))
per2_sort
# select BMAL1 and sort the expression from highest to lowest(desc. order).
bmal1_sort <- circ_df %>%
  filter(Gene == "BMAL1") %>%
  arrange(desc(Expression))
bmal1_sort
# Finding the time point of highest PER1 and BMAL1 
per2_peak <- per2_sort$ZT[1]
bmal1_peak <- bmal1_sort$ZT[1]
per2_peak
bmal1_peak
# Calculate the difference between the peak time
difference_in_peak <- abs(per2_peak - bmal1_peak)
difference_in_peak

#Adding a dark colunm where ZT value > 12 are labelled TRUE  
# and value <12 are labelled FALSE.
circ_df2 <- circ_df %>%
  mutate(dark = ZT >= 12)
# Pipeline for dark sample
dark_summary <- circ_df2 %>%
  filter(dark == TRUE) %>%
  group_by(Gene) %>%
  summarize(
    mean_of_Expression = mean(Expression),
    sample = n()
  )
dark_summary
# Pipeline for light sample
light_summary <- circ_df2 %>%
  filter(dark == FALSE) %>%
  group_by(Gene) %>%
  summarize(
    mean_of_expression = mean(Expression),
    sample = n()
  )
light_summary

# Creating a scatter plot using the circ_df data frame
rhythm_plot <- ggplot(circ_df, aes(x = ZT, y = Expression, color = Gene)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c("PER2" = "steelblue", "BMMAL1" = "firebrick")) +
  # adding the graph title, axis labels and legend title
  labs( title = "Expression of circadian gene", x = "Zeitgeber time (h)", 
        y = "Expression",) + 
  theme_minimal() # apply simple and clean graph theme
print(rhythm_plot)
# Saving the scatter plot as a png 
ggsave("rhythm_plot.png", plot = rhythm_plot, width = 6, height = 4, dpi = 300)
  
# Creating a box plot of gene expression                      
expression_boxplot <- ggplot(circ_df, aes(x = Gene, y = Expression, fill = Gene)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("PER2" = "steelblue", "BMAL1" = "firebrick")) + 
# Adding the graph title and axis labels   
  labs( title = "Expression distribution by gene", x = "Gene", y = "Expression") +
# Apply a simple theme  
  theme_minimal()
# Display the box plot.
print(expression_boxplot)
#Saving the box plot as png
ggsave("expression_boxplot.png", plot = expression_boxplot, width = 6, height = 4, dpi = 300) 

