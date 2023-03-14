#https://learn.microsoft.com/en-us/training/modules/explore-analyze-data-with-r/5-exercise-visualize-data-with-ggplot2
# Load your data
# Load the tidyverse and make it available in the current R session
library(tidyverse)

## Now, let's get the data ready for further analysis.
  # Read a CSV file into a tibble
  df_students <- read_csv(file = "https://raw.githubusercontent.com/MicrosoftDocs/ml-basics/master/data/grades.csv")
  
  # Remove any rows with missing data
  df_students <- df_students %>% 
    drop_na()
  
  # Add a column "Pass" that specifies whether a student passed or failed
  # Assuming '60' is the grade needed to pass
  df_students <- df_students %>% 
    mutate(Pass = Grade >= 60)
  
  # Print the results
  df_students

## Visualize data by using ggplot2
  #Now let's see this in action. Start with a simple bar chart that shows each student's grade:
  ggplot(data = df_students) +
    geom_col(mapping = aes(x = Name, y = Grade))
  
  #ggplot(data = df_students) basically creates an empty graph to which you can add layers by using a plus sign (+).
  #geom_col() then adds a layer of bars whose height corresponds to the variables that are specified by the mapping 
  #argument. The mapping argument is always paired with aes(), which specifies how variables in the data are mapped. 
  #What goes into aes() are variables found in the data. In this case, you specified that you want to map Name to 
  #the x-axis and Grade to the y-axis.
  
  #Now, let's improve the visual elements of the plot. For example, the following code:
  
   # Specifies the color of the bar chart.
   # Adds a title to the chart (so you know what it represents)
   # Adds labels to the x-axis and y-axis (so you know which axis shows which data)
  
  # Change the default grey background
  theme_set(theme_light())
  
  
  ggplot(data = df_students) +
    geom_col(mapping = aes(x = Name, y = Grade),
             # Specifiy color and transparency of the bars
             fill = "midnightblue", alpha = 0.7) +
    # Add a title to the chart
    ggtitle("Student Grades") +
    # Add labels to axes
    xlab("Student") +
    ylab("Grade")
  
  # For example, you can:
   # Center the title
   # Add a grid (to make it easier to determine the values for the bars)
   # Rotate the x-axis markers (so you can read them)
  ggplot(data = df_students) +
    geom_col(mapping = aes(x = Name, y = Grade),
             fill = "midnightblue", alpha = 0.7) +
    ggtitle("Student Grades") +
    xlab("Student") +
    ylab("Grade") +
    theme(
      # Center the title
      plot.title = element_text(hjust = 0.5),
      
      # Add a grid (to make it easier to determine the bar values
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "gray", linetype = "dashed", size = 0.5),
      
      # Rotate the x-axis markers so you can read them
      axis.text.x = element_text(angle = 90)
      
    )
  # et's reorder the levels of the Name column in descending order, based on the Grade column, and then plot this.
  df_students %>% 
    mutate(Name = fct_reorder(Name, Grade, .desc = TRUE)) %>% 
    ggplot() +
    geom_col(mapping = aes(x = Name, y = Grade),
             fill = "midnightblue", alpha = 0.7) +
    ggtitle("Student Grades") +
    xlab("Student") +
    ylab("Grade") +
    theme(
      plot.title = element_text(hjust = 0.5),
      
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "gray", linetype = "dashed", size = 0.5),
      
      axis.text.x = element_text(angle = 90)
      
    )
  
  #That's a much better plot, both aesthetically and informationally. For example, you can quickly and easily discern how each student performed.
  
# Get started with statistical analysis
  # Descriptive statistics and data distribution
  #he starting point for this exploration is often to visualize the data as a histogram, and to see how frequently each variable value occurs.
  # Visualize distribution of the grades in a histogram
  ggplot(data = df_students) +
    geom_histogram(mapping = aes(x = Grade))
  
  #All right, this certainly tells you something about your data. For example, most of the grades seem to be about 50
  #  you can experiment with arguments such as binwidth and boundary, as shown here:
  # Visualize distribution of the grades in a histogram
  ggplot(data = df_students) +
    geom_histogram(mapping = aes(x = Grade), , binwidth = 20, boundary = 0.5, fill = "midnightblue", alpha = 0.7) +
    xlab('Grade') +
    ylab('Frequency') +
    theme(plot.title = element_text(hjust = 0.5))
  
  #Much better! The histogram for grades is a symmetric shape, where the most frequently occurring grades tend to be in the middle of the range (about 50), with the least frequently occurring grades displayed at the extreme ends of the scale.
  
  ## Measures of central tendency
   # The mean: A simple average that's calculated by adding all the values in the sample set, and then dividing the total by the number of samples.
   # The median: The value in the middle of the range of all of the sample values.
   # The mode: The most commonly occurring value in the sample set.
  
  # Let's calculate these values, along with the minimum and maximum values for comparison, and show them on the histogram
  # Load statip into the current R sesssion
  library(statip)
  
  # Get summary statistics
  min_val <- min(df_students$Grade)
  max_val <- max(df_students$Grade)
  mean_val <- mean(df_students$Grade)
  med_val <- median(df_students$Grade)
  mod_val <- mfv(df_students$Grade)
  
  # Print the stats
  cat(
    "Minimum: ", round(min_val, 2),
    "\nMean: ", round(mean_val, 2),
    "\nMedian: ", round(med_val, 2),
    "\nMode: ", round(mod_val, 2),
    "\nMaximum: ", round(max_val, 2)
  )
  # Now let's incorporate these statistics in your graph:
  # Plot a histogram
  ggplot(data = df_students) +
    geom_histogram(mapping = aes(x = Grade), binwidth = 20, fill = "midnightblue", alpha = 0.7, boundary = 0.5) +
    
    # Add lines for the statistics
    geom_vline(xintercept = min_val, color = 'gray33', linetype = "dashed", size = 1.3) +
    geom_vline(xintercept = mean_val, color = 'cyan', linetype = "dashed", size = 1.3) +
    geom_vline(xintercept = med_val, color = 'red', linetype = "dashed", size = 1.3 ) +
    geom_vline(xintercept = mod_val, color = 'yellow', linetype = "dashed", size = 1.3 ) +
    geom_vline(xintercept = max_val, color = 'gray33', linetype = "dashed", size = 1.3 ) +
    
    # Add titles and labels
    ggtitle('Data Distribution')+
    xlab('Value')+
    ylab('Frequency')+
    theme(plot.title = element_text(hjust = 0.5))
  
   # geom_vline() adds a vertical reference line to a plot.
  # For the grade data, the mean, median, and mode all seem to be more or less in the middle of the minimum and maximum, at around 50.
  
  # Another way to visualize the distribution of a variable is to use a box plot (sometimes called a box-and-whiskers plot). Let's create one for the grade data
  # Plot a box plot
  ggplot(data = df_students) +
    geom_boxplot(mapping = aes(x = 1, y = Grade), fill = "#E69F00", color = "gray23", alpha = 0.7) +
    
    # Add titles and labels
    ggtitle("Data Distribution") +
    xlab("") +
    ylab("Grade") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # patchwork extends the ggplot API by providing mathematical operators (such as + or /) for combining multiple plots. Yes, it's as easy as that.
  library(patchwork)
  # Create a function that you can reuse
  show_distribution <- function(var_data, binwidth) {
    
    # Get summary statistics by first extracting values from the column
    min_val <- min(pull(var_data))
    max_val <- max(pull(var_data))
    mean_val <- mean(pull(var_data))
    med_val <- median(pull(var_data))
    mod_val <- statip::mfv(pull(var_data))
    
    # Print the stats
    stats <- glue::glue(
      'Minimum: {format(round(min_val, 2), nsmall = 2)}
   Mean: {format(round(mean_val, 2), nsmall = 2)}
   Median: {format(round(med_val, 2), nsmall = 2)}
   Mode: {format(round(mod_val, 2), nsmall = 2)}
   Maximum: {format(round(max_val, 2), nsmall = 2)}'
    )
    
    # Plot the histogram
    hist_gram <- ggplot(var_data) +
      geom_histogram(aes(x = pull(var_data)), binwidth = binwidth,
                     fill = "midnightblue", alpha = 0.7, boundary = 0.4) +
      
      # Add lines for the statistics
      geom_vline(xintercept = min_val, color = 'gray33', linetype = "dashed", size = 1.3) +
      geom_vline(xintercept = mean_val, color = 'cyan', linetype = "dashed", size = 1.3) +
      geom_vline(xintercept = med_val, color = 'red', linetype = "dashed", size = 1.3 ) +
      geom_vline(xintercept = mod_val, color = 'yellow', linetype = "dashed", size = 1.3 ) +
      geom_vline(xintercept = max_val, color = 'gray33', linetype = "dashed", size = 1.3 ) +
      
      # Add titles and labels
      ggtitle('Data Distribution') +
      xlab('')+
      ylab('Frequency') +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Plot the box plot
    bx_plt <- ggplot(data = var_data) +
      geom_boxplot(mapping = aes(x = pull(var_data), y = 1),
                   fill = "#E69F00", color = "gray23", alpha = 0.7) +
      
      # Add titles and labels
      xlab("Value") +
      ylab("") +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # To return multiple outputs, use a list
    return(
      
      list(stats,
           # Combine histogram and box plot using library patchwork
           hist_gram / bx_plt)
      
    ) # End of returned outputs
    
  } # End of function
  
   
  # Now that the show_distribution() function is done, let's get a variable column to examine and then call the function.
  # Get the variable to examine
  col <- df_students %>% 
    select(Grade)
  
  # Call the function
  show_distribution(var_data = col, binwidth = 20)
  
  
  # A density plot is a representation of the distribution of a numeric variable. It is a smoothed version of the histogram and is often used in the same kind of situation.
  
  # geom_density() computes and draws a kernel density estimate, which is a smoothed version of the histogram.
  
  # Create a function that returns a density plot
  show_density <- function(var_data) {
    
    # Get statistics
    mean_val <- mean(pull(var_data))
    med_val <- median(pull(var_data))
    mod_val <- statip::mfv(pull(var_data))
    
    
    # Plot the density plot
    density_plot <- ggplot(data = var_data) +
      geom_density(aes(x = pull(var_data)), fill="orangered", color="white", alpha=0.4) +
      
      # Add lines for the statistics
      geom_vline(xintercept = mean_val, color = 'cyan', linetype = "dashed", size = 1.3) +
      geom_vline(xintercept = med_val, color = 'red', linetype = "dashed", size = 1.3 ) +
      geom_vline(xintercept = mod_val, color = 'yellow', linetype = "dashed", size = 1.3 ) +
      
      # Add titles and labels
      ggtitle('Data Density') +
      xlab('') +
      ylab('Density') +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    
    return(density_plot) # End of returned outputs
    
  } # End of function
  
  
  # Get the density of Grade
  col <- df_students %>% select(Grade)
  show_density(var_data = col)
  