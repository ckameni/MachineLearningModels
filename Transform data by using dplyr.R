# Load the packages in the tidyverse into the current R session

#install.packages("tidyverse")
library (tidyverse)
# Build a tibble of student data
df_students <- tibble(
  
  # Student names
  name = c('Dan', 'Joann', 'Pedro', 'Rosie', 'Ethan', 'Vicky',
           'Frederic', 'Jimmie', 'Rhonda', 'Giovanni',
           'Francesca', 'Rajab', 'Naiyana', 'Kian', 'Jenny',
           'Jakeem','Helena','Ismat','Anila','Skye','Daniel',
           'Aisha'),
  
  # Study hours
  study_hours = c(10.0, 11.5, 9.0, 16.0, 9.25, 1.0, 11.5, 9.0,
                  8.5, 14.5, 15.5, 13.75, 9.0, 8.0, 15.5, 8.0,
                  9.0, 6.0, 10.0, 12.0, 12.5, 12.0),
  # Grades
  grade = c(50, 50, 47, 97, 49, 3, 53, 42, 26,
            74, 82, 62, 37, 15, 70, 27, 36, 35,
            48, 52, 63, 64)
)

# Print the tibble
df_students

#Load a data frame from a file
#Let's replace the data frame of student grades with the contents of a CSV file.
# Read a CSV file into a tibble
students <- read_csv(file = "https://raw.githubusercontent.com/MicrosoftDocs/ml-basics/master/data/grades.csv")

# Print the first 10 rows of the data
slice_head(students, n = 10)
slice(students, n = 5:10)

#Explore tibbles by using dplyr
#Now that you have some data, you can begin to solve some of the common data manipulation challenges:
  
#  Filter rows by using dplyr::filter()
filter(students, Name == "Jenny")
filter(students, Name %in% c("Jenny", "Giovanni"))
filter(students, StudyHours > 12, Grade > 80)
filter(students, StudyHours > 12 & Grade > 80)

#About the pipe operator (%>%)
students %>% 
  filter(Name == "Bill")

#Handling missing values
anyNA(students)
is.na(students)
# Another more intuitive way would be to get the sum of missing values for each column, like this:
colSums(is.na(students))

#You can also get the sum of missing values for each row, which could be more useful because filter is primarily used to subset rows.
rowSums(is.na(students))


students %>% 
  filter(rowSums(is.na(students)) > 0)

#Create and modify columns by using dplyr::mutate()
# Replace NA in column StudyHours with the mean study hours
students <- students %>% 
  mutate(StudyHours = replace_na(StudyHours, mean(StudyHours, na.rm = TRUE)))

# Print the data frame
students


#Alternatively, it might be important to ensure that you use only data that you know to be absolutely correct. So let's drop rows that contain missing values by using tidyr::drop_na function.
# Drop NAs from our tibble
students <- students %>% 
  drop_na()

# Print tibble
students

anyNA(students)
####################################################

# Get the mean study hours using the accessor `$`
mean_study <- mean(students$StudyHours)

# Get the mean grade using dplyr::pull
mean_grade <- students %>% 
  pull(Grade) %>% 
  mean()

# Print the mean study hours and mean grade
cat(
  'Average weekly study hours: ', round(mean_study, 2),
  '\nAverage grade: ', round(mean_grade, 2)
)

#With this information, you might want to filter the data frame to find only the students who studied for more than the average number of hours.
# Get students who studied for more than the average number of hours
students %>% 
  filter(StudyHours > mean_study)


#For example, how about finding the average grade for students who spent more than the average amount of study time
# Mean grade of students who studied more than average hours
students %>% 
  filter(StudyHours > mean_study) %>% 
  pull(Grade) %>% 
  mean()

#Let's assume that the passing grade for the course is 60.
#You can use that information to add a new column to the data frame, indicating whether each student passed
# TRUE/FALSE column based on whether student passed or not
students <- students %>% 
  mutate(Pass = Grade >= 60)

# Print data frame
students

#Grouped summaries
# Mean study time and grade for students who passed or failed the course
students %>% 
  group_by(Pass) %>% 
  summarise(mean_study = mean(StudyHours), mean_grade = mean(Grade))

#Let's say that you have many numeric columns and still want to apply the mean function across them.
# Mean study time and grade for students who passed or failed the course
students %>% 
  group_by(Pass) %>% 
  summarise(across(where(is.numeric), mean))
# Grouped count for Pass column
students %>% 
  count(Pass)

#Let's say you want to pick only the Name and StudyHours columns. Here's how you would approach the problem:
# Select the Name and StudyHours  column
students %>% 
  select(Name, StudyHours)
#For example, let's keep all but the StudyHours column:
# Keep all columns except the StudyHours column
select(students, !StudyHours)
# Select numeric columns
students %>% 
  select(where(is.numeric))

#Order rows by using dplyr::arrange()
# Create a data frame with the data sorted by Grade (descending)
students_sorted <- students %>%
  # Sort by descending order
  arrange(desc(Grade))

# Print data frame
students_sorted

