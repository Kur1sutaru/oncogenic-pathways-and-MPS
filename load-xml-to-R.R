
setwd()

#Reading XML File
#The xml file is read by R using the function xmlParse(). It is stored as a list in R.

# Load the package required to read XML files.
library("XML")

# Also load the other required package.
library("methods")

input<-xmlParse("C:/Users/Mateus/Desktop/pride.pep.xml")

# Give the input file name to the function.
result <- xmlParse(file = "pride.pep.xml")

# Print the result.
print(input)
