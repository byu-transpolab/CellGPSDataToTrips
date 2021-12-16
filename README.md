# Converting GPS data into trips using R

## Author: Gillian Riches

## Date: September 20, 2021

I am currently doing research for Dr. Greg Macfarlane from the traffic engineering
department at Brigham Young University. The purpose of this project and repository
is to consolidate our research question, the method(s) we are using, and the findings 
into one PDF and website. To do this, R Markdown files are created and edited rather than
R script files. Furthermore, the website will be hosted through GitHub.

This project is set up in a way that each one of those sections (question, method, and 
findings) will have its own R Markdown file. By doing so, each section will then have 
its own page on the website and its own section in the PDF.

This is useful because now our research will be accessible to the public via the 
website, the PDF version can easily be submitted to academic journals, and I will 
now have a good basis for when I begin writing my entire thesis. To reiterate, this
project is a very consolidated and shortened version of my research; my thesis is going
to be much more in depth.

In order to get the targets, you must put the data files you want to analyze in its own zipped folder
in the R Project. Make sure that this folder is named "data" and is in the same folder as this project. 
Then, make your own set of GeoJSON files where each file is for a different day and the file is named
in the format "year-month-day" just like it is in the cleaned GPS data "caps". Store each of these
GeoJSON files in a folder called "manual_clusters" and add that folder to your R project.
Once you have the "data" and "manual_clusters" folders in the R Project, load the targets 
library and then tar_make(). The names of the targets are in the targets.R file. Simply 
run tar_load(name_of_target) to see these targets you just made.
