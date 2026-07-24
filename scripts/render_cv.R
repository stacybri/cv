# Renders resume.Rmd to both HTML and PDF with stable, URL-safe filenames.
library(rmarkdown)
library(pagedown)

render("resume.Rmd", output_file = "resume.html")
chrome_print("resume.html", output = "resume.pdf")
