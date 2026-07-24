# Renders resume.Rmd to both HTML and PDF with stable, URL-safe filenames.
library(rmarkdown)
library(pagedown)

render("resume.Rmd", output_file = "resume.html")

# pagedown::find_chrome() only consults PAGEDOWN_CHROME on Windows; on Linux it
# just scans Sys.which() for google-chrome/chromium names, which won't match
# the binary that browser-actions/setup-chrome installs in CI. So pass the
# path through explicitly when it's provided.
chrome <- Sys.getenv("PAGEDOWN_CHROME", unset = "")

if (nzchar(chrome)) {
  chrome_print("resume.html", output = "resume.pdf", browser = chrome, timeout = 120)
} else {
  chrome_print("resume.html", output = "resume.pdf", timeout = 120)
}
