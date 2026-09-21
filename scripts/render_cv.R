# Renders resume.Rmd to both HTML and PDF with stable, URL-safe filenames.
library(rmarkdown)
library(pagedown)

render("resume.Rmd", output_file = "resume.html")

# pagedown::find_chrome() only consults PAGEDOWN_CHROME on Windows; on Linux it
# just scans Sys.which() for google-chrome/chromium names, which won't match
# the binary that browser-actions/setup-chrome installs in CI. So pass the
# path through explicitly when it's provided.
chrome <- Sys.getenv("PAGEDOWN_CHROME", unset = "")
chrome_args <- "--disable-gpu"

# Headless Chrome can fail to start in CI without --no-sandbox, which prevents
# pagedown from connecting to the remote debugging port.
if (tolower(Sys.getenv("CI", unset = "")) == "true" ||
    tolower(Sys.getenv("GITHUB_ACTIONS", unset = "")) == "true") {
  chrome_args <- c(chrome_args, "--no-sandbox")
}

if (nzchar(chrome)) {
  chrome_print("resume.html", output = "resume.pdf", browser = chrome, timeout = 120, extra_args = chrome_args)
} else {
  chrome_print("resume.html", output = "resume.pdf", timeout = 120, extra_args = chrome_args)
}
