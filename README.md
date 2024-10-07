# Expected Utility Shiny App
With this app, you can see the difference between the expected value and the expected utility of a gamble.

You are able to adjust the values of events, the probabilities with which they happen, and the curvature of the utility function (i.e., how strongly marginal utility diminishes in monetary value).

# Usage

## Preferred

### Requirements
R & RStudio

In RStudio, run:

```
if (!require("shiny", quietly=T)) {
     install.packages("shiny", dependencies = TRUE, quiet = TRUE)
}
library(shiny)
runGitHub("simonvbaal/shiny_utility", subdir = "utility")

```

## Alternative

Visit: 
https://simonvbaal.shinyapps.io/utility/
