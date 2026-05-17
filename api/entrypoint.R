# api/entrypoint.R
#
# Posit Connect requires this file to launch a Plumber API.
# It must return the plumber router object — do NOT call $run() here.
# Connect handles starting the server on the correct port.

library(plumber)

plumber::pr("plumber.R")