# Local notes

These notes are here to remind me of how to do certain management
tasks related to this book. They are specific to my local installation.

The migration guide is available at
<https://pkgs.rstudio.com/bookdown/articles/bookdown-org-migration-guide.html>.

In the old setup, the procedure for publishing was to execute the
command `bookdown::publish_book()` in R.

In the new setup, the procedure is to execute `rsconnect::deploySite()`
 in R.

It may be necessary to establish a connection to the Posit Cloud server
first.  This can be done by executing `rsconnect::connectCloudUser()`.
