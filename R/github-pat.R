#' Set your Github Personal Access Token
#'
#' Populates the `GITHUB_PAT` environment variable using the [git_credential][http_credentials]
#' manager, which `git` itself uses for storing passwords. The credential manager
#' returns stored credentials if available, and securely prompt the user for
#' credentials when needed.
#'
#' Packages that require a `GITHUB_PAT` can call this function to automatically
#' set the `GITHUB_PAT` when needed. Users may call this function in their
#' [.Rprofile][Startup] script to automatically set `GITHUB_PAT` for each R
#' session without hardcoding any tokens on disk in plain-text.
#'
#' @export
#' @param force_new forget existing pat, always ask for new one.
#' @param url a custom server for GitHub Enterprise users.
#' @param validate checks with the github API that this token works. Defaults to
#' `TRUE` only in an interactive R session (not when running e.g. CMD check).
#' @param verbose prints a message showing the credential helper and PAT owner.
#' @return Returns `TRUE` if a valid GITHUB_PAT was set, and FALSE if not.
set_github_pat <- function(force_new = FALSE, url = 'https://api.github.com',
                           validate = interactive(), verbose = validate){
  url_data <- get_pat_credential_url(url)
  pat_url <- url_data$url
  pat_var <- url_data$var
  if(isTRUE(force_new))
    git_credential_forget(pat_url)
  if(isTRUE(verbose))
    message("If prompted for GitHub credentials, enter your PAT in the password field")
  askpass <- Sys.getenv('GIT_ASKPASS')
  if(nchar(askpass)){
    # Hack to override prompt sentence to say "Token" instead of "Password"
    Sys.setenv(GIT_ASKTOKEN = askpass)
    Sys.setenv(GIT_ASKPASS = system.file('ask_token.sh', package = 'credentials', mustWork = TRUE))
    Sys.setenv(GIT_ASKTOKEN_NAME = 'Personal Access Token (PAT)')
    on.exit(Sys.setenv(GIT_ASKPASS = askpass), add = TRUE)
    on.exit(Sys.unsetenv('GIT_ASKTOKEN_NAME'), add = TRUE)
    on.exit(Sys.unsetenv('GIT_ASKTOKEN'), add = TRUE)
  }
  for(i in 1:3){
    # The username doesn't have to be real, Github seems to ignore username for PATs
    cred <- git_credential_ask(pat_url, verbose = verbose)
    if(length(cred$password) && !is.na(cred$password)){
      if(nchar(cred$password) < 40){
        message("Please enter a token in the password field, not your master password! Let's try again :-)")
        message("To generate a new token, visit: https://github.com/settings/tokens")
        credential_reject(cred)
        next
      }
      if(isTRUE(validate)) {
        hx <- curl::handle_setheaders(curl::new_handle(), Authorization = paste("token", cred$password))
        req <- curl::curl_fetch_memory(paste0(url, '/user'), handle = hx)
        if(req$status_code >= 400){
          message("Authentication failed. Token invalid.")
          credential_reject(cred)
          next
        }
        if(verbose == TRUE){
          data <- jsonlite::fromJSON(rawToChar(req$content))
          helper <- tryCatch(credential_helper_get()[1], error = function(e){"??"})
          message(sprintf("Using GITHUB_PAT from %s (credential helper: %s)", data$name, helper))
        }
      }
      return(do.call(Sys.setenv, structure(list(cred$password), names = pat_var)))
    }
  }
  if(verbose == TRUE){
    message("Failed to find a valid GITHUB_PAT after 3 attempts")
  }
  return(FALSE)
}

# Some notes:
# - Format: GITHUB_PAT_({user}_AT_){host}
# - By default, tokens get stored as user 'PersonalAccessToken' in the credential
#   store, to distinguish them from user/password credentials that the user may have
#   stored. You can override this via the URL, but better not.
# - GitHub usernames (incl GHE) can only contain alphanumeric characters and dashes.
# - Hostnames are case insensitive. We normalize hostname tolower, so you can
#   safely parse _AT_ to look for custom usernames.
get_pat_credential_url <- function(url){
  # strip protocol
  url <- sub("^[a-z]+://", "", tolower(url))
  # strip path
  url <- sub("/.*$", "", url)
  url <- sub("api.github.com", "github.com", url, fixed = TRUE)
  pat_url <- if(grepl("@", url, fixed = TRUE)){
    paste0('https://', url)
  } else {
    paste0('https://PersonalAccessToken@', url)
  }
  pat_var <- if(grepl("github.com$", url)){
    'GITHUB_PAT'
  } else {
    paste0('GITHUB_PAT_', gsub("\\.", "_", sub("^.+@", "", url)))
  }
  list(
    url = pat_url,
    var = pat_var
  )
}

message <- function(...){
  base::message(...)
  utils::flush.console()
}
