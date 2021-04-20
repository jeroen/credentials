# We load 'askpass' for its side effects, see below!
#' @importFrom askpass ssh_askpass
.onLoad <- function(libname, pkgname){
  # Loading askpass automatically sets SSH_ASKPASS and GIT_ASKPASS variables
  askpass::ssh_askpass()

  # Note: isatty(stdin()) = TRUE in Windows RGui
  if(is_windows() || !interactive() || !isatty(stdin())){
    if(is.na(Sys.getenv('GIT_TERMINAL_PROMPT', NA))){
      Sys.setenv(GIT_TERMINAL_PROMPT=0)
    }
  }

  # Start ssh-agent if available but not running
  if(is_windows() && is.na(Sys.getenv('SSH_AGENT_PID', NA)) && cmd_exists('start-ssh-agent')){
    # Agent not work in Windows: https://github.com/libgit2/libgit2/issues/4958
    #ssh_agent_start()
  }

  # If no credential helper has been set, use the 'cache' helper
  if(!is_check())
    set_default_cred_helper()
}

.onAttach <- function(libname, pkgname){
  tryCatch({
    gitver <- git_with_sys("--version", NULL, FALSE)
    packageStartupMessage(sprintf("Found %s", gitver))
    helpers <- sub("^credential-", "", credential_helper_list())
    packageStartupMessage(sprintf("Supported HTTPS credential helpers: %s",
                                  paste(helpers, collapse = ", ")))
  }, error = function(e){
    if(is_windows()){
      packageStartupMessage("Git for Windows is not installed.\nDownload from: https://git-scm.com/download/win")
    } else {
      packageStartupMessage("Unable to find git :-(")
    }
  })

  tryCatch({
    sshver <- ssh_version()
    packageStartupMessage(sprintf("Found %s", sshver))
    tryCatch({
      key <- find_ssh_key()
      if(length(key)){
        packageStartupMessage(sprintf("Default SSH key: %s", key))
      } else {
        packageStartupMessage("No key found. Use ssh_keygen() to generate one!")
      }
    }, error = function(e){
      packageStartupMessage("Failed to lookup key file")
    })
  }, error = function(e){
    packageStartupMessage(e$message)
  })
  #agent_output <- ssh_agent_start()
  #if(length(agent_output) && nchar(agent_output)){
  #  packageStartupMessage(trimws(agent_output))
  #}
}

set_default_cred_helper <- function(){
  if(has_git_cmd()){
    invisible(tryCatch({
      credential_helper_get()
    }, error = function(...){
      tryCatch({
        helper <- credential_helper_list()[1]
        credential_helper_set(helper, global = TRUE)
      }, error = function(e){
        packageStartupMessage(e$message)
      })
    }))
  }
}

is_check <- function(){
  grepl('credentials.Rcheck', getwd(), fixed = TRUE)
}
