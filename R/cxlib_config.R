#' Utility class to represent cxlib configurations
#' 
#' @field .attr Internal configuration data store
#' 
#' @method initialize initialize
#' @method option option
#' @method show show
#' 
#' @description
#' A utility class to represent cxlib configuration defined in property files and
#' environmental variables.
#' 
#' Property files are named `<context>.properties` where the configuration option
#' is referred to by `context/<property>`. 
#' 
#' Property file syntax and conventions are specified in the help reference for 
#' function \link[cxlib]{cxlib_propertiesread}.
#' 
#' Class initialization first searches for `cxlib.properties` in predefined locations
#' with the first occurrence used. The search sequence is defined as 
#' \enumerate{
#'   \item current working directory
#'   \item `config` sub-directory in the app home directory (`APP_HOME` 
#'   environmental variable, if defined)
#'   \item app home directory (`APP_HOME` environmental variable, if defined)
#'   \item cxlib package install directory in the library tree (`libPaths()`)
#' } 
#' 
#' You can specify a vector of paths as an argument when initiating the 
#' `cxlib_config()` class. If the path specified ends in `.properties`, it is
#' assumed a properties file. Otherwise,  the entry is assumed a directory 
#' containing property files with the extension `.properties`. The vector `x` 
#' is processed in specified order and files within a directory in natural sort 
#' order. 
#' 
#' The `option()` method returns the value of an option if it exists or the value
#' of `unset` if the option does not exist. An option is referred to by the string
#' `<context>/<property>`. If context is omitted, it is assumed `cxlib`.
#' 
#' If the option is not defined as part of a property file, the `option()` method
#' searches for the environment variable `<context>_<property>`, case insensitive. 
#' Any periods in property the name part is converted to underscores.  
#' 
#' An option value that contains the prefix `[env] <name>` or starts with the 
#' character `$<name>` is interpreted as a reference to an environmental 
#' variable with specified name. If the specified environmental variable is not
#' defined, the value of `unset` is returned. The environmental variable name is
#' case sensitive with leading and trailing spaces removed.
#' 
#' The `option()` method switch `as.type = TRUE` returns a \emph{typed} value.
#' If the option name contains `PATH`, case insensitive, a vector of paths
#' separated by `path.sep` defined in \link[base]{.Platform} is returned. 
#' 
#' The logical option value `TRUE` is returned if the option value is equal to
#' one of `ENABLE`, `ENABLED`, `GRANT` and `PERMIT`, case insensitive.
#' 
#' The logical option value `FALSE` is returned if the option value is equal to
#' one of `DISABLE`, `DISABLED`, `REVOKE` and `DENY`, case insensitive.
#' 
#' If none of the above type conversions are satisfied or if `as.type` is equal 
#' to `FALSE`, the \emph{raw} property value is returned. 
#' 
#' (\emph{Experimental}) An option value that contains the prefix `[vault] <name>`
#' is interpreted as a reference to a vault secret with specified name. If a 
#' vault service is not configured or available or the vault is not defined the
#' specified secret, the value of `unset` is returned. The secret name is case 
#' sensitive with leading and trailing spaces removed. See \link[cxlib]{cxlib_vault}
#' for configuration options.
#' 
#' 
#' 
#' @exportClass cxlib_config
#' @export cxlib_config


cxlib_config <- methods::setRefClass( "cxlib_config", 
                                      fields = list( ".attr" = "list" ) )


cxlib_config$methods( "initialize" = function( x ) {
  "Initialize"
  
  
  # -- init .attr
  .self$.attr <- list( ".internal" = list( "property.files" = character(0) ) 
  )
  
  
  # -- initiate list of property files to load
  prop_files <- character(0)

  
  # -- cxlib.properties
    
  # - add current working directory
  srch_paths <- file.path( cxlib::cxlib_standardpath(base::getwd()), "cxlib.properties", fsep = "/" ) 
  
  
  # - look for an APP_HOME environmental variable 
  #   note: assuming directory
  if ( "APP_HOME" %in% base::toupper(names(Sys.getenv())) )  {
    
    # case insensitive matching     
    env_names <- names(Sys.getenv())
    app_home <- cxlib::cxlib_standardpath( Sys.getenv( utils::head( env_names[ base::toupper(env_names) == "APP_HOME" ] , n = 1 ) ) )
    
    srch_paths <- append( srch_paths, 
                          c( file.path( app_home, "config", "cxlib.properties", fsep = "/" ), 
                             file.path( app_home, "cxlib.properties", fsep = "/" ) ) )
  }
  

  # - in .libPaths()
  srch_paths <- append( srch_paths, 
                        base::file.path( cxlib::cxlib_standardpath(.libPaths()), 
                                         "cxlib",
                                         "cxlib.properties", 
                                         fsep = "/" ) )

  # - if cxlib.properties exists
  if ( any( file.exists( srch_paths ) ) )
    prop_files[ "cxlib.properties" ] <- utils::head( srch_paths[ file.exists(srch_paths) ], n = 1 )
      
  
  
  # - specified 
  
  if ( ! missing(x) && ! is.null(x) ) {
    
    if ( ! inherits( x, "character" ) )
      stop( "Expecting a character vector of paths")
    

    for ( xitem in x[ ! is.na(x) ] ) {
      
      # specified as a file 
      if ( ( base::tolower(tools::file_ext( xitem )) == "properties" ) &&
           file.exists( xitem ) ) {
        
        if ( ! base::basename(xitem) %in% names(prop_files) )
          prop_files[ base::basename(xitem) ] <- xitem
        
        next()
      }
      
      # xitem is not obviously a directory
      if ( ! dir.exists( xitem ) )
        next()
      
      
      # treat xitem as a directory
      for ( xfile in list.files( xitem, pattern = ".properties$", recursive = FALSE, include.dirs = FALSE, full.names = TRUE ) )
        if ( ! base::basename(xfile) %in% names(prop_files) )
          prop_files[ base::basename(xfile) ] <- xfile
      
    }  

  }
  
  
  
  # - register property files 
  #   note: first found in the search
  if ( any(file.exists( prop_files )) )
    .self$.attr[[".internal"]][["property.files"]] <- unname( prop_files[ file.exists(prop_files) ] )
  
  
  
  # -- process property files
  
  for ( xpath in .self$.attr[[".internal"]][["property.files"]] ) {
    
    props <- cxlib::cxlib_propertiesread( xpath )
    names(props) <- base::tolower(names(props))
    
    xcontext <- base::basename( tools::file_path_sans_ext( xpath ) )
    
    if ( ! grepl( "^[a-z0-9]+$", xcontext, perl = TRUE, ignore.case = TRUE ) )
      stop( "Invalid property file name" )
    
    
    .self$.attr[[ xcontext ]] <-  props
    
    base::rm( props )
  }
  
  
    

})



cxlib_config$methods( "option" = function( x, unset = NA, as.type = TRUE ) {
  "Get property value"
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits(x, "character") )
    stop( "The specified option is missing" )
  
  if ( length(x) > 1 )
    stop( "More than one option specified. Expecting one.")
  
  if ( ! base::grepl( "^([a-z0-9\\._\\-]+/)?[a-z0-9\\._]+$", x, ignore.case = TRUE, perl = TRUE ) )
    stop( "Option reference is invalid" )
  
  
  # -- generate a standard set of references
  #    note: if context not specified, assume cxlib
  
  opt_std <- base::tolower( ifelse( grepl( "/", x), x, paste("cxlib", x, sep = "/") ) )
  
  opt_ref <- c( "property" = base::gsub( "/", ".", opt_std ),
                "env" = base::gsub( "[\\-\\./]", "_", opt_std ) )
  
  
  # -- initialize value
  xvalue <- NA
  
  
  # -- search properties
  xopts <- base::unlist( .self$.attr[ names(.self$.attr) != ".internal" ] )
  
  if ( base::tolower(opt_ref["property"]) %in% base::tolower(names(xopts)) )
    xvalue <- base::trimws( unname(xopts[ opt_ref["property"] ]) )
  
  
  # -- search environmental variables  
  #    note: search is case in-sensitive
  if ( is.na( xvalue ) ) {
    
    opt_env_names <- names(Sys.getenv())
    
    if ( base::tolower(opt_ref["env"]) %in% base::tolower(opt_env_names) ) 
      xvalue <- base::trimws( Sys.getenv( utils::head( opt_env_names[ base::tolower(opt_env_names) %in% base::tolower(opt_ref["env"]) ], n = 1 ), unset = NA ) )
    
  }
  
  
  if ( is.na(xvalue) )
    return(unset)
  
  
  # -- env variable re-directs
  
  #    note: the value has the prefix "[env]"
  if ( base::startsWith( base::trimws(base::toupper(xvalue)), "[ENV]" ) ) {
    
    # note: start position 6 is length of [env] + 1
    xref_name <- base::trimws( base::substring( base::trimws(xvalue), 6 ) )
    
    xvalue <- base::Sys.getenv( xref_name, unset = unset )
  }
  
  
  #    note: the value starts with the character "$"
  if ( base::startsWith( base::trimws(base::toupper(xvalue)), "$" ) ) {
    
    # note: start position 2 is character after $
    xref_name <- base::trimws( base::substring( base::trimws(xvalue), 2 ) )
    
    xvalue <- base::Sys.getenv( xref_name, unset = unset )
  }
  
  
  
  # -- vault secret re-directs
  
  if ( base::startsWith( base::trimws(base::toupper(xvalue)), "[VAULT]" ) ) {
    
    # note: start position 8 is length of [vault] + 1
    xref_name <- base::trimws( base::substring( base::trimws(xvalue), 8 ) )
    
    # connect to a vault    
    vaultsvc <- cxlib::cxlib_vault()
    
    xvalue <- vaultsvc$secret( xref_name, unset = unset )
  }
  
  
  
  if ( ! as.type )
    return(xvalue)
  
  
  # -- paths
  #    note: if property name includes the term PATH
  #    note: value is treated as valus of delimited list of paths
  
  if ( grepl( "path", gsub( ".*/(.*)", "\\1", opt_std ), ignore.case = TRUE ) )
    return( base::trimws(base::unlist(base::strsplit( xvalue, .Platform$path.sep, fixed = TRUE))) )
  
  
  # -- enabled switch
  #    note: value is a single word
  #    note: if the value is equal to enable, enabled, grant or permit
  #    note: enabled switch is TRUE
  
  if ( grepl( "^(enable|enabled|grant|permit)$", xvalue, ignore.case = TRUE, perl = TRUE ) )
    return( TRUE )
  
  
  # -- disabled switch
  #    note: value is a single word
  #    note: if the value is equal to disable, disabled, revoke or deny
  #    note: disabled switch is TRUE
  
  if ( grepl( "^(disable|disabled|revoke|deny)$", xvalue, ignore.case = TRUE, perl = TRUE ) )
    return( FALSE )
  
  
  # -- or it is simply a value
  
  return(base::trimws(xvalue))
  
})




cxlib_config$methods( "show" = function( x ) {
  "Display list of properties"
  
  xlst <- character(0)
  
  # -- add list of property files to list
  
  xlst <- append( xlst, c( "Property files",
                           paste( base::rep_len( "-", 60), collapse = "") ) )
  
  if ( ! ".internal" %in% names(.self$.attr) || 
       ! "property.files" %in% names(.self$.attr[[".internal"]]) || 
       ( length(.self$.attr[[".internal"]][["property.files"]]) == 0 ) )
    xlst <- append( xlst, "(None)" )
  
  
  if ( ".internal" %in% names(.self$.attr) && 
       "property.files" %in% names(.self$.attr[[".internal"]]) &&
       ( length(.self$.attr[[".internal"]][["property.files"]]) > 0 ) )
    
    xlst <- append( xlst, .self$.attr[[".internal"]][["property.files"]] )
  
  
  # -- display list
  cat( c( base::rep_len(" ", 2),
          xlst, 
          base::rep_len(" ", 2) ), 
       sep = "\n" )
  
})