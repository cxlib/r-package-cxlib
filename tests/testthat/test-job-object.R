#
#
# Tests for cxlib jobs
#
#


#' @cx.testsfor cxlib::cxlib_job()


testthat::skip( message = "TODO: add missing tests" )

testthat::skip( message = "TODO: add tests for cxlib_job$add( ..., after = 0 )" )
testthat::skip( message = "TODO: add tests for cxlib_job$add( ..., after = <pos> )")
testthat::skip( message = "TODO: add tests for cxlib_job$add( ..., after = <test reference> )" )

testthat::skip( message = "TODO: add tests for cxlib_job$drop( <pos> )")
testthat::skip( message = "TODO: add tests for cxlib_job$drop( <test reference> )")





testthat::test_that( "job.defaultParms", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }

  # - reset APP_HOME setting
  on.exit({

    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )

  

  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  

  
  
  # -- test
  #' @cx.tests A default job definition   
  result <- cxlib::cxlib_job()

  
  # -- assertions
  
  testthat::expect_true( inherits( result, "cxlib_job") )
  testthat::expect_equal( attr( class(result), "package"), "cxlib" )


})




testthat::test_that( "job.initNoDefaultActions", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )

    
  # -- test
  #' @cx.tests A default job definition has no default actions   
  result <- test_obj$actions()
  
  
  # -- assertions
  testthat::expect_length( result, 0)
  testthat::expect_length( result$.attr[["actions"]], 0)
  
  
})





testthat::test_that( "job.addSingleActionMissingType", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # -- test
  #' @cx.tests Add action to job definition without specifying type results in an error
  testthat::expect_error( test_obj$add(), regexp = "^The specified action type is missing or invalid$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})




testthat::test_that( "job.addSingleActionTypeNull", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # -- test
  #' @cx.tests Add action to job definition with type equal to NULL results in an error
  testthat::expect_error( test_obj$add(NULL), regexp = "^The specified action type is missing or invalid$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})



testthat::test_that( "job.addSingleActionTypeNA", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # -- test
  #' @cx.tests Add action to job definition with type equal to NA results in an error
  testthat::expect_error( test_obj$add(NA), regexp = "^The specified action type is missing or invalid$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})



testthat::test_that( "job.addSingleActionTypeEmptyCharVector", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # -- test
  #' @cx.tests Add action to job definition with type equal to a character vector with no elements results in an error
  testthat::expect_error( test_obj$add(character(0)), regexp = "^The specified action type is missing or invalid$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})




testthat::test_that( "job.addSingleActionTypeEmptyString", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # -- test
  #' @cx.tests Add action to job definition with type equal to an empty string results in an error
  testthat::expect_error( test_obj$add("  "), regexp = "^The specified action type is missing or invalid$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})




testthat::test_that( "job.addSingleActionTypeInvalidTypeReference", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  # - invalid type reference
  #   note: underscore not a valid character for an action type
  test_type <- paste( replicate( 5, paste( sample( base::letters, 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "_" )
  

  # -- test
  #' @cx.tests Add action to job definition with type in an invalid reference format results in an error
  testthat::expect_error( test_obj$add(test_type), regexp = "^The action type is in an invalid format$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})




testthat::test_that( "job.addSingleActionTypeNoAttributes", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  # - type reference
  #   note: period a valid character for an action type
  test_type <- paste( replicate( 5, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." )
  
  
  # -- test
  #' @cx.tests Add action to job definition with valid type reference and no action attributes
  result <- test_obj$add(test_type)

  
  # -- expected
  expected_actiontype <- test_type
  
  
  # -- assertions
  
  testthat::expect_length( result, 1)
  testthat::expect_length( test_obj$actions(), 1)
  testthat::expect_length( test_obj$.attr[["actions"]], 1)
  
  
  # - result action type
  testthat::expect_equal( test_obj$actions()[[1]][["type"]], expected_actiontype )
  
  
})





testthat::test_that( "job.addSingleActionTypeAttrAsList", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  # - type reference
  #   note: period a valid character for an action type
  test_type <- paste( replicate( 5, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." )
  
  
  # - test attributes
  test_attrs <- as.list( replicate( sample( 2:15, 1),
                                    paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
  
  base::names(test_attrs) <- replicate( length(test_attrs),
                                        paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
  

  # -- test
  #' @cx.tests Add action to job definition with valid type reference and no action attributes
  result <- test_obj$add(test_type, test_attrs)

  
  # -- expected
  expected_actiontype <- test_type
  
  expected_attrs <- test_attrs
  
  
  # -- assertions
  
  testthat::expect_length( result, 1)
  testthat::expect_length( test_obj$actions(), 1)
  testthat::expect_length( test_obj$.attr[["actions"]], 1)

    
  # - result action type
  testthat::expect_equal( test_obj$actions()[[1]][["type"]], expected_actiontype )
  
  
  # - action attributes
  testthat::expect_equal( base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])), base::sort(base::names(expected_attrs)) )
  testthat::expect_equal( test_obj$.attr[["actions"]][[1]][["attributes"]][ base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])) ],
                          expected_attrs[ base::sort(base::names(expected_attrs)) ] )

})






testthat::test_that( "job.addSingleActionTypeAttrAsList", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  # - type reference
  #   note: period a valid character for an action type
  test_type <- paste( replicate( 5, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." )
  
  
  # - test attributes
  test_attrs <- as.list( replicate( sample( 2:15, 1),
                                    paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
  
  base::names(test_attrs) <- replicate( length(test_attrs),
                                        paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
  
  
  # -- test
  #' @cx.tests Add action to job definition with valid type reference and action attributes specified as list of named entries
  result <- test_obj$add(test_type, test_attrs)
  
  
  # -- expected
  expected_actiontype <- test_type
  
  expected_attrs <- test_attrs
  
  
  # -- assertions
  
  testthat::expect_length( result, 1)
  testthat::expect_length( test_obj$actions(), 1)
  testthat::expect_length( test_obj$.attr[["actions"]], 1)
  
  
  # - result action type
  testthat::expect_equal( test_obj$actions()[[1]][["type"]], expected_actiontype )
  
  
  # - action attributes
  testthat::expect_equal( base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])), base::sort(base::names(expected_attrs)) )
  testthat::expect_equal( test_obj$.attr[["actions"]][[1]][["attributes"]][ base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])) ],
                          expected_attrs[ base::sort(base::names(expected_attrs)) ] )
  
})




testthat::test_that( "job.addSingleActionTypeAttrAsArgs", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  # - type reference
  #   note: period a valid character for an action type
  test_type <- paste( replicate( 5, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." )
  
  
  # - test attributes
  test_attrs <- as.list( replicate( sample( 2:15, 1),
                                    paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
  
  base::names(test_attrs) <- replicate( length(test_attrs),
                                        paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
  
  
  # - test arguments 
  test_args <- list( test_type, 
                     test_attrs )
  

  # -- test
  #' @cx.tests Add action to job definition with valid type reference and action attributes specified as named arguments
  result <- do.call( test_obj$add, test_args )

  
  # -- expected
  expected_actiontype <- test_type
  
  expected_attrs <- test_attrs
  
  
  # -- assertions
  
  testthat::expect_length( result, 1)
  testthat::expect_length( test_obj$actions(), 1)
  testthat::expect_length( test_obj$.attr[["actions"]], 1)
  
  
  # - result action type
  testthat::expect_equal( test_obj$actions()[[1]][["type"]], expected_actiontype )
  
  
  # - action attributes
  testthat::expect_equal( base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])), base::sort(base::names(expected_attrs)) )
  testthat::expect_equal( test_obj$.attr[["actions"]][[1]][["attributes"]][ base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])) ],
                          expected_attrs[ base::sort(base::names(expected_attrs)) ] )
  
})





testthat::test_that( "job.addSingleActionTypeAttrAsArgs", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  # - type reference
  #   note: period a valid character for an action type
  test_type <- paste( replicate( 5, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." )
  
  
  # - test attributes
  test_attrs <- as.list( replicate( sample( 2:15, 1),
                                    paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
  
  base::names(test_attrs) <- replicate( length(test_attrs),
                                        paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
  
  
  # - test arguments 
  test_args <- list( test_type, 
                     test_attrs )
  
  
  # -- test
  #' @cx.tests Add action to job definition with valid type reference and action attributes specified as named arguments
  result <- do.call( test_obj$add, test_args )
  
  
  # -- expected
  expected_actiontype <- test_type
  
  expected_attrs <- test_attrs
  
  
  # -- assertions
  
  testthat::expect_length( result, 1)
  testthat::expect_length( test_obj$actions(), 1)
  testthat::expect_length( test_obj$.attr[["actions"]], 1)
  
  
  # - result action type
  testthat::expect_equal( test_obj$actions()[[1]][["type"]], expected_actiontype )
  
  
  # - action attributes
  testthat::expect_equal( base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])), base::sort(base::names(expected_attrs)) )
  testthat::expect_equal( test_obj$.attr[["actions"]][[1]][["attributes"]][ base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])) ],
                          expected_attrs[ base::sort(base::names(expected_attrs)) ] )
  
})




testthat::test_that( "job.addMultiActionTypeAttrAsArgs", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )

    
  # - type references
  test_types <-  replicate( 5, 
                            paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                            simplify = TRUE )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  

  # - stage actions
  for ( xtype in utils::head( test_types, n = length(test_types) - 1 ) ) {

    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_obj$add, test_args )
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  


  # - test arguments
  test_args <- list( utils::tail( test_types, n = 1),
                     test_actions[[ utils::tail( test_types, n = 1) ]] )



  # -- test
  #' @cx.tests Default append action to job definition that has existing actions using action attributes specified as named arguments
  result <- do.call( test_obj$add, test_args )


  # -- expected

  expected_actiontypes <- test_types

  expected_actions <- test_actions


  # -- assertions

  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))

  for ( act_action in test_obj$.attr[["actions"]] ) {

    # - result action type
    testthat::expect_true( act_action[["type"]] %in% expected_actiontypes )


    # - action attributes
    testthat::expect_equal( base::sort(base::names(act_action[["attributes"]])), base::sort(base::names(expected_actions[[ act_action[["type"]] ]] )) )
    testthat::expect_equal( act_action[["attributes"]][ base::sort(base::names(act_action[["attributes"]])) ],
                            expected_actions[[ act_action[["type"]] ]][ base::sort(base::names(expected_actions[[ act_action[["type"]] ]] )) ] )
    
  }


})






testthat::test_that( "job.addMultiActionTypeAttrAsArgs", {
  
  
  # -- stage 
  
  # - test area
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - move APP_HOME
  #   note: align with cxlib:::cxlib_config()
  
  prev_config <- NA
  
  prev_envnames <- base::names(Sys.getenv()) 
  
  if ( "APP_HOME" %in% base::toupper(prev_envnames) ) {
    
    # - catch APP_HONE setting
    prev_config <- Sys.getenv( prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ], unset = NA )
    base::names(prev_config) <- prev_envnames[ match( "APP_HOME", base::toupper(prev_envnames)) ]
    
    # - remove APP_HOME setting
    Sys.unsetenv( base::names(prev_config) )
  }
  
  # - reset APP_HOME setting
  on.exit({
    
    if ( ! is.na(prev_config) )
      Sys.setenv( prev_config )
    
  }, add = TRUE )
  
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxlib", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( "# no cxlib properties",
                                        con = base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxlib", "cxlib.properties" ) ) )
    testthat::fail( "Could not stage empty cxlib properties file" )
  
  
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # - type references
  test_types <-  replicate( 5, 
                            paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                            simplify = TRUE )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  for ( xtype in utils::head( test_types, n = length(test_types) - 1 ) ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_obj$add, test_args )
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  
  

  # -- test
  #' @cx.tests Default append action to job definition that has existing actions using action attributes specified as list of named elements
  result <- test_obj$add( utils::tail( test_types, n = 1), test_actions[[ utils::tail( test_types, n = 1) ]] )
  
  
  # -- expected
  
  expected_actiontypes <- test_types
  
  expected_actions <- test_actions
  
  
  # -- assertions
  
  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))
  
  for ( act_action in test_obj$.attr[["actions"]] ) {
    
    # - result action type
    testthat::expect_true( act_action[["type"]] %in% expected_actiontypes )
    
    
    # - action attributes
    testthat::expect_equal( base::sort(base::names(act_action[["attributes"]])), base::sort(base::names(expected_actions[[ act_action[["type"]] ]] )) )
    testthat::expect_equal( act_action[["attributes"]][ base::sort(base::names(act_action[["attributes"]])) ],
                            expected_actions[[ act_action[["type"]] ]][ base::sort(base::names(expected_actions[[ act_action[["type"]] ]] )) ] )
    
  }
  
  
})
