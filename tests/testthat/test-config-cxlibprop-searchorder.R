#
# Tests for cxlib::cxlib_config()
#
# Search order for cxlib.properties
#
#
#

testthat::test_that( "config.cxlibPropSrchNoneExist", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, recursive = TRUE, force = FALSE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot create test root" )
  
  
  # - generate test locations
  
  srch_paths <- c( "wd" = cxlib::cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "") ), 
                   "app_home" = cxlib::cxlib_standardpath( base::tempfile( pattern = "app-home-", tmpdir = test_root, fileext = "") ),
                   "libpath" = cxlib::cxlib_standardpath( base::tempfile( pattern = "libpath-", tmpdir = test_root, fileext = "") ) )
  
  srch_paths["app_config"] <- file.path( srch_paths["app_home"], "config", fsep = "/" )
  

  # ... create search directories 
  for ( xdir in srch_paths )
    if ( dir.exists( xdir ) || ! dir.create( xdir, recursive = TRUE ) )
      testthat::fail( "Could not stage path in search directory" )
  
  
  # ... create cxlib directory under libPath reference
  if ( dir.exists( file.path( srch_paths["libpath"], "cxlib", fsep = "/" ) ) || ! dir.create( file.path( srch_paths["libpath"], "cxlib", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage libpath directory for cxlib" )
  
  
    
  # - setup working directory
  
  current_wd <- base::getwd()
  
  on.exit( {
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( srch_paths["wd"] )
  
  
  # - setup app home

  current_apphome <- Sys.getenv( "APP_HOME", unset = NA )
  
  on.exit( {
    
    if ( is.na(current_apphome) ) {
      Sys.unsetenv("APP_HOME")
    } else {
      Sys.setenv( "APP_HOME" = current_apphome )
    }

  }, add = TRUE )
  
  Sys.setenv( "APP_HOME" = srch_paths["app_home"] )
  
  
  # - set up .libPaths
  
  current_libpath <- .libPaths()
  
  on.exit({
    .libPaths( current_libpath )
  }, add = TRUE )
  
  .libPaths( append( srch_paths["libpath"], .libPaths() ) )
  
  
  if ( any( file.exists( file.path( .libPaths(), "cxlib", "cxlib.properties", fsep = "/" ) ) ) )
    testthat::skip( "A cxlib.properties configuration file already exists in .libPaths" )
  
  
  # -- test
  
  result <- cxlib::cxlib_config()
  
  
  # -- assertion
  
  testthat::expect_length( result$.attr[["property.files"]], 0 )

  
})




testthat::test_that( "config.cxlibPropSrchLibPath", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, recursive = TRUE, force = FALSE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot create test root" )
  
  
  # - generate test locations
  
  srch_paths <- c( "wd" = cxlib::cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "") ), 
                   "app_home" = cxlib::cxlib_standardpath( base::tempfile( pattern = "app-home-", tmpdir = test_root, fileext = "") ),
                   "libpath" = cxlib::cxlib_standardpath( base::tempfile( pattern = "libpath-", tmpdir = test_root, fileext = "") ) )
  
  srch_paths["app_config"] <- file.path( srch_paths["app_home"], "config", fsep = "/" )
  
  
  # ... create search directories 
  for ( xdir in srch_paths )
    if ( dir.exists( xdir ) || ! dir.create( xdir, recursive = TRUE ) )
      testthat::fail( "Could not stage path in search directory" )
  
  
  # ... create cxlib directory under libPath reference
  if ( dir.exists( file.path( srch_paths["libpath"], "cxlib", fsep = "/" ) ) || ! dir.create( file.path( srch_paths["libpath"], "cxlib", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage libpath directory for cxlib" )
  
  
  
  # - setup working directory
  
  current_wd <- base::getwd()
  
  on.exit( {
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( srch_paths["wd"] )
  
  
  # - setup app home
  
  current_apphome <- Sys.getenv( "APP_HOME", unset = NA )
  
  on.exit( {
    
    if ( is.na(current_apphome) ) {
      Sys.unsetenv("APP_HOME")
    } else {
      Sys.setenv( "APP_HOME" = current_apphome )
    }
    
  }, add = TRUE )
  
  Sys.setenv( "APP_HOME" = srch_paths["app_home"] )
  
  
  # - set up .libPaths
  
  current_libpath <- .libPaths()
  
  on.exit({
    .libPaths( current_libpath )
  }, add = TRUE )
  
  .libPaths( append( srch_paths["libpath"], .libPaths() ) )
  
  
  if ( any( file.exists( file.path( .libPaths(), "cxlib", "cxlib.properties", fsep = "/" ) ) ) )
    testthat::skip( "A cxlib.properties configuration file already exists in .libPaths" )
  
  
  # - property name
  
  test_property_name <- paste( sample( c( base::LETTERS, as.character(0:9) ), 15 ), collapse = "" )
  
  
  # - property values
  
  test_property_values <- replicate( length(srch_paths), 
                                     base::trimws( paste( sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ) ), 
                                     simplify = TRUE )
  
  names(test_property_values) <- names(srch_paths)
  
  
  # - stage value at libPath level 
  
  writeLines( c( " test property file", 
                 paste( test_property_name, test_property_values["libpath"], sep = " = ") ),
              con = file.path( srch_paths["libpath"], "cxlib", "cxlib.properties", fsep = "/" ) )


  # -- test
  
  result <- cxlib::cxlib_config()

  
  # -- expected
  
  expected_name <- paste( "cxlib", test_property_name, sep = "/" )
  expected_shortname <- test_property_name
  
  expected_value <- unname(test_property_values["libpath"])

  
  # -- assertion
  
  testthat::expect_equal( result$option( expected_name, unset = NA ), expected_value )
  testthat::expect_equal( result$option( expected_shortname, unset = NA ), expected_value )

})





testthat::test_that( "config.cxlibPropSrchAppHome", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, recursive = TRUE, force = FALSE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot create test root" )
  
  
  # - generate test locations
  
  srch_paths <- c( "wd" = cxlib::cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "") ), 
                   "app_home" = cxlib::cxlib_standardpath( base::tempfile( pattern = "app-home-", tmpdir = test_root, fileext = "") ),
                   "libpath" = cxlib::cxlib_standardpath( base::tempfile( pattern = "libpath-", tmpdir = test_root, fileext = "") ) )
  
  srch_paths["app_config"] <- file.path( srch_paths["app_home"], "config", fsep = "/" )
  
  
  # ... create search directories 
  for ( xdir in srch_paths )
    if ( dir.exists( xdir ) || ! dir.create( xdir, recursive = TRUE ) )
      testthat::fail( "Could not stage path in search directory" )
  
  
  # ... create cxlib directory under libPath reference
  if ( dir.exists( file.path( srch_paths["libpath"], "cxlib", fsep = "/" ) ) || ! dir.create( file.path( srch_paths["libpath"], "cxlib", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage libpath directory for cxlib" )
  
  
  
  # - setup working directory
  
  current_wd <- base::getwd()
  
  on.exit( {
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( srch_paths["wd"] )
  
  
  # - setup app home
  
  current_apphome <- Sys.getenv( "APP_HOME", unset = NA )
  
  on.exit( {
    
    if ( is.na(current_apphome) ) {
      Sys.unsetenv("APP_HOME")
    } else {
      Sys.setenv( "APP_HOME" = current_apphome )
    }
    
  }, add = TRUE )
  
  Sys.setenv( "APP_HOME" = srch_paths["app_home"] )
  
  
  # - set up .libPaths
  
  current_libpath <- .libPaths()
  
  on.exit({
    .libPaths( current_libpath )
  }, add = TRUE )
  
  .libPaths( append( srch_paths["libpath"], .libPaths() ) )
  
  
  if ( any( file.exists( file.path( .libPaths(), "cxlib", "cxlib.properties", fsep = "/" ) ) ) )
    testthat::skip( "A cxlib.properties configuration file already exists in .libPaths" )
  
  
  # - property name
  
  test_property_name <- paste( sample( c( base::LETTERS, as.character(0:9) ), 15 ), collapse = "" )
  
  
  # - property values
  
  test_property_values <- replicate( length(srch_paths), 
                                     base::trimws( paste( sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ) ), 
                                     simplify = TRUE )
  
  names(test_property_values) <- names(srch_paths)
  

  # - stage value for app home 
  
  writeLines( c( " test property file", 
                 paste( test_property_name, test_property_values["app_home"], sep = " = ") ),
              con = file.path( srch_paths["app_home"], "cxlib.properties", fsep = "/" ) )
  
    
  # - stage value at libPath level 
  
  writeLines( c( " test property file", 
                 paste( test_property_name, test_property_values["libpath"], sep = " = ") ),
              con = file.path( srch_paths["libpath"], "cxlib", "cxlib.properties", fsep = "/"  ) )
  
  
  # -- test
  
  result <- cxlib::cxlib_config()
  
  
  # -- expected
  
  expected_name <- paste( "cxlib", test_property_name, sep = "/" )
  expected_shortname <- test_property_name
  
  expected_value <- unname(test_property_values["app_home"])
  
  
  # -- assertion
  
  testthat::expect_equal( result$option( expected_name, unset = NA ), expected_value )
  testthat::expect_equal( result$option( expected_shortname, unset = NA ), expected_value )
  
})






testthat::test_that( "config.cxlibPropSrchAppHomeConfig", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, recursive = TRUE, force = FALSE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot create test root" )
  
  
  # - generate test locations
  
  srch_paths <- c( "wd" = cxlib::cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "") ), 
                   "app_home" = cxlib::cxlib_standardpath( base::tempfile( pattern = "app-home-", tmpdir = test_root, fileext = "") ),
                   "libpath" = cxlib::cxlib_standardpath( base::tempfile( pattern = "libpath-", tmpdir = test_root, fileext = "") ) )
  
  srch_paths["app_config"] <- file.path( srch_paths["app_home"], "config", fsep = "/" )
  
  
  # ... create search directories 
  for ( xdir in srch_paths )
    if ( dir.exists( xdir ) || ! dir.create( xdir, recursive = TRUE ) )
      testthat::fail( "Could not stage path in search directory" )
  
  
  # ... create cxlib directory under libPath reference
  if ( dir.exists( file.path( srch_paths["libpath"], "cxlib", fsep = "/" ) ) || ! dir.create( file.path( srch_paths["libpath"], "cxlib", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage libpath directory for cxlib" )
  
  
  
  # - setup working directory
  
  current_wd <- base::getwd()
  
  on.exit( {
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( srch_paths["wd"] )
  
  
  # - setup app home
  
  current_apphome <- Sys.getenv( "APP_HOME", unset = NA )
  
  on.exit( {
    
    if ( is.na(current_apphome) ) {
      Sys.unsetenv("APP_HOME")
    } else {
      Sys.setenv( "APP_HOME" = current_apphome )
    }
    
  }, add = TRUE )
  
  Sys.setenv( "APP_HOME" = srch_paths["app_home"] )
  
  
  # - set up .libPaths
  
  current_libpath <- .libPaths()
  
  on.exit({
    .libPaths( current_libpath )
  }, add = TRUE )
  
  .libPaths( append( srch_paths["libpath"], .libPaths() ) )
  
  
  if ( any( file.exists( file.path( .libPaths(), "cxlib", "cxlib.properties", fsep = "/" ) ) ) )
    testthat::skip( "A cxlib.properties configuration file already exists in .libPaths" )
  
  
  # - property name
  
  test_property_name <- paste( sample( c( base::LETTERS, as.character(0:9) ), 15 ), collapse = "" )
  
  
  # - property values
  
  test_property_values <- replicate( length(srch_paths), 
                                     base::trimws( paste( sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ) ), 
                                     simplify = TRUE )
  
  names(test_property_values) <- names(srch_paths)

  
  # - stage value for app home config
  
  writeLines( c( " test property file", 
                 paste( test_property_name, test_property_values["app_config"], sep = " = ") ),
              con = file.path( srch_paths["app_config"], "cxlib.properties", fsep = "/" ) )
  
  
  
  # - stage value for app home 
  
  writeLines( c( " test property file", 
                 paste( test_property_name, test_property_values["app_home"], sep = " = ") ),
              con = file.path( srch_paths["app_home"], "cxlib.properties", fsep = "/" ) )
  
  
  # - stage value at libPath level 
  
  writeLines( c( " test property file", 
                 paste( test_property_name, test_property_values["libpath"], sep = " = ") ),
              con = file.path( srch_paths["libpath"], "cxlib", "cxlib.properties", fsep = "/"  ) )
  
  
  # -- test
  
  result <- cxlib::cxlib_config()
  
  
  # -- expected
  
  expected_name <- paste( "cxlib", test_property_name, sep = "/" )
  expected_shortname <- test_property_name
  
  expected_value <- unname(test_property_values["app_config"])
  
  
  # -- assertion
  
  testthat::expect_equal( result$option( expected_name, unset = NA ), expected_value )
  testthat::expect_equal( result$option( expected_shortname, unset = NA ), expected_value )
  
})




testthat::test_that( "config.cxlibPropSrchWorkingDirectory", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, recursive = TRUE, force = FALSE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot create test root" )
  
  
  # - generate test locations
  
  srch_paths <- c( "wd" = cxlib::cxlib_standardpath( base::tempfile( pattern = "working-directory-", tmpdir = test_root, fileext = "") ), 
                   "app_home" = cxlib::cxlib_standardpath( base::tempfile( pattern = "app-home-", tmpdir = test_root, fileext = "") ),
                   "libpath" = cxlib::cxlib_standardpath( base::tempfile( pattern = "libpath-", tmpdir = test_root, fileext = "") ) )
  
  srch_paths["app_config"] <- file.path( srch_paths["app_home"], "config", fsep = "/" )
  
  
  # ... create search directories 
  for ( xdir in srch_paths )
    if ( dir.exists( xdir ) || ! dir.create( xdir, recursive = TRUE ) )
      testthat::fail( "Could not stage path in search directory" )
  
  
  # ... create cxlib directory under libPath reference
  if ( dir.exists( file.path( srch_paths["libpath"], "cxlib", fsep = "/" ) ) || ! dir.create( file.path( srch_paths["libpath"], "cxlib", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage libpath directory for cxlib" )
  
  
  
  # - setup working directory
  
  current_wd <- base::getwd()
  
  on.exit( {
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( srch_paths["wd"] )
  
  
  # - setup app home
  
  current_apphome <- Sys.getenv( "APP_HOME", unset = NA )
  
  on.exit( {
    
    if ( is.na(current_apphome) ) {
      Sys.unsetenv("APP_HOME")
    } else {
      Sys.setenv( "APP_HOME" = current_apphome )
    }
    
  }, add = TRUE )
  
  Sys.setenv( "APP_HOME" = srch_paths["app_home"] )
  
  
  # - set up .libPaths
  
  current_libpath <- .libPaths()
  
  on.exit({
    .libPaths( current_libpath )
  }, add = TRUE )
  
  .libPaths( append( srch_paths["libpath"], .libPaths() ) )
  
  
  if ( any( file.exists( file.path( .libPaths(), "cxlib", "cxlib.properties", fsep = "/" ) ) ) )
    testthat::skip( "A cxlib.properties configuration file already exists in .libPaths" )
  
  
  # - property name
  
  test_property_name <- paste( sample( c( base::LETTERS, as.character(0:9) ), 15 ), collapse = "" )
  
  
  # - property values
  
  test_property_values <- replicate( length(srch_paths), 
                                     base::trimws( paste( sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ) ), 
                                     simplify = TRUE )
  
  names(test_property_values) <- names(srch_paths)

    
  # - stage value for working directory
  
  writeLines( c( " test property file", 
                 paste( test_property_name, test_property_values["wd"], sep = " = ") ),
              con = file.path( srch_paths["wd"], "cxlib.properties", fsep = "/" ) )

  
  # - stage value for app home config
  
  writeLines( c( " test property file", 
                 paste( test_property_name, test_property_values["app_config"], sep = " = ") ),
              con = file.path( srch_paths["app_config"], "cxlib.properties", fsep = "/" ) )

  
  # - stage value for app home 
  
  writeLines( c( " test property file", 
                 paste( test_property_name, test_property_values["app_home"], sep = " = ") ),
              con = file.path( srch_paths["app_home"], "cxlib.properties", fsep = "/" ) )
  
  
  # - stage value at libPath level 
  
  writeLines( c( " test property file", 
                 paste( test_property_name, test_property_values["libpath"], sep = " = ") ),
              con = file.path( srch_paths["libpath"], "cxlib", "cxlib.properties", fsep = "/"  ) )
  
  
  # -- test
  
  result <- cxlib::cxlib_config()
  
  
  # -- expected
  
  expected_name <- paste( "cxlib", test_property_name, sep = "/" )
  expected_shortname <- test_property_name
  
  expected_value <- unname(test_property_values["wd"])
  
  
  # -- assertion
  
  testthat::expect_equal( result$option( expected_name, unset = NA ), expected_value )
  testthat::expect_equal( result$option( expected_shortname, unset = NA ), expected_value )
  
})

