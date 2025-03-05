#
#  tests for cxlib::cxlib_config()
#
#


testthat::test_that( "config.empty", {
 
  
  # -- test
  result <- cxlib::cxlib_config()
  
  
  # -- assertions
  testthat::expect_length( result[[".attr"]][[".internal"]][["property.files"]], 0 )
   
})



testthat::test_that( "config.cxlibConfigEmpty", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( "# empty test properties file", con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  result <- cxlib::cxlib_config()
  
  
  # -- expected
  expected_propfiles <- file.path( test_cxlib_path, "cxlib.properties", fsep = "/")
  
  
  # -- assertions
  testthat::expect_equal( result[[".attr"]][[".internal"]][["property.files"]], expected_propfiles )

})





testthat::test_that( "config.cxlibSimpleConfigSingle", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # test property value
  test_reference_name <- base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) )
  test_reference_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" )
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( test_reference_name, test_reference_value, sep = "=" ) ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  result <- cxlib::cxlib_config()
  
  
  # -- expected
  expected_propfiles <- file.path( test_cxlib_path, "cxlib.properties", fsep = "/")
  
  expected_props <- base::trimws(test_reference_value)
  names(expected_props) <- base::tolower(test_reference_name)
  
  expected_attr <- list( ".internal" = list( "property.files" = expected_propfiles ),
                         "cxlib" = expected_props ) 
  

  # -- assertions
  testthat::expect_equal( result$.attr, expected_attr )

  
    
})



testthat::test_that( "config.cxlibSimpleConfigMultiple", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # test property value
  test_reference_names <- utils::head( base::replicate( 1000, 
                                                        base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) ),
                                                        simplify = TRUE ),
                                       n = 10 )

  if ( length(unique(test_reference_names)) != length(test_reference_names) )
    testthat::fail( "Could not generate 10 unique names" )

  test_reference_values <- base::replicate( 10, 
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ), 
                                            simplify = TRUE )
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file", 
                       paste( sort(test_reference_names), test_reference_values, sep = " = " )),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  result <- cxlib::cxlib_config()
  
  
  # -- expected
  expected_propfiles <- file.path( test_cxlib_path, "cxlib.properties", fsep = "/")
  
  expected_props <- base::trimws(test_reference_values)
  names(expected_props) <- base::tolower(sort(test_reference_names))
  
  expected_attr <- list( ".internal" = list( "property.files" = expected_propfiles ),
                         "cxlib" = expected_props ) 
  
  
  # -- assertions
  testthat::expect_equal( result$.attr, expected_attr )
  
})



testthat::test_that( "config.cxlibSimpleConfigMultipleFirstOccurence", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # two-level config set up
  
  test_levels <- c( "one", "two")
  
  for ( xlevel in test_levels )
    if ( ! dir.exists( file.path( test_root, xlevel, fsep = "/" ) ) && ! dir.create( file.path( test_root, xlevel, fsep = "/" ), recursive = TRUE ) )
      testthat::fail("Could not create test level in test area")
  

  # update .libPaths

  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( file.path( test_root, test_levels, fsep = "/" ), .libPaths() ) )
  

  # test property value
  test_reference_names <- sort(utils::head( base::replicate( 1000, 
                                                             base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) ),
                                                             simplify = TRUE ), 
                                            n = 20 ))
  
  if ( length(unique(test_reference_names)) != length(test_reference_names) )
    testthat::fail( "Could not generate 20 unique names" )
  
  test_reference_values <- base::replicate( 20, 
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ), 
                                            simplify = TRUE )
  
  
  # inject cxlib properties file in .libPaths
  
  for ( xi in 0:(length(test_levels)-1) ) {
    
    test_cxlib_path <- file.path( test_root, test_levels[xi+1], "cxlib", fsep = "/" )
    
    if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
      testthat::fail("Could not stage cxlib in test area")
    
    prop_values <- test_reference_values[ (10*xi + 1):10*(xi+1) ]
    prop_names  <- test_reference_names[ (10*xi + 1):10*(xi+1) ]
    
    base::writeLines( c( "# test properties file", 
                         paste( sort(prop_names), prop_values, sep = " = " )),
                      con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
    
    if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
      testthat::fail( paste( "Could not stage cxlib.properties at level", xi ) )
  }
  
  # -- test
  result <- cxlib::cxlib_config()
  

  # -- expected
  expected_propfiles <- file.path( file.path( test_root, test_levels[1], "cxlib", fsep = "/" ), "cxlib.properties", fsep = "/")
  
  expected_props <- utils::head( base::trimws(test_reference_values), n = 10 )
  names(expected_props) <- utils::head( base::tolower(sort(test_reference_names)), n = 10 )
  
  expected_attr <- list( ".internal" = list( "property.files" = expected_propfiles ),
                         "cxlib" = expected_props ) 
  

  # -- assertions
  testthat::expect_equal( result$.attr, expected_attr )

})





testthat::test_that( "config.cxlibSingleFile", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # test property value
  test_reference_names <- sort(utils::head( base::replicate( 1000, 
                                                             base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) ),
                                                             simplify = TRUE ), 
                                            n = 20 ))
  
  if ( length(unique(test_reference_names)) != length(test_reference_names) )
    testthat::fail( "Could not generate 20 unique names" )
  
  test_reference_values <- base::replicate( 20, 
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ), 
                                            simplify = TRUE )
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_libpath <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_libpath ) && ! dir.create( test_cxlib_libpath, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  
  base::writeLines( c( "# test properties file", 
                       paste( utils::head( sort(test_reference_names), n = 10), utils::head( test_reference_values, n = 10 ), sep = " = " )),
                    con = file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib properties file" )
  
  
  
    
  # inject properties file in file
  # note: using random context
  test_context <- base::tolower( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 10 ), collapse = "" ) )
  test_prop_file <- file.path( test_root, paste0( test_context, ".properties" ), fsep = "/" )
  
  base::writeLines( c( "# test properties file", 
                       paste( utils::tail( sort(test_reference_names), n = 10), utils::tail( test_reference_values, n = 10 ), sep = " = " )),
                    con = test_prop_file )

  if ( ! file.exists( test_prop_file ) )
    testthat::fail( "Could not stage custom properties file" )
  

  # -- test
  result <- cxlib::cxlib_config( test_prop_file )
  

  
  # -- expected
  expected_propfiles <- c( file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/"), 
                           test_prop_file )
  
  expected_attr <- list( ".internal" = list( "property.files" = expected_propfiles ) ) 

    
  expected_props <- list()
  
  expected_props[["cxlib"]] <- utils::head( base::trimws(test_reference_values), n = 10 )
  names(expected_props[["cxlib"]]) <- utils::head( base::tolower(sort(test_reference_names)), n = 10 )
  
  expected_props[[test_context]] <- utils::tail( base::trimws(test_reference_values), n = 10 )
  names(expected_props[[test_context]]) <- utils::tail( base::tolower(sort(test_reference_names)), n = 10 )
  
  expected_attr <- append( expected_attr, 
                           expected_props ) 
  
  
  # -- assertions
  testthat::expect_equal( result$.attr, expected_attr )


})




testthat::test_that( "config.cxlibSingleDir", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # test property value
  test_reference_names <- sort(utils::head( base::replicate( 1000, 
                                                             base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) ),
                                                             simplify = TRUE ), 
                                            n = 20 ))
  
  if ( length(unique(test_reference_names)) != length(test_reference_names) )
    testthat::fail( "Could not generate 20 unique names" )
  
  test_reference_values <- base::replicate( 20, 
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ), 
                                            simplify = TRUE )
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_libpath <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_libpath ) && ! dir.create( test_cxlib_libpath, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  
  base::writeLines( c( "# test properties file", 
                       paste( utils::head( sort(test_reference_names), n = 10), utils::head( test_reference_values, n = 10 ), sep = " = " )),
                    con = file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib properties file" )
  
  
  
  
  # inject properties file in file
  # note: using random context
  test_context <- base::tolower( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 10 ), collapse = "" ) )
  test_prop_file <- file.path( test_root, paste0( test_context, ".properties" ), fsep = "/" )
  
  base::writeLines( c( "# test properties file", 
                       paste( utils::tail( sort(test_reference_names), n = 10), utils::tail( test_reference_values, n = 10 ), sep = " = " )),
                    con = test_prop_file )
  
  if ( ! file.exists( test_prop_file ) )
    testthat::fail( "Could not stage custom properties file" )
  
  
  # -- test
  result <- cxlib::cxlib_config( test_root )
  
  
  
  # -- expected
  expected_propfiles <- c( file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/"), 
                           test_prop_file )
  
  expected_attr <- list( ".internal" = list( "property.files" = expected_propfiles ) ) 
  
  
  expected_props <- list()
  
  expected_props[["cxlib"]] <- utils::head( base::trimws(test_reference_values), n = 10 )
  names(expected_props[["cxlib"]]) <- utils::head( base::tolower(sort(test_reference_names)), n = 10 )
  
  expected_props[[test_context]] <- utils::tail( base::trimws(test_reference_values), n = 10 )
  names(expected_props[[test_context]]) <- utils::tail( base::tolower(sort(test_reference_names)), n = 10 )
  
  expected_attr <- append( expected_attr, 
                           expected_props ) 
  
  
  # -- assertions
  testthat::expect_equal( result$.attr, expected_attr )
  
})






testthat::test_that( "config.configOptionFullReferenceTyped", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # test property value
  test_reference_names <- sort(utils::head( base::replicate( 1000, 
                                                             base::toupper( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 15 ), collapse = "" ) ),
                                                             simplify = TRUE ), 
                                            n = 20 ))
  
  if ( length(unique(test_reference_names)) != length(test_reference_names) )
    testthat::fail( "Could not generate 20 unique names" )
  
  test_reference_values <- base::replicate( 20, 
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 40 ), collapse = "" ), 
                                            simplify = TRUE )
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_libpath <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_libpath ) && ! dir.create( test_cxlib_libpath, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  
  base::writeLines( c( "# test properties file", 
                       paste( utils::head( sort(test_reference_names), n = 10), utils::head( test_reference_values, n = 10 ), sep = " = " )),
                    con = file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib properties file" )
  
  
  
  
  # inject properties file in file
  # note: using random context
  test_context <- base::tolower( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 10 ), collapse = "" ) )
  test_prop_file <- file.path( test_root, paste0( test_context, ".properties" ), fsep = "/" )
  
  base::writeLines( c( "# test properties file", 
                       paste( utils::tail( sort(test_reference_names), n = 10), utils::tail( test_reference_values, n = 10 ), sep = " = " )),
                    con = test_prop_file )
  
  if ( ! file.exists( test_prop_file ) )
    testthat::fail( "Could not stage custom properties file" )
  

  # -- test
  result <- cxlib::cxlib_config( test_root )

  
  # -- expected
  
  expected_names <- c( paste( "cxlib", utils::head( sort(test_reference_names), n = 10), sep = "/" ),
                       paste( test_context, utils::tail( sort(test_reference_names), n = 10), sep = "/" ) )
  
  expected_values <- base::trimws( c( utils::head( test_reference_values, n = 10 ), 
                                      utils::tail( test_reference_values, n = 10 ) ) )
  
  names(expected_values) <- expected_names


  
  # -- assertions

  # note: generate a list of values known not to have been used previously
  actual_values <- base::replicate( base::length(expected_names), 
                                    paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), base::rep_len( " ", 5) ), 22 ), collapse = "" ), 
                                    simplify = TRUE )
  
  names(actual_values) <- expected_names
  
  if ( any( actual_values %in% expected_values ) )
    testthat:::fail( "Could not generate a list of reference unknown values" )

    
  for ( xitem in expected_names )
    actual_values[ xitem ] <- result$option( xitem, as.type = TRUE ) 
    
  
  testthat::expect_equal( actual_values, expected_values )
  
  
})






testthat::test_that( "config.configOptionTypeEnabled", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  

  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # test property values
  
  test_values <- c( "enable", "enabled", "grant", "permit" )

  test_names <- base::toupper( paste0( "test", test_values ) )

    



  # inject cxlib properties file in .libPaths
  test_cxlib_libpath <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_libpath ) && ! dir.create( test_cxlib_libpath, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  
  base::writeLines( c( "# test properties file", 
                       paste( test_names, test_values, sep = " = ") ),
                    con = file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib properties file" )
  
  
  
  

  
  # -- test
  result <- cxlib::cxlib_config()
  
  
  # -- expected
  
  expected_names <- test_names
  
  expected_values <- base::rep_len( TRUE, length(expected_names) )
  
  
  
  # -- assertions

  actual_values <- base::rep_len( NA, length(expected_names) )
  names(actual_values) <- expected_names
  
  for ( xitem in expected_names )
    actual_values[ xitem ] <- result$option( xitem, as.type = TRUE )
  
  
  testthat::expect_true( all( actual_values ) )

})



testthat::test_that( "config.configOptionTypeDisabled", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # test property values
  
  test_values <- c( "disable", "disabled", "revoke", "deny" )
  
  test_names <- base::toupper( paste0( "test", test_values ) )
  
  
  
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_libpath <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_libpath ) && ! dir.create( test_cxlib_libpath, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  
  base::writeLines( c( "# test properties file", 
                       paste( test_names, test_values, sep = " = ") ),
                    con = file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_libpath, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib properties file" )
  
  
  
  
  
  
  # -- test
  result <- cxlib::cxlib_config()
  
  
  # -- expected
  
  expected_names <- test_names
  
  expected_values <- base::rep_len( TRUE, length(expected_names) )
  
  
  
  # -- assertions
  
  actual_values <- base::rep_len( NA, length(expected_names) )
  names(actual_values) <- expected_names
  
  for ( xitem in expected_names )
    actual_values[ xitem ] <- result$option( xitem, as.type = TRUE )
  
  
  testthat::expect_false( any( actual_values ) )
  
})







