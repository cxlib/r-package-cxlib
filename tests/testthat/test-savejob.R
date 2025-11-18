#
#
# Tests for save cxlib jobs
#
#


#' @cx.testsfor cxlib::cxlib_savejob()




testthat::test_that( "savejob.invalidJob", {
  
  #' @cx.tests Save job to file results in error when job is an invalid type
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  

  
  # - destination file
  
  test_targetfile <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-job-", 
                                                                tmpdir = base::tempfile( pattern = "test-jobdir-", tmpdir = test_root, fileext = ""),
                                                                fileext = ".job") )
  
  
  if ( dir.exists( base::dirname(test_targetfile) ) || ! dir.create( base::dirname(test_targetfile), recursive = TRUE )  )
    testthat::fail( "Target directory could not be created" )
  
  
  # - invalid job
  test_job <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), base::sample(20:50, 1), replace = TRUE ), collapse = "" )

  
  # -- test
  testthat::expect_error( cxlib::cxlib_savejob( test_job, test_targetfile ), regexp = "^The specified job is an invalid type$" )
  
  
})




testthat::test_that( "savejob.parentDirectoryNotExist", {
  
  #' @cx.tests Save job to file results in error when parent directory does not exist
  

  # -- stage

  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
    
  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_job$add, test_args )
    
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  
  
  
  # - destination file
  
  test_targetdir <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-jobdir-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_targetdir ) )
    testthat::fail( "Unexpected target directory exists")
  
  test_targetfile <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-job-", tmpdir = test_targetdir, fileext = ".job") )
  
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_savejob( test_job, test_targetfile ), regexp = "^The parent directory for file path does not exist$" )

  
})




testthat::test_that( "savejob.pathIsDirectory", {
  
  #' @cx.tests Save job to file results in error when the file path is a  directory
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, test_actions[[xtype]] )
    test_stage <- do.call( test_job$add, test_args )

    # clean up arguments
    base::rm(test_args)
  } 
  
  
  
  # - destination file

  test_targetfile <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-job-", 
                                                                tmpdir = base::tempfile( pattern = "test-jobdir-", tmpdir = test_root, fileext = ""),
                                                                fileext = ".job") )
  
  
  if ( dir.exists( test_targetfile ) || ! dir.create( test_targetfile, recursive = TRUE )  )
    testthat::fail( "Target directory could not be created" )
  
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_savejob( test_job, test_targetfile ), regexp = "^The file path is a directory$" )
  
  
})




testthat::test_that( "savejob.pathNull", {
  
  #' @cx.tests Save job to file results in error when the file path is NULL
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, test_actions[[xtype]] )
    test_stage <- do.call( test_job$add, test_args )
    
    # clean up arguments
    base::rm(test_args)
  } 
  

  # -- test
  testthat::expect_error( cxlib::cxlib_savejob( test_job, NULL ), regexp = "^The file path is missing or invalid$" )

})



testthat::test_that( "savejob.pathNA", {
  
  #' @cx.tests Save job to file results in error when the file path is NA
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, test_actions[[xtype]] )
    test_stage <- do.call( test_job$add, test_args )
    
    # clean up arguments
    base::rm(test_args)
  } 
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_savejob( test_job, NA ), regexp = "^The file path is missing or invalid$" )
  
})


testthat::test_that( "savejob.pathEmptyString", {
  
  #' @cx.tests Save job to file results in error when the file path is an empty string
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, test_actions[[xtype]] )
    test_stage <- do.call( test_job$add, test_args )
    
    # clean up arguments
    base::rm(test_args)
  } 
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_savejob( test_job, "   " ), regexp = "^The file path is missing or invalid$" )
  
})






testthat::test_that( "savejob.savedAsFile", {
  
  #' @cx.tests Save job to file 
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, test_actions[[xtype]] )
    test_stage <- do.call( test_job$add, test_args )
    
    # clean up arguments
    base::rm(test_args)
  } 
  
  
  
  # - destination file
  
  test_targetfile <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-job-", 
                                                                tmpdir = base::tempfile( pattern = "test-jobdir-", tmpdir = test_root, fileext = ""),
                                                                fileext = ".job") )
  
  
  if ( dir.exists( base::dirname(test_targetfile) ) || ! dir.create( base::dirname(test_targetfile), recursive = TRUE )  )
    testthat::fail( "Target directory could not be created" )
  
  
  
  # -- test
  result <- cxlib::cxlib_savejob( test_job, test_targetfile )
  
  
  # -- expected
  
  expected_file <- test_targetfile
  
  expected_hash <- test_job$hash()

  # -- assertions
  
  # - file exists
  testthat::expect_true( file.exists(test_targetfile) )
  
  
  # - json
  chk_job <- cxlib::cxlib_job()
  chk_job$fromJSON( paste( base::readLines( expected_file ), collapse = " " ) )
  
  testthat::expect_equal( chk_job$hash(), expected_hash )
  
})
