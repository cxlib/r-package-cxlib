#
#
# Tests for read cxlib jobs
#
#


#' @cx.testsfor cxlib::cxlib_readjob()




testthat::test_that( "readjob.pathMissing", {
  
  #' @cx.tests Reead job to file results in error when file path is not specified

  
  # -- test
  testthat::expect_error( cxlib::cxlib_readjob(), regexp = "^The specified file is missing, invalid or does not exist$" )
  
})


testthat::test_that( "readjob.pathNull", {
  
  #' @cx.tests Reead job to file results in error when file path is NULL
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_readjob(NULL), regexp = "^The specified file is missing, invalid or does not exist$" )
  
})


testthat::test_that( "readjob.pathNA", {
  
  #' @cx.tests Reead job to file results in error when file path is NA
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_readjob(NA), regexp = "^The specified file is missing, invalid or does not exist$" )
  
})



testthat::test_that( "readjob.pathEmptyString", {
  
  #' @cx.tests Reead job to file results in error when file path is an empty string
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_readjob("  "), regexp = "^The specified file is missing, invalid or does not exist$" )
  
})




testthat::test_that( "readjob.pathMultipleFiles", {
  
  #' @cx.tests Reead job to file results in error when file path is a vector of multiple paths
  
  
  # -- stage 
  
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - target files
  
  test_files <- replicate( 5, 
                           cxlib::cxlib_standardpath( base::tempfile( pattern = "test-job-", tmpdir = test_root, fileext = ".job") ),
                           simplify = TRUE )
  
  for ( xfile in test_files )
    base::writeLines( "# test file", con = xfile  )

  if ( any( ! file.exists(test_files) ) )
    testthat::fail( "Could not stage test files" )

  
  # -- test
  testthat::expect_error( cxlib::cxlib_readjob( test_files ), regexp = "^The specified file is missing, invalid or does not exist$" )
  
})




testthat::test_that( "readjob.jobFileNotJSON", {
  
  #' @cx.tests Reead job to file results in error when job file is not a JSON file
  
  
  # -- stage 
  
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - target files
  
  test_files <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-job-", tmpdir = test_root, fileext = ".job") )
  
  for ( xfile in test_files )
    base::writeLines( "# test file", con = xfile  )
  
  if ( any( ! file.exists(test_files) ) )
    testthat::fail( "Could not stage test files" )
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_readjob( test_files ))
  
})




testthat::test_that( "readjob.jobFile", {
  
  #' @cx.tests Read job from file 
  
  
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
  
  
  # - stage saved file
  base::writeLines( test_job$toJSON(), con = test_targetfile )
  
  if ( ! file.exists( test_targetfile ) )
    testthat::fail( "Could not stage job as JSON file" )
  
  
  # -- test
  result <- cxlib::cxlib_readjob( test_targetfile )
  
  
  # -- expected
  
  expected_hash <- test_job$hash()

    
  # -- assertions
  
  testthat::expect_true( inherits( result, "cxlib_job" ) )
  testthat::expect_equal( base::attr(class(result), "package"), "cxlib" )
  

  testthat::expect_equal( result$hash(), expected_hash )
    

})




