#
#
#  Tests for cxlib::cxlib_batchjob()
#
#  Params and config
#
#
#


# testthat::test_that( "batchjob.noParams", {
# 
#   # -- test
#   
#   testthat::expect_error( cxlib::cxlib_batchjob(), regexp = "^Input is invalid or missing$" )
# 
# })
# 
# 
# 
# testthat::test_that( "batchjob.inputNull", {
#   
#   # -- test
#   
#   testthat::expect_error( cxlib::cxlib_batchjob( NULL ), regexp = "^Input is invalid or missing$" )
#   
# })



testthat::test_that( "batchjob.inputNA", {
  
  # -- test
  
  testthat::expect_error( cxlib::cxlib_batchjob( NA ), regexp = "^Input is invalid or missing$" )
  
})





testthat::test_that( "batchjob.inputInvalidType", {
  
  # -- test
  
  testthat::expect_error( cxlib::cxlib_batchjob( numeric(1) ), regexp = "^Input is invalid or missing$" )
  
})




testthat::test_that( "batchjob.configOptionPathNotExist", {
  
  
  # -- stage

  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  

  # - libPath
  
  test_lbpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( file.path( test_lbpath, "cxlib", fsep = "/" ) ) || ! dir.create( file.path( test_lbpath, "cxlib", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxlib libpath directory" )
  
  current_lbpath <- .libPaths()
  
  on.exit({
    .libPaths( current_lbpath )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, current_lbpath ) )
  
  
  # - cxlib properties
  
  test_jobpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-jobpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_jobpath ) )
    testthat::fail( "Unexpected test job path exists" )
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ) ),
                    con = file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) )

  if ( ! file.exists( file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) ) )  
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # - meaningless input values
  test_input <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 20 ), collapse = "" )
  
  

  # -- test
  
  testthat::expect_error( cxlib::cxlib_batchjob( test_input ), 
                          regexp = paste( "^The PATH configuration option directory", test_jobpath, "does not exist$" ) )
  
})




testthat::test_that( "batchjob.configOptionJobPathExistsWorkPathNotExist", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - libPath
  
  test_lbpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( file.path( test_lbpath, "cxlib", fsep = "/" ) ) || ! dir.create( file.path( test_lbpath, "cxlib", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxlib libpath directory" )
  
  current_lbpath <- .libPaths()
  
  on.exit({
    .libPaths( current_lbpath )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, current_lbpath ) )
  
  
  # - test job path
  
  test_jobpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-jobpath-", tmpdir = test_root, fileext = "" ) )

  if ( dir.exists( test_jobpath ) || ! dir.create( test_jobpath, recursive = TRUE ) )
    testthat::fail( "Could not stage test job directory" )
  
  
  # - test work area path
  
  test_workpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-workpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_workpath ) )
    testthat::fail( "Unexpected test work path exists" )
  
    
  # - cxlib properties
  
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ),
                       paste0( "WORKPATH=", test_workpath ) ),
                    con = file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) ) )  
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # - meaningless input values
  test_input <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 20 ), collapse = "" )
  
  
  
  # -- test
  
  testthat::expect_error( cxlib::cxlib_batchjob( test_input ), 
                          regexp = paste( "^The WORKPATH configuration option directory", test_workpath, "does not exist$" ) )
  
})





testthat::test_that( "batchjob.configOptionJobPathNotDefWorkPathNotExist", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - libPath
  
  test_lbpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( file.path( test_lbpath, "cxlib", fsep = "/" ) ) || ! dir.create( file.path( test_lbpath, "cxlib", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxlib libpath directory" )
  
  current_lbpath <- .libPaths()
  
  on.exit({
    .libPaths( current_lbpath )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, current_lbpath ) )
  

  
  # - test work area path
  
  test_workpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-workpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_workpath ) )
    testthat::fail( "Unexpected test work path exists" )
  
  
  # - cxlib properties
  
  
  base::writeLines( c( "# test properties", 
                       paste0( "WORKPATH=", test_workpath ) ),
                    con = file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) ) )  
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # - meaningless input values
  test_input <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 20 ), collapse = "" )
  
  
  
  # -- test
  
  testthat::expect_error( cxlib::cxlib_batchjob( test_input ), 
                          regexp = paste( "^The WORKPATH configuration option directory", test_workpath, "does not exist$" ) )
  
})




testthat::test_that( "batchjob.validJobIDNotExist", {
  
  # -- stage
  
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # -- test
  
  testthat::expect_error( cxlib::cxlib_batchjob( test_id ), regexp = "^A job with the specified ID is not defined$" )
  
})





testthat::test_that( "batchjob.jobDefIDExists", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - libPath
  
  test_lbpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-libpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( file.path( test_lbpath, "cxlib", fsep = "/" ) ) || ! dir.create( file.path( test_lbpath, "cxlib", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage cxlib libpath directory" )
  
  current_lbpath <- .libPaths()
  
  on.exit({
    .libPaths( current_lbpath )
  }, add = TRUE )
  
  .libPaths( c( test_lbpath, current_lbpath ) )
  
  
  # - test job path
  
  test_jobpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-jobpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_jobpath ) || ! dir.create( test_jobpath, recursive = TRUE ) )
    testthat::fail( "Could not stage test job directory" )
  
  
  # - test job
  
  test_job_id <- cxlib:::.cxlib_referenceid( type = "job" )
  

  if ( dir.exists( file.path( test_jobpath, test_job_id, fsep = "/" ) ) || ! dir.create( file.path( test_jobpath, test_job_id, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test job" )
  

  # - cxlib properties
  
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath )),
                    con = file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) ) )  
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  
  # -- test
  
  testthat::expect_error( cxlib::cxlib_batchjob( list( "id" = test_job_id ) ), 
                          regexp = "^A job with the specified ID already exists$" )
  
})


