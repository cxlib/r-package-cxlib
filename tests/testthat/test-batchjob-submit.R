#
#
#  Tests for cxlib::cxlib_batchjob()
#
#  Submit
#
#
#  Note: wait for multiple programs is expected to be the same as wait for one 
#        just a lot longer 
#



testthat::test_that( "batchjob.submitSingleProgram", {
  
  
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
  
  
  
  # - test working directory
  
  test_wd <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_wd ) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  current_wd <- base::getwd()
  
  on.exit({
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( test_wd )
  
  
  
  # - cxlib properties
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ) ),
                    con = file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) ) )  
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # - test reference
  
  test_reference <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 20 ), collapse = "" )
  

  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  base::writeLines( c( "# Test program",
                       paste0( "cat( \"", test_reference, "\", sep = \"\\n\" )" ) ), 
                      con = test_program )
  
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program exists" )
  
  
  test_program_refs <- base::substring( test_program, base::nchar(test_wd) + 2 )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_refs ) )
  
 
  # -- test

  test_job$submit( wait = TRUE )
  

  # -- expected
  
  # - job id
  expected_id <- test_id
  
  
  # - programs
  expected_programs <- test_program_refs
  
  
  # - logs
  expected_logs <- paste0( tools::file_path_sans_ext( expected_programs ), ".Rout" ) 


  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )

  # - log
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_logs, fsep = "/" ) ) ) )

  
  # - ensure program acions executed
  
  result_log <- readLines( file.path( test_jobpath, expected_id, ".work", expected_logs, fsep = "/" ) )
  testthat::expect_true( any(grepl( paste0( "^", test_reference, "$" ), result_log )) )

})





testthat::test_that( "batchjob.submitMultiProgram", {
  
  
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
  
  
  
  # - test working directory
  
  test_wd <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_wd ) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  current_wd <- base::getwd()
  
  on.exit({
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( test_wd )
  
  
  
  # - cxlib properties
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ) ),
                    con = file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) ) )  
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # - test reference
  
  test_references <- replicate( 5, 
                                paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 20 ), collapse = "" ), 
                                simplify = TRUE )
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_programs <- replicate( 5, 
                              cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                                         tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                                         fileext = ".R" ) ), 
                              simplify = TRUE )
  
  for ( xpgm in test_programs ) {
    
    base::writeLines( c( "# Test program",
                         paste0( "cat( \"", test_references[ match( xpgm, test_programs ) ], "\", sep = \"\\n\" )" ) ), 
                      con = xpgm )
    
    
    if ( ! file.exists( xpgm ) )
      testthat::fail( "Unexpected test program exists" )
  
  }
    
  
  test_program_refs <- base::substring( test_programs, base::nchar(test_wd) + 2 )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_refs ) )
  
  
  # -- test

  test_job$submit( wait = TRUE )

  
  
  # -- expected
  
  # - job id
  expected_id <- test_id
  
  # - references
  expected_references <- test_references
  
  # - programs
  expected_programs <- test_program_refs
  
  
  # - logs
  expected_logs <- paste0( tools::file_path_sans_ext( expected_programs ), ".Rout" ) 
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )
  
  # - log
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_logs, fsep = "/" ) ) ) )
  
  
  # - ensure program acions executed
  
  for ( xlog in expected_logs ) {
    
    result_log <- readLines( file.path( test_jobpath, expected_id, ".work", xlog, fsep = "/" ) )
    testthat::expect_true( any(grepl( paste0( "^", expected_references[ match( xlog, expected_logs ) ], "$" ), result_log )) )
    
    base::rm( list = "result_log" )
  }
  
  
})




testthat::test_that( "batchjob.submitSingleProgramWaitDisabled", {
  
  
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
  
  
  
  # - test working directory
  
  test_wd <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_wd ) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  current_wd <- base::getwd()
  
  on.exit({
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( test_wd )
  
  
  
  # - cxlib properties
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ) ),
                    con = file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) ) )  
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # - test reference
  
  test_reference <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 20 ), collapse = "" )
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  base::writeLines( c( "# Test program",
                       "# sleep for 2 seconds",
                       "Sys.sleep(2)",
                       paste0( "cat( \"", test_reference, "\", sep = \"\\n\" )" ) ), 
                    con = test_program )
  
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program exists" )
  
  
  test_program_refs <- base::substring( test_program, base::nchar(test_wd) + 2 )
  

  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_refs ) )
  
  
  # identify test job action

  test_action_result_file <- paste0( tools::file_path_sans_ext( list.files( file.path( test_jobpath, test_id, ".job", fsep = "/" ), 
                                                                            pattern = "^\\d+-action-.*", 
                                                                            full.names = TRUE, 
                                                                            recursive = FALSE, 
                                                                            include.dirs = FALSE )), 
                                     "-completed.json")
  
  
  # -- test
  
  test_job$submit( wait = FALSE )

  if ( file.exists( test_action_result_file ) ) 
    testthat::fail( "Unexpected short run time" )
    

  # - monitor for completed action
  #   note: time-loop for 20 second futility easiest we can do
    
  test_completed <- FALSE
  
  for ( idx in 1:20 ) {
    
    Sys.sleep(1)
    
    if ( file.exists( test_action_result_file ) ) {
      test_completed <- TRUE
      break()
    }
  }

  if ( ! test_completed )
    testthat::fail( "Unexpected run time duration exceeded 20 seconds")
  


  # -- expected
  
  # - job id
  expected_id <- test_id

  
  # - logs
  expected_logs <- paste0( tools::file_path_sans_ext( test_program_refs ), ".Rout" ) 


  # - expected archive
  expected_archive <- file.path( test_jobpath, test_id, ".job", paste0( "job-", expected_id, "-results.zip"), fsep = "/" )

  expected_archive_files <- expected_logs
  

  
  
  
  # -- assertions
  
  # - results archive
  testthat::expect_true( file.exists( expected_archive ) )
  
  
  # - deleted work area 
  testthat::expect_false( dir.exists( file.path( test_jobpath, expected_id, ".work", fsep = "/" ) ) ) 

  
  # - inspect results archive
  results_archive_contents <- zip::zip_list( expected_archive )

  testthat::expect_equal( sort(results_archive_contents[, "filename"]), sort(expected_archive_files) )

})

