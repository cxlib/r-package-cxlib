#
#  Test for internal  cxlib:::.cxlib_batchjob_execd()
#
#
#



testthat::test_that( "batchjob.execdCtlDirectoryNotExist", {
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd(), 
                          regexp = "^Directory path to actions missing or invalid$" )
  
})




testthat::test_that( "batchjob.execdCtlDirectoryNotExist", {

  # -- stage
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = base::tempdir(), fileext = "") )
  
  if ( dir.exists( test_ctl_path ) )
    testthat::fail( "Unexpected control directory exists" )
  
    
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd( test_ctl_path ), 
                          regexp = "^Directory path to actions missing or invalid$" )
  
})






testthat::test_that( "batchjob.execdWorAreaNotMissing", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd( test_ctl_path ), 
                          regexp = "^Work area directory path missing or invalid$" )
  
})





testthat::test_that( "batchjob.execdWorAreaNotExist", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  

  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) )
    testthat::fail( "Unexpected work area exists" )

  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea ), 
                          regexp = "^Work area directory path missing or invalid$" )
  
})





testthat::test_that( "batchjob.execdJobDefMissing", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )

  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Could not stage work area" )
  
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea ), 
                          regexp = "^Job definition not available$" )
  
})





testthat::test_that( "batchjob.execdJobDefNotJSON", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Could not stage work area" )
  
  
  # - job definition
  
  job_def_file <- file.path( test_ctl_path, "job.json", fsep = "/" )
  
  base::writeLines(  paste(sample( c( base::LETTERS, base::letters, as.character(0:9)), 30), collapse = "" ), 
                     con = job_def_file )
  
  if ( ! file.exists( job_def_file) )
    testthat::fail( "Unexpected job definition file does not exist" )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea ), 
                          regexp = "^Could not import job definition$" )
  
})





testthat::test_that( "batchjob.execdJobDefJobIDMissing", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Could not stage work area" )
  
  
  # - job definition

  test_job <- list( paste(sample( c( base::LETTERS, base::letters, as.character(0:9)), 30), collapse = "" ) )
  names(test_job) <- paste(sample( base::letters, 10), collapse = "" )
  

  job_def_file <- file.path( test_ctl_path, "job.json", fsep = "/" )
  
  base::writeLines(  jsonlite::toJSON( test_job, pretty = TRUE), 
                     con = job_def_file )
  
  if ( ! file.exists( job_def_file) )
    testthat::fail( "Unexpected job definition file does not exist" )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea ), 
                          regexp = "^Job ID not defined$" )
  
})



testthat::test_that( "batchjob.execdJobDefJobIDInvalidFormat", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Could not stage work area" )
  
  
  # - job definition
  
  test_job <- list( "id" = paste(sample( c( base::LETTERS, base::letters, as.character(0:9)), base::nchar( uuid::UUIDgenerate())), collapse = "" ) )

  
  job_def_file <- file.path( test_ctl_path, "job.json", fsep = "/" )
  
  base::writeLines(  jsonlite::toJSON( test_job, pretty = TRUE), 
                     con = job_def_file )
  
  if ( ! file.exists( job_def_file) )
    testthat::fail( "Unexpected job definition file does not exist" )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea ), 
                          regexp = "^Job ID is in an invalid format$" )
  
})





testthat::test_that( "batchjob.execdEmptyJob", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Could not stage work area" )
  
  
  # - job definition
  
  test_job <- list( "id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  job_def_file <- file.path( test_ctl_path, "job.json", fsep = "/" )
  
  base::writeLines(  jsonlite::toJSON( test_job, pretty = TRUE), 
                     con = job_def_file )
  
  if ( ! file.exists( job_def_file) )
    testthat::fail( "Unexpected job definition file does not exist" )
  
  
  # -- test
  
  result <- cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea )


  # -- expected
  
  expected_attr <- c( "id", "start", "complete" )
  
    
  # -- assertions
  
  # - simple return
  testthat::expect_true( result )

    
  # - job definition update
  result_jobdef <- jsonlite::fromJSON( readLines( job_def_file ) )
  
  testthat::expect_true( all( expected_attr %in% names(result_jobdef) ) )

})




testthat::test_that( "batchjob.execdJobInvalidActionFileFormat", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Could not stage work area" )
  
  
  # - job definition
  
  test_job <- list( "id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  job_def_file <- file.path( test_ctl_path, "job.json", fsep = "/" )
  
  base::writeLines(  jsonlite::toJSON( test_job, pretty = TRUE), 
                     con = job_def_file )
  
  if ( ! file.exists( job_def_file) )
    testthat::fail( "Unexpected job definition file does not exist" )
  
  
  # - action
  
  test_action_file <- paste0( "001-action-program-", cxlib:::.cxlib_referenceid( type = "action"), ".json" )
  
  # note: not JSON
  base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 20), collapse = ""), 
                    con = file.path( test_ctl_path, test_action_file, fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_ctl_path, test_action_file, fsep = "/" ) ) )
    testthat::fail( "Could not stage invalid action file format" )
  
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea ), 
                          regexp = paste0( "^Failed to import action ", test_action_file, "$" ) )
  
})




testthat::test_that( "batchjob.execdStaleAbandonedJob", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Could not stage work area" )
  
  
  # - job definition
  
  test_job <- list( "id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  job_def_file <- file.path( test_ctl_path, "job.json", fsep = "/" )
  
  base::writeLines(  jsonlite::toJSON( test_job, pretty = TRUE), 
                     con = job_def_file )
  
  if ( ! file.exists( job_def_file) )
    testthat::fail( "Unexpected job definition file does not exist" )
  
  
  # - inject actions files representing an ongoing job
  
  test_action_files <- c( paste0( "001-action-program-", cxlib:::.cxlib_referenceid( type = "action"), "-completed.json" ), 
                          paste0( "002-action-program-", cxlib:::.cxlib_referenceid( type = "action"), ".lck" ), 
                          paste0( "003-action-program-", cxlib:::.cxlib_referenceid( type = "action"), ".json" ) )
  
  for ( xfile in test_action_files )
    base::writeLines( "{}", con = file.path( test_ctl_path, xfile, fsep = "/") )
  
  if ( ! all( file.exists( file.path( test_ctl_path, test_action_files, fsep = "/") )) )
    testthat::fail( "Could not stage a stale/abandoned set of actions")

  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea ), 
                          regexp = "^Specified job is active or stale$" )
  
  
})




testthat::test_that( "batchjob.execdJobInvalidActionFileFormat", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Could not stage work area" )
  
  
  # - job definition
  
  test_job <- list( "id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  job_def_file <- file.path( test_ctl_path, "job.json", fsep = "/" )
  
  base::writeLines(  jsonlite::toJSON( test_job, pretty = TRUE), 
                     con = job_def_file )
  
  if ( ! file.exists( job_def_file) )
    testthat::fail( "Unexpected job definition file does not exist" )
  
  
  # - action
  
  test_action_file <- paste0( "001-action-program-", cxlib:::.cxlib_referenceid( type = "action"), ".json" )
  
  # note: not JSON
  base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 20), collapse = ""), 
                    con = file.path( test_ctl_path, test_action_file, fsep = "/" ) )
  
  test_action <- list( "type" = paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 20), collapse = "") )

  base::writeLines( jsonlite::toJSON( test_action ), 
                    con = file.path( test_ctl_path, test_action_file, fsep = "/" ) )
    
  if ( ! file.exists( file.path( test_ctl_path, test_action_file, fsep = "/" ) ) )
    testthat::fail( "Could not stage invalid action file format" )
  
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea ), 
                          regexp = paste0( "^Action ", base::toupper(test_action[["type"]]), " not supported$" ) )
  
})




testthat::test_that( "batchjob.execdJobSingleProgramAction", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Could not stage work area" )
  
  
  # - programs
  
  test_reference <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 20), collapse = "")
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/")
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )

  test_program <- file.path( test_program_parent, 
                             base::basename( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                             fileext = ".R") ),
                             fsep = "/")  

  
  base::writeLines( c( "# test program", 
                       paste0( "cat(\"",  test_reference, "\", sep = \"\\n\")" ) ), 
                    con = file.path( test_workarea, test_program, fsep = "/" ) )
  
  test_program_sha <- digest::digest( file.path( test_workarea, test_program, fsep = "/" ), algo = "sha1", file = TRUE )
  

  # - test action
  
  test_action_id <- cxlib:::.cxlib_referenceid( type = "action")
  
  test_action_file <- paste0( "001-action-program-", test_action_id, ".json" )

  test_action <- list( "id" = test_action_id, 
                       "type" = "program", 
                       "path" = test_program, 
                       "sha1" = test_program_sha, 
                       "log" = list( "path" = paste0( tools::file_path_sans_ext( test_program), ".Rout" ) )
  )
  
  base::writeLines( jsonlite::toJSON( test_action ), 
                    con = file.path( test_ctl_path, test_action_file, fsep = "/") )
  
  if ( ! file.exists( file.path( test_ctl_path, test_action_file, fsep = "/") ) )
    testthat::fail( "Could not stage test action" )
  
  
  
  # - job definition
  
  test_job <- list( "id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  job_def_file <- file.path( test_ctl_path, "job.json", fsep = "/" )
  
  base::writeLines(  jsonlite::toJSON( test_job, pretty = TRUE), 
                     con = job_def_file )
  
  if ( ! file.exists( job_def_file) )
    testthat::fail( "Unexpected job definition file does not exist" )
  
  

  
  # -- test

  result <- cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea )

  
  # -- expected
  
  # - program
  expected_program_sha <- test_program_sha
  
  # - log
  expected_log <- list( "path" = paste0( tools::file_path_sans_ext( test_program), ".Rout" ), 
                        "sha1" = digest::digest( file.path( test_workarea, paste0( tools::file_path_sans_ext( test_program), ".Rout" ), fsep = "/"), algo = "sha1", file = TRUE  ) )
  
  expected_log_reference <- test_reference
  
  
  # - action
  expected_action_file <- paste0( tools::file_path_sans_ext( test_action_file ), "-completed.json" )
    
  
  # - program exec
  expcted_reference <- test_reference
  
  
  # -- assertions
  
  
  # - action
  testthat::expect_true( file.exists( file.path( test_ctl_path, expected_action_file, fsep = "/" ) ) )
  
  # read in completed action 
  result_action <- jsonlite::fromJSON( file.path( test_ctl_path, expected_action_file, fsep = "/" ) )


  # - program integrity
  testthat::expect_true( file.exists( file.path( test_workarea, test_program, fsep = "/" ) ) )
  testthat::expect_equal( digest::digest( file.path( test_workarea, test_program, fsep = "/" ), algo = "sha1", file = TRUE), expected_program_sha )
  
  # - log
  testthat::expect_true( file.exists( file.path( test_workarea, expected_log[["path"]], fsep = "/" ) ) )
  testthat::expect_equal( result_action[["log"]], expected_log )

  
  # - verify program execution
  result_log <- readLines( file.path( test_workarea, expected_log[["path"]], fsep = "/" ) )
  testthat::expect_true( any( grepl( paste0( "^", test_reference, "$"), result_log, perl = TRUE, ignore.case = FALSE ) ) )


})






testthat::test_that( "batchjob.execdJobMultipleProgramAction", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - control directory
  
  test_ctl_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-ctl-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_ctl_path ) || ! dir.create( test_ctl_path, recursive = TRUE ) )
    testthat::fail( "Could not stage control directory" )
  
  
  # - work area
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Could not stage work area" )

    
  # - output area

  test_output_parent <- file.path( "path", "to", "outputs", fsep = "/")
  
  if ( dir.exists( file.path( test_workarea, test_output_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_output_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test output parent directory" )
  
  
  # - programs
  
  test_reference <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 20), collapse = "")
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/")
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  test_programs <- replicate( 2, 
                              file.path( test_program_parent, 
                                         base::basename( base::tempfile( pattern = "test-program-", 
                                                                         tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                                         fileext = ".R") ),
                                         fsep = "/"), 
                              simplify = TRUE )
  
  
  # program 1
  
  base::writeLines( c( "# test program 1", 
                       paste0( "cat(\"",  test_reference, "\", sep = \"\\n\", file = \"", file.path( test_output_parent, "output-1.txt", fsep = "/"), "\")" ) ), 
                    con = file.path( test_workarea, test_programs[1], fsep = "/" ) )
  
  # program 2
  
  base::writeLines( c( "# test program 2", 
                       paste0( "txt <- readLines( \"", file.path( test_output_parent, "output-1.txt", fsep = "/"), "\" )"),
                       paste0( "cat( paste0( txt, txt), sep = \"\\n\", file = \"", file.path( test_output_parent, "output-2.txt", fsep = "/"), "\")" ) ),
                    con = file.path( test_workarea, test_programs[2], fsep = "/" ) ) 
  


  # - test actions
  
  test_actions <- list()

  for ( xprogram in test_programs ) {
    
    # define test action
    
    action_idx <- length(test_actions) + 1
    
    test_actions[[ action_idx]] <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                                         "type" = "program", 
                                         "path" = xprogram, 
                                         "sha1" = digest::digest( file.path( test_workarea, xprogram, fsep = "/"), algo = "sha1", file = TRUE ), 
                                         "log" = list( "path" = paste0( tools::file_path_sans_ext( xprogram), ".Rout" ) ) )
    

    # create action file
    
    test_action_file <- paste0( sprintf( "%03d", utils::head( match( xprogram, test_programs), n = 1 ) ), 
                                "-action-program-",
                                test_actions[[ action_idx ]][["id"]], 
                                ".json" )
    
    base::writeLines( jsonlite::toJSON( test_actions[[ action_idx ]] ), 
                      con = file.path( test_ctl_path, test_action_file, fsep = "/") )
    
    if ( ! file.exists( file.path( test_ctl_path, test_action_file, fsep = "/") ) )
      testthat::fail( "Could not stage test action" )
    

  }

  
  
  # - job definition
  
  test_job <- list( "id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  job_def_file <- file.path( test_ctl_path, "job.json", fsep = "/" )
  
  base::writeLines(  jsonlite::toJSON( test_job, pretty = TRUE), 
                     con = job_def_file )
  
  if ( ! file.exists( job_def_file) )
    testthat::fail( "Unexpected job definition file does not exist" )
  
  
  # - pre-inventory
  test_pre_inv <- sapply( cxlib::cxlib_standardpath( list.files( test_workarea, recursive = TRUE, include.dirs = FALSE) ), function(x) {
    digest::digest( file.path( test_workarea, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  

  
  # -- test
  
  result <- cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea )
  
  
  
  # -- expected

  expected_post_inv <- sapply( cxlib::cxlib_standardpath( list.files( test_workarea, recursive = TRUE, include.dirs = FALSE) ), function(x) {
    digest::digest( file.path( test_workarea, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  # - action
  
  expected_action_files <- character(0)

  for ( xidx in 1:length(test_actions) )
    expected_action_files[xidx] <- paste0( sprintf( "%03d", xidx ), 
                                           "-action-program-",
                                           test_actions[[ xidx ]][["id"]], 
                                           "-completed.json" )
    

  # - outputa
  
  expected_outputs <- file.path( test_output_parent, c( "output-1.txt", "output-2.txt" ), fsep = "/" )
  
    

  # - logs

  expected_logs <- paste0( tools::file_path_sans_ext(test_programs), ".Rout" )

    
  # - program exec
  expected_reference <- test_reference
  
  
  
  # -- assertions
  
  
  # - action
  
  testthat::expect_true( all( file.exists( file.path( test_ctl_path, expected_action_files, fsep = "/" ) ) ) )
  

  # - program integrity
  testthat::expect_true( all( file.exists( file.path( test_workarea, test_programs, fsep = "/" ) ) ) )
  
  for ( xpgm in test_programs )
    testthat::expect_equal( expected_post_inv[ xpgm ], test_pre_inv[ xpgm ] )

  
  # - verify program execution through outputs
  
  testthat::expect_true( all( file.exists( file.path( test_workarea, expected_outputs, fsep = "/" ) ) ) )
  
  testthat::expect_equal( readLines( file.path( test_workarea, test_output_parent, "output-1.txt", fsep = "/" ) ), expected_reference )
  testthat::expect_equal( readLines( file.path( test_workarea, test_output_parent, "output-2.txt", fsep = "/" ) ), paste( rep_len( expected_reference, 2), collapse = "") )
  
  
  # - log
  
  testthat::expect_true( all( file.exists( file.path( test_workarea, expected_logs, fsep = "/" ) ) ) )
  
  
  
})
