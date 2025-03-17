#
#  Test for internal  cxlib:::.cxlib_batchjob_execd()
#
#  Archive results
#



testthat::test_that( "batchjob.execdJobMultipleProgramActionArchiveResults", {
  
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
  
  result <- cxlib:::.cxlib_batchjob_execd( test_ctl_path, work = test_workarea, archive.results = TRUE )

  
  
  # -- expected
  
  expected_post_inv <- sapply( cxlib::cxlib_standardpath( list.files( test_workarea, recursive = TRUE, include.dirs = FALSE) ), function(x) {
    digest::digest( file.path( test_workarea, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - job id
  
  expected_id <- unname( test_job[["id"]] )
  
  
  # - action
  
  expected_action_files <- character(0)
  
  for ( xidx in 1:length(test_actions) )
    expected_action_files[xidx] <- paste0( sprintf( "%03d", xidx ), 
                                           "-action-program-",
                                           test_actions[[ xidx ]][["id"]], 
                                           "-completed.json" )
  
  
  # - results archive
  
  expected_archive <- file.path( test_ctl_path, paste0( "job-", expected_id, "-results.zip" ), fsep = "/" )
  
  
  # - outputa
  
  expected_outputs <- file.path( test_output_parent, c( "output-1.txt", "output-2.txt" ), fsep = "/" )
  
  
  
  # - logs
  
  expected_logs <- paste0( tools::file_path_sans_ext(test_programs), ".Rout" )
  
  
  # - program exec
  expected_reference <- test_reference
  
  
  
  # -- assertions
  
  
  # - action
  
  testthat::expect_true( all( file.exists( file.path( test_ctl_path, expected_action_files, fsep = "/" ) ) ) )
  
  
  # - results archive
  testthat::expect_true( file.exists( expected_archive ) )
  

  # - work area deleted
  testthat::expect_false( dir.exists( test_workarea ) )
  

  
})
