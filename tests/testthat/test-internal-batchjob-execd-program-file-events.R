#
#  Tests for cxlib:::.cxlib_batchjob_execd_program()
#  
#  Files creared
#
#


testthat::test_that( "batchjob.execdSingleFileCreate", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - work area  
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-area-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Cannot stage test work area" )
  
  
  # - test output to create
  
  test_output_parent <- "path/to/outputs"
  
  if ( dir.exists( file.path( test_workarea, test_output_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_output_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test output parent directory" )
  
  
  test_output_file <- file.path( test_output_parent, 
                                 base::basename( base::tempfile( pattern = "test-output-", 
                                                                 tmpdir = file.path( test_workarea, test_output_parent, fsep = "/" ), 
                                                                 fileext = ".txt") ),
                                 fsep = "/" )
  
  if ( file.exists(file.path( test_workarea, test_output_file, fsep = "/" )) )
    testthat::fail( "Unexpected output file exists" )
  
  
  test_output_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 30 ), collapse = "")
  
  
  
  # - test program
  
  test_program_parent <- "path/to/programs"
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                             fileext = ".R") )
  
  base::writeLines( c( "# test program", 
                       paste0( "writeLines( \"", test_output_value, "\", con = \"", test_output_file, "\")") ),
                       con = test_program )
  
  
  test_program_ref <- base::substring( test_program, base::nchar( test_workarea ) + 2 )
  
  
  # - test log
  
  test_log_parent <- "path/to/logs"
  
  if ( dir.exists( file.path( test_workarea, test_log_parent, fsep = "/") ) || 
       ! dir.create( file.path( test_workarea, test_log_parent, fsep = "/"), recursive = TRUE ) )
    testthat::fail( "Could not stage test log directory" )
  
  test_log <- file.path( test_log_parent, 
                         paste0( tools::file_path_sans_ext(base::basename(test_program_ref)), ".log"), fsep = "/" )
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(),
                       "type" = "program", 
                       "path" = test_program_ref, 
                       "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                       "log" = list( "path" = test_log ) )
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  result <- cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts )

  
  # -- expected
  
  # - start with action
  expected_results <- test_action
  
  # - add log 
  expected_results[["log"]][["sha1"]] <- digest::digest( file.path( test_workarea, test_log, fsep = "/"), algo = "sha1", file = TRUE )
  
  # - expected output
  expected_output <- list( "path" = test_output_file, 
                           "sha1" = digest::digest( file.path( test_workarea, test_output_file, fsep = "/" ), algo = "sha1", file = TRUE ) )
  
  
  # - add inputs
  expected_results <- append( expected_results, 
                              list( "files.input" = list( test_action[ c( "path", "sha1") ] ), 
                                    "files.created" = list( expected_results[["log"]], expected_output ),
                                    "files.updated" = list(), 
                                    "files.deleted" = list() )  
  )
  
  
  # -- assertions
  
  # - results
  testthat::expect_equal( result[ names(expected_results) ], expected_results[ names(expected_results) ] )
  
  # - program
  testthat::expect_true( file.exists( file.path( test_workarea, expected_results[["path"]], fsep = "/" ) ) )
  testthat::expect_equal( digest::digest( file.path( test_workarea, expected_results[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ),
                          expected_results[["sha1"]] )
  
  # - log
  testthat::expect_true( file.exists( file.path( test_workarea, expected_results[["log"]][["path"]], fsep = "/" ) ) )
  testthat::expect_equal( digest::digest( file.path( test_workarea, expected_results[["log"]][["path"]], fsep = "/" ), algo = "sha1", file = TRUE ),
                          expected_results[["log"]][["sha1"]] )
  
})





testthat::test_that( "batchjob.execdSingleFileUpdate", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - work area  
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-area-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Cannot stage test work area" )
  
  
  # - test output to create
  
  test_output_parent <- "path/to/outputs"
  
  if ( dir.exists( file.path( test_workarea, test_output_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_output_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test output parent directory" )
  
  
  test_output_file <- file.path( test_output_parent, 
                                 base::basename( base::tempfile( pattern = "test-output-", 
                                                                 tmpdir = file.path( test_workarea, test_output_parent, fsep = "/" ), 
                                                                 fileext = ".txt") ),
                                 fsep = "/" )
  
  base::writeLines( c( "# test update existing output", 
                       paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 30 ), collapse = "") ), 
                    con = file.path( test_workarea, test_output_file, fsep = "/" ) )
  
  
  if ( ! file.exists(file.path( test_workarea, test_output_file, fsep = "/" )) )
    testthat::fail( "Unexpected output file does not exist" )
  
  
  test_output_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 30 ), collapse = "")
  
  test_output_ref_sha1 <- digest::digest( file.path( test_workarea, test_output_file, fsep = "/" ), algo = "sha1", file = TRUE )
  
  
  # - test program
  
  test_program_parent <- "path/to/programs"
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                             fileext = ".R") )
  
  base::writeLines( c( "# test program", 
                       paste0( "writeLines( \"", test_output_value, "\", con = \"", test_output_file, "\")") ),
                    con = test_program )
  
  
  test_program_ref <- base::substring( test_program, base::nchar( test_workarea ) + 2 )
  
  
  # - test log
  
  test_log_parent <- "path/to/logs"
  
  if ( dir.exists( file.path( test_workarea, test_log_parent, fsep = "/") ) || 
       ! dir.create( file.path( test_workarea, test_log_parent, fsep = "/"), recursive = TRUE ) )
    testthat::fail( "Could not stage test log directory" )
  
  test_log <- file.path( test_log_parent, 
                         paste0( tools::file_path_sans_ext(base::basename(test_program_ref)), ".log"), fsep = "/" )
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(),
                       "type" = "program", 
                       "path" = test_program_ref, 
                       "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                       "log" = list( "path" = test_log ) )
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  result <- cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts )

  
  # -- expected
  
  # - start with action
  expected_results <- test_action
  
  # - add log 
  expected_results[["log"]][["sha1"]] <- digest::digest( file.path( test_workarea, test_log, fsep = "/"), algo = "sha1", file = TRUE )

  
  # - expected inputs
  
  input_files <- character(0)
  input_files[ test_action[[ "path" ]] ] <- test_action[[ "sha1" ]]   # add program
  input_files[ test_output_file ] <- test_output_ref_sha1
  
  expected_inputs <- lapply( sort(names(input_files)), function(x) {
    list( "path" = x, "sha1" = unname(input_files[x]) )
  } )
  
    
  # - expected output
  expected_output <- list( "path" = test_output_file, 
                           "sha1" = digest::digest( file.path( test_workarea, test_output_file, fsep = "/" ), algo = "sha1", file = TRUE ) )
  
  
  # - add inputs
  expected_results <- append( expected_results, 
                              list( "files.input" = expected_inputs, 
                                    "files.created" = list( expected_results[["log"]] ),
                                    "files.updated" = list( expected_output ), 
                                    "files.deleted" = list() )  
  )
  

  # -- assertions
  
  # - results
  testthat::expect_equal( result[ names(expected_results) ], expected_results[ names(expected_results) ] )
  
  # - program
  testthat::expect_true( file.exists( file.path( test_workarea, expected_results[["path"]], fsep = "/" ) ) )
  testthat::expect_equal( digest::digest( file.path( test_workarea, expected_results[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ),
                          expected_results[["sha1"]] )
  
  # - log
  testthat::expect_true( file.exists( file.path( test_workarea, expected_results[["log"]][["path"]], fsep = "/" ) ) )
  testthat::expect_equal( digest::digest( file.path( test_workarea, expected_results[["log"]][["path"]], fsep = "/" ), algo = "sha1", file = TRUE ),
                          expected_results[["log"]][["sha1"]] )
  
})





testthat::test_that( "batchjob.execdSingleFileDelete", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  # - work area  
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-area-", tmpdir = test_root, fileext = "") )
  
  if ( dir.exists( test_workarea ) || ! dir.create( test_workarea, recursive = TRUE ) )
    testthat::fail( "Cannot stage test work area" )
  
  
  # - test output to create
  
  test_output_parent <- "path/to/outputs"
  
  if ( dir.exists( file.path( test_workarea, test_output_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_output_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test output parent directory" )
  
  
  test_output_file <- file.path( test_output_parent, 
                                 base::basename( base::tempfile( pattern = "test-output-", 
                                                                 tmpdir = file.path( test_workarea, test_output_parent, fsep = "/" ), 
                                                                 fileext = ".txt") ),
                                 fsep = "/" )
  
  base::writeLines( c( "# test update existing output", 
                       paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 30 ), collapse = "") ), 
                    con = file.path( test_workarea, test_output_file, fsep = "/" ) )
  
  
  if ( ! file.exists(file.path( test_workarea, test_output_file, fsep = "/" )) )
    testthat::fail( "Unexpected output file does not exist" )
  
  
  test_output_value <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 30 ), collapse = "")
  
  test_output_ref_sha1 <- digest::digest( file.path( test_workarea, test_output_file, fsep = "/" ), algo = "sha1", file = TRUE )
  
  
  # - test program
  
  test_program_parent <- "path/to/programs"
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                             fileext = ".R") )
  
  base::writeLines( c( "# test program", 
                       paste0( "file.remove( \"", test_output_file, "\" )") ),
                    con = test_program )
  
  
  test_program_ref <- base::substring( test_program, base::nchar( test_workarea ) + 2 )
  
  
  # - test log
  
  test_log_parent <- "path/to/logs"
  
  if ( dir.exists( file.path( test_workarea, test_log_parent, fsep = "/") ) || 
       ! dir.create( file.path( test_workarea, test_log_parent, fsep = "/"), recursive = TRUE ) )
    testthat::fail( "Could not stage test log directory" )
  
  test_log <- file.path( test_log_parent, 
                         paste0( tools::file_path_sans_ext(base::basename(test_program_ref)), ".log"), fsep = "/" )
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(),
                       "type" = "program", 
                       "path" = test_program_ref, 
                       "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                       "log" = list( "path" = test_log ) )
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  result <- cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts )

  
  # -- expected
  
  # - start with action
  expected_results <- test_action
  
  # - add log 
  expected_results[["log"]][["sha1"]] <- digest::digest( file.path( test_workarea, test_log, fsep = "/"), algo = "sha1", file = TRUE )
  
  
  # - expected inputs
  
  input_files <- character(0)
  input_files[ test_action[[ "path" ]] ] <- test_action[[ "sha1" ]]   # add program
  input_files[ test_output_file ] <- test_output_ref_sha1
  
  expected_inputs <- lapply( sort(names(input_files)), function(x) {
    list( "path" = x, "sha1" = unname(input_files[x]) )
  } )
  
  
  # - expected deleted
  expected_deleted <- list( "path" = test_output_file, 
                            "sha1" = test_output_ref_sha1 )
  
  
  # - add inputs
  expected_results <- append( expected_results, 
                              list( "files.input" = expected_inputs, 
                                    "files.created" = list( expected_results[["log"]] ),
                                    "files.updated" = list(), 
                                    "files.deleted" = list( expected_deleted ) )  
  )
  
  
  # -- assertions
  
  # - results
  testthat::expect_equal( result[ names(expected_results) ], expected_results[ names(expected_results) ] )
  
  # - program
  testthat::expect_true( file.exists( file.path( test_workarea, expected_results[["path"]], fsep = "/" ) ) )
  testthat::expect_equal( digest::digest( file.path( test_workarea, expected_results[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ),
                          expected_results[["sha1"]] )
  
  # - log
  testthat::expect_true( file.exists( file.path( test_workarea, expected_results[["log"]][["path"]], fsep = "/" ) ) )
  testthat::expect_equal( digest::digest( file.path( test_workarea, expected_results[["log"]][["path"]], fsep = "/" ), algo = "sha1", file = TRUE ),
                          expected_results[["log"]][["sha1"]] )
  
})
