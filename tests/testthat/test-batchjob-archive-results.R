#
#
#  Tests for cxlib::cxlib_batchjob()
#
#  Results archive
#
#




testthat::test_that( "batchjob.resultsArchiveProgramsAnnoInputsOutputs", {
  
  
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
  
  
  # - cxlib properties
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath )),
                    con = file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) ) )  
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # - test working directory
  
  test_wd <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_wd ) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  current_wd <- base::getwd()
  
  on.exit({
    base::setwd( current_wd )
  }, add = TRUE )
  
  base::setwd( test_wd )
  
  
  
  # - test references
  
  test_references <- replicate( 5, 
                                paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 25 ), collapse = "" ),
                                simplify = TRUE )
  
  
  
  # - test inputs
  
  test_input_parent <- file.path( "path", "to", "inputs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_input_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_input_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test input parent directory" )
  
  test_inputs <- replicate( 5,
                            base::tempfile( pattern = "test-input-", tmpdir = file.path( test_wd, test_input_parent, fsep = "/"), fileext = ".txt" ),
                            simplify = TRUE )
  
  
  for ( xinput in test_inputs )
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 35 ) , collapse = ""), 
                      con = xinput )
  
  
  test_input_refs <- base::substring( test_inputs, base::nchar( test_wd ) + 2 )
  
  
  
  # - test outputs
  
  test_output_parent <- file.path( "path", "to", "outputs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_output_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_output_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test output parent directory" )
  
  
  test_outputs <- replicate( 5,
                             base::tempfile( pattern = "test-output-", tmpdir = file.path( test_wd, test_output_parent, fsep = "/"), fileext = ".txt" ),
                             simplify = TRUE )
  
  # generate some of the outputs
  for ( xout in utils::head( test_outputs, n = 3 ) )
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 35 ) , collapse = ""), 
                      con = xout )
  
  
  
  test_output_refs <- base::substring( test_outputs, base::nchar( test_wd ) + 2 )
  
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_programs <- replicate( 5, 
                              base::tempfile( pattern = "test-program-", 
                                              tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                              fileext = ".R" ),
                              simplify = TRUE )
  
  
  
  test_program_annotations <- list()
  
  
  for ( xpgm in test_programs ) {
    
    test_program_annotations[[ xpgm ]] <- list( "input" = test_input_refs[ match( xpgm, test_programs) ], 
                                                "output" = test_output_refs[ match( xpgm, test_programs) ] )
    
    pgm_lines <- c( paste( "# Empty test program", xpgm ),
                    paste( "# @cx.input", test_program_annotations[[xpgm]][["input"]] ), 
                    paste( "# @cx.input", test_output_parent ),
                    paste( "# @cx.output", test_output_parent ), 
                    paste0( "cat( \"", test_references[ match( xpgm, test_programs) ], "\", file = \"", test_program_annotations[[ xpgm ]][["output"]], "\" )" ) )
    
    
    # cat( c( "-----------", pgm_lines, "-----------"), sep = "\n" )
    
    base::writeLines( pgm_lines, con = xpgm )
    
  }
  
  
  if ( ! all(file.exists( test_programs) ) )
    testthat::fail( "Test programs could not be staged" )
  
  
  test_program_refs <- base::substring( test_programs, base::nchar( test_wd ) + 2 )
  
  names(test_program_annotations) <- test_program_refs
  
  
  
  # - test archive 
  
  test_archive <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip") )
  

  # - job ID
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  

  # - test job
  
  test_job <- cxlib::cxlib_batchjob( list( "id" = test_id, 
                                           "programs" = test_program_refs ) )
  
  test_job$submit( wait = TRUE )
  

  
  # -- test
  
  result <- test_job$archive( test_archive )
  
  
  
  # -- expected
  
  # - test id
  expected_id <- test_id
  
  
  # - logs 
  expected_logs <- paste0( tools::file_path_sans_ext( test_program_refs ), ".Rout" )
  

  # - expected outputs
  expected_outputs <- test_output_refs
  
  
  # - expected archive
  expected_archive <- test_archive
  expected_archive_files <- append( expected_logs, expected_outputs )
  
  
  # - expected sha1
  expected_sha1 <- sapply( expected_archive_files, function(x) {
    digest::digest( file.path( test_jobpath, expected_id, ".work", x, fsep = "/" ), alg = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # -- assertions
  
  # - archive
  testthat::expect_equal( result, expected_archive )
  testthat::expect_true( file.exists(expected_archive) )
  
  
  # - archive contents
  
  result_archive_ext <- cxlib::cxlib_standardpath( base::tempfile( pattern = "results-arcive-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( result_archive_ext ) || ! dir.create( result_archive_ext, recursive = TRUE ) )
    testthat:fail( "Could not create extract area for results archive assertions" )
  
  # extract archive
  zip::unzip( result, exdir = result_archive_ext )
  
  
  # verify extracted files exist
  testthat::expect_true( all( file.exists( file.path( result_archive_ext, expected_archive_files ) ) ) )
  
  
  # verify extracted file integrity
  
  result_archive_sha1 <- sapply( expected_archive_files, function(x) {
    digest::digest( file.path( result_archive_ext, x, fsep = "/" ), alg = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )

  testthat::expect_equal( result_archive_sha1[ sort(expected_archive_files) ], expected_sha1[ sort(expected_archive_files) ] )

  
})


