#
#
#  Tests for cxlib::cxlib_batchjob()
#
#  Save local file system
#
#


testthat::test_that( "batchjob.saveMultiProgram", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )

  
    
  # - test job path
  
  test_jobpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-jobpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_jobpath ) || ! dir.create( test_jobpath, recursive = TRUE ) )
    testthat::fail( "Could not stage test job directory" )

  
  
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
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ) ),
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
  

  # - execute actions
  test_job$submit( wait = TRUE )
  
    
  # -- test

  test_job$save()

  
  
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

    
  # - log in work area
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_logs, fsep = "/" ) ) ) )
  

  # - log in working directory
  
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_logs, fsep = "/" ) ) ) )
  
  
  # - integrity of log in working directory
  
  for ( xlog in expected_logs ) 
    testthat::expect_equal( digest::digest( file.path( test_jobpath, expected_id, ".work", xlog, fsep = "/" ), algo = "sha1", file = TRUE ), 
                            digest::digest( file.path( test_wd, xlog, fsep = "/" ), algo = "sha1", file = TRUE ) )
  
    
  # - ensure program actions executed
  
  for ( xlog in expected_logs ) {
    
    result_log <- readLines( file.path( test_wd, xlog, fsep = "/" ) )
    testthat::expect_true( any(grepl( paste0( "^", expected_references[ match( xlog, expected_logs ) ], "$" ), result_log )) )
    
    base::rm( list = "result_log" )
  }
  
  
})




testthat::test_that( "batchjob.saveMultiProgramCreatedOutputs", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  
  # - test job path
  
  test_jobpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-jobpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_jobpath ) || ! dir.create( test_jobpath, recursive = TRUE ) )
    testthat::fail( "Could not stage test job directory" )
  
  
  
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
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ) ),
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
  
  
  # - test outputs
  
  if ( dir.exists( file.path( test_wd, "path", "to", fsep = "/" ) ) || ! dir.create( file.path( test_wd, "path", "to", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test standard paths" )
  
  test_output_dirs <- replicate( 2, 
                                 cxlib::cxlib_standardpath( base::tempfile( pattern = "outputs-", 
                                                                            tmpdir = file.path( test_wd, "path", "to", fsep = "/" ),
                                                                            fileext = "" ) ), 
                                 simplify = TRUE )
  
  for ( xpath in test_output_dirs )
    if ( dir.exists(xpath) || ! dir.create( xpath, recursive = TRUE ) )
      testthat::fail( "Could not stage output directory" )
  
  
  test_outputs <- character(0)
  
  for ( xout in test_output_dirs )
    test_outputs[ match( xout, test_output_dirs ) ] <- file.path( xout, 
                                                                  paste0( "output-", match( xout, test_output_dirs ), ".txt") )
  
  
  test_output_refs <- base::substring( test_outputs, base::nchar(test_wd) + 2 )
  


  # - test reference
  
  test_references <- replicate( 2, 
                                paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 20 ), collapse = "" ), 
                                simplify = TRUE )
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_programs <- replicate( 2, 
                              cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                                         tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                                         fileext = ".R" ) ), 
                              simplify = TRUE )
  
  xout_directives <- c( paste( "# @cx.output", base::dirname( utils::head( test_output_refs, n = 1) ) ), 
                        "# no outputs" )
  
  
  for ( xpgm in test_programs ) {

    xout_file <- test_output_refs[ match( xpgm, test_programs ) ]
    
    pgm_lines <- c( "# Test program",
                    xout_directives[ match( xpgm, test_programs ) ],
                    paste0("base::writeLines( \"", test_references[ match( xpgm, test_programs ) ], "\", con = \"", xout_file, "\" )" ) )

    base::writeLines( pgm_lines, con = xpgm )
    
    
    if ( ! file.exists( xpgm ) )
      testthat::fail( "Unexpected test program exists" )
    
  }
  
  
  test_program_refs <- base::substring( test_programs, base::nchar(test_wd) + 2 )
  
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_refs ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  

  # -- test

  test_job$save()



  # -- expected

  # - job id
  expected_id <- test_id

  # - references
  expected_references <- utils::head( test_references, n = 1 )

  # - programs
  expected_programs <- test_program_refs


  # - logs
  expected_logs <- paste0( tools::file_path_sans_ext( expected_programs ), ".Rout" )

  # - output 
  
  expected_outputs <- file.path( test_wd, utils::head( test_output_refs, n = 1), fsep = "/" )

  

  # -- assertions


  # - staged programs

  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )


  # - log in work area

  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_logs, fsep = "/" ) ) ) )


  # - log in working directory

  testthat::expect_true( all( file.exists( file.path( test_wd, expected_logs, fsep = "/" ) ) ) )


  # - integrity of log in working directory

  for ( xlog in expected_logs )
    testthat::expect_equal( digest::digest( file.path( test_jobpath, expected_id, ".work", xlog, fsep = "/" ), algo = "sha1", file = TRUE ),
                            digest::digest( file.path( test_wd, xlog, fsep = "/" ), algo = "sha1", file = TRUE ) )
  
  
  # - test output
  
  testthat::expect_true( all( file.exists( expected_outputs ) ) )

  
  # - test output content  
  result_out <- base::readLines( expected_outputs )
  testthat::expect_true( any(grepl( paste0( "^", expected_references, "$" ), result_out)) )

})




testthat::test_that( "batchjob.saveMultiProgramUpdatedOutputs", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  
  # - test job path
  
  test_jobpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-jobpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_jobpath ) || ! dir.create( test_jobpath, recursive = TRUE ) )
    testthat::fail( "Could not stage test job directory" )
  
  
  
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
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ) ),
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
  
  
  # - test outputs
  
  if ( dir.exists( file.path( test_wd, "path", "to", fsep = "/" ) ) || ! dir.create( file.path( test_wd, "path", "to", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test standard paths" )
  
  test_output_dirs <- replicate( 2, 
                                 cxlib::cxlib_standardpath( base::tempfile( pattern = "outputs-", 
                                                                            tmpdir = file.path( test_wd, "path", "to", fsep = "/" ),
                                                                            fileext = "" ) ), 
                                 simplify = TRUE )
  
  for ( xpath in test_output_dirs )
    if ( dir.exists(xpath) || ! dir.create( xpath, recursive = TRUE ) )
      testthat::fail( "Could not stage output directory" )
  
  
  test_outputs <- character(0)
  
  for ( xout in test_output_dirs )
    test_outputs[ match( xout, test_output_dirs ) ] <- file.path( xout, 
                                                                  paste0( "output-", match( xout, test_output_dirs ), ".txt") )
  

  test_output_refs <- base::substring( test_outputs, base::nchar(test_wd) + 2 )

  
  
  # - stage outputs for update
  
  for ( xout in test_output_refs )
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 25), collapse = ""), 
                      con = xout )
  

  
  
  # - test reference
  
  test_references <- replicate( 2, 
                                paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 20 ), collapse = "" ), 
                                simplify = TRUE )
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_programs <- replicate( 2, 
                              cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                                         tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                                         fileext = ".R" ) ), 
                              simplify = TRUE )
  
  xout_directives <- c( paste( "# @cx.output", base::dirname( utils::head( test_output_refs, n = 1) ) ), 
                        "# no outputs" )
  
  
  for ( xpgm in test_programs ) {
    
    xout_file <- test_output_refs[ match( xpgm, test_programs ) ]
    
    pgm_lines <- c( "# Test program",
                    paste( "# @cx.input", xout_file ),
                    xout_directives[ match( xpgm, test_programs ) ],
                    paste0("base::writeLines( \"", test_references[ match( xpgm, test_programs ) ], "\", con = \"", xout_file, "\" )" ) )
    
    # cat( c( "--------------", pgm_lines , "--------------"), sep = "\n")    

    base::writeLines( pgm_lines, con = xpgm )
    
    if ( ! file.exists( xpgm ) )
      testthat::fail( "Unexpected test program exists" )
    
  }
  
  
  test_program_refs <- base::substring( test_programs, base::nchar(test_wd) + 2 )
  

  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_refs ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  

  # -- test
  
  test_job$save()
  
  

  # -- expected
  

  # - job id
  expected_id <- test_id
  
  # - references
  expected_references <- utils::head( test_references, n = 1 )
  
  # - programs
  expected_programs <- test_program_refs
  
  
  # - logs
  expected_logs <- paste0( tools::file_path_sans_ext( expected_programs ), ".Rout" )

    
  # - updated outputs

  expected_outputs <- utils::head( test_output_refs, n = 1)

  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - log in work area
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_logs, fsep = "/" ) ) ) )
  
  
  # - log in working directory
  
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_logs, fsep = "/" ) ) ) )
  
  
  # - integrity of log in working directory
  
  for ( xlog in expected_logs )
    testthat::expect_equal( digest::digest( file.path( test_jobpath, expected_id, ".work", xlog, fsep = "/" ), algo = "sha1", file = TRUE ),
                            digest::digest( file.path( test_wd, xlog, fsep = "/" ), algo = "sha1", file = TRUE ) )
  
  
  # - updated test output
  
  testthat::expect_true( all( file.exists( expected_outputs ) ) )

  
  postinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )

  common_files <- sort(intersect( names(test_preinv), names(postinv) ))

  testthat::expect_true( all( common_files[ postinv[common_files] != test_preinv[common_files] ] %in% expected_outputs ) )
  
    
  
  # - test output content  
  
  result_out <- base::readLines( expected_outputs )
  testthat::expect_true( any(grepl( paste0( "^", expected_references, "$" ), result_out)) )
  
})




testthat::test_that( "batchjob.saveMultiProgramDeleteOutputs", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  
  # - test job path
  
  test_jobpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-jobpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_jobpath ) || ! dir.create( test_jobpath, recursive = TRUE ) )
    testthat::fail( "Could not stage test job directory" )
  
  
  
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
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ) ),
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
  
  
  # - test outputs
  
  if ( dir.exists( file.path( test_wd, "path", "to", fsep = "/" ) ) || ! dir.create( file.path( test_wd, "path", "to", fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test standard paths" )
  
  test_output_dirs <- replicate( 2, 
                                 cxlib::cxlib_standardpath( base::tempfile( pattern = "outputs-", 
                                                                            tmpdir = file.path( test_wd, "path", "to", fsep = "/" ),
                                                                            fileext = "" ) ), 
                                 simplify = TRUE )
  
  for ( xpath in test_output_dirs )
    if ( dir.exists(xpath) || ! dir.create( xpath, recursive = TRUE ) )
      testthat::fail( "Could not stage output directory" )
  
  
  test_outputs <- character(0)
  
  for ( xout in test_output_dirs )
    test_outputs[ match( xout, test_output_dirs ) ] <- file.path( xout, 
                                                                  paste0( "output-", match( xout, test_output_dirs ), ".txt") )
  
  
  test_output_refs <- base::substring( test_outputs, base::nchar(test_wd) + 2 )
  
  
  
  # - stage outputs for update
  
  for ( xout in test_output_refs )
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 25), collapse = ""), 
                      con = xout )
  
  
  
  
  # - test reference
  
  test_references <- replicate( 2, 
                                paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 20 ), collapse = "" ), 
                                simplify = TRUE )
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_programs <- replicate( 2, 
                              cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                                         tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                                         fileext = ".R" ) ), 
                              simplify = TRUE )
  
  xout_directives <- c( paste( "# @cx.output", base::dirname( utils::head( test_output_refs, n = 1) ) ), 
                        "# no outputs" )
  
  
  for ( xpgm in test_programs ) {
    
    xout_file <- test_output_refs[ match( xpgm, test_programs ) ]
    
    pgm_lines <- c( "# Test program",
                    paste( "# @cx.input", xout_file ),
                    xout_directives[ match( xpgm, test_programs ) ],
                    paste0("file.remove( \"", xout_file, "\" )" ) )
    
    # cat( c( "--------------", pgm_lines , "--------------"), sep = "\n")    

    base::writeLines( pgm_lines, con = xpgm )
    
    if ( ! file.exists( xpgm ) )
      testthat::fail( "Unexpected test program exists" )
          
  }
  

  test_program_refs <- base::substring( test_programs, base::nchar(test_wd) + 2 )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_refs ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  
  
  # -- test
  
  test_job$save()
  
  

  # -- expected
  
  
  # - job id
  expected_id <- test_id
  
  # - references
  expected_references <- utils::head( test_references, n = 1 )
  
  # - programs
  expected_programs <- test_program_refs
  
  
  # - logs
  expected_logs <- paste0( tools::file_path_sans_ext( expected_programs ), ".Rout" )
  
  
  # - updated outputs
  
  expected_deleted <- utils::head( test_output_refs, n = 1)
  expected_outputs <- test_output_refs[ ! test_output_refs %in% expected_deleted ]
  
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - log in work area
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_logs, fsep = "/" ) ) ) )
  
  
  # - log in working directory
  
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_logs, fsep = "/" ) ) ) )
  
  
  # - integrity of log in working directory
  
  for ( xlog in expected_logs )
    testthat::expect_equal( digest::digest( file.path( test_jobpath, expected_id, ".work", xlog, fsep = "/" ), algo = "sha1", file = TRUE ),
                            digest::digest( file.path( test_wd, xlog, fsep = "/" ), algo = "sha1", file = TRUE ) )
  
  
  # - deleted output
  
  testthat::expect_false( all( file.exists( expected_deleted ) ) )
  
  
  # - updated test output
  
  testthat::expect_true( all( file.exists( expected_outputs ) ) )
  
  
  postinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  common_files <- sort(intersect( names(test_preinv), names(postinv) ))
  
  testthat::expect_true( all( common_files[ postinv[common_files] != test_preinv[common_files] ] %in% expected_outputs ) )

})






testthat::test_that( "batchjob.saveMultiProgramDeleteCreateOutput", {
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit( {
    base::unlink( test_root, force = TRUE, recursive = TRUE )
  }, add = TRUE )
  
  if ( dir.exists( test_root ) || ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Cannot stage test area" )
  
  
  
  # - test job path
  
  test_jobpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-jobpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_jobpath ) || ! dir.create( test_jobpath, recursive = TRUE ) )
    testthat::fail( "Could not stage test job directory" )
  
  
  
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
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ) ),
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
  
  
  
  # - test reference
  
  test_references <- replicate( 2, 
                                paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 20 ), collapse = "" ), 
                                simplify = TRUE )  
  
  
  # - test outputs
  
  test_output_parent <- file.path( test_wd, "path", "to", "outputs", fsep = "/" )
  
  if ( dir.exists( test_output_parent ) || ! dir.create( test_output_parent, recursive = TRUE ) )
    testthat::fail( "Could not stage test standard output diretory" )
  

  test_output <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-output-", tmpdir = test_output_parent, fileext = ".txt") )
  
  base::writeLines( test_references[1], con = test_output )

  test_output_ref <- base::substring( test_output, base::nchar( test_wd ) + 2 )

  

  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_programs <- replicate( 2, 
                              cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                                         tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                                         fileext = ".R" ) ), 
                              simplify = TRUE )

  
  # program 1 ... delete
  
  pgm_to_delete <- c( "# Test program that deletes file",
                      paste( "# @cx.input", test_output_ref ),
                      paste( "# @cx.output", base::dirname( test_output_ref ) ), 
                      paste0( "file.remove( \"", test_output_ref, "\" )" ) )

  # cat( c( "--------------", pgm_to_delete , "--------------"), sep = "\n")    
  
  base::writeLines( pgm_to_delete, con = test_programs[1] )
  
  
  
  # program 2 ... create
  
  pgm_to_create <- c( "# Test program that creates file",
                      paste( "# @cx.output", base::dirname( test_output_ref ) ), 
                      paste0( "base::writeLines( \"", test_references[2], "\", con = \"", test_output_ref, "\" )" ) )
  
  
  # cat( c( "--------------", pgm_to_create , "--------------"), sep = "\n")    
  
  base::writeLines( pgm_to_create, con = test_programs[2] )
  

  test_program_refs <- base::substring( test_programs, base::nchar(test_wd) + 2 )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_refs ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  
  
  # -- test
  
  test_job$save()
  
  
  
  # -- expected
  
  
  # - job id
  expected_id <- test_id

    
  # - references
  expected_reference <- test_references[2]
  
  
  # - programs
  expected_programs <- test_program_refs
  
  
  # - logs
  expected_logs <- paste0( tools::file_path_sans_ext( expected_programs ), ".Rout" )
  
  
  # - updated outputs
  
  expected_outputs <- test_output_ref
  
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - log in work area
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_logs, fsep = "/" ) ) ) )
  
  
  # - log in working directory
  
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_logs, fsep = "/" ) ) ) )
  
  
  # - integrity of log in working directory
  
  for ( xlog in expected_logs )
    testthat::expect_equal( digest::digest( file.path( test_jobpath, expected_id, ".work", xlog, fsep = "/" ), algo = "sha1", file = TRUE ),
                            digest::digest( file.path( test_wd, xlog, fsep = "/" ), algo = "sha1", file = TRUE ) )
  
  
  # - updated test output

  testthat::expect_true( all( file.exists( expected_outputs ) ) )
  
  result_output <- readLines( file.path( test_wd, test_output_ref, fsep = "/") )
  
  testthat::expect_false( all(grepl( paste0( "^", test_references[ ! test_references %in% expected_reference ], "$" ), result_output, perl = TRUE )) )
  testthat::expect_true( grepl( paste0( "^", expected_reference, "$" ), result_output, perl = TRUE ) )
  
})

