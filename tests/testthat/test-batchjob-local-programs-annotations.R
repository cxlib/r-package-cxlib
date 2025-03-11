#
#
#  Tests for cxlib::cxlib_batchjob()
#
#  Local programs
#
#  Input and output annotations
#



testthat::test_that( "batchjob.localProgramAnnoInputFiles", {
  
  
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
  
  
  
  # - test inputs
  
  
  test_input_parent <- file.path( "path", "to", "inputs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_input_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_input_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test inputs parent directory" )
  
  
  test_inputs <- replicate( 5, 
                            cxlib::cxlib_standardpath( base::tempfile( pattern = "test-input-", 
                                                                       tmpdir = file.path( test_wd, test_input_parent, fsep = "/"), 
                                                                       fileext = ".txt") ), 
                            simplify = TRUE ) 
  
  for ( xinput in test_inputs ) {
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9), 25 ) ), collapse = ""), 
                      con = xinput )
   
    if ( ! file.exists( xinput ) )
      testthat::fail( "Could not stage inputs" )
  }
  
  
  test_input_refs <- base::substring( test_inputs, base::nchar(test_wd) + 2 ) 
  

  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_programs <- replicate( 5, 
                              file.path( test_program_parent, 
                                         base::basename( base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) ),
                                         fsep = "/" ),
                              simplify = TRUE )
  
  for ( xpgm in test_programs )
    base::writeLines( c( paste( "# Test program", xpgm ),
                         paste( "# @cx.input", test_input_refs[ match( xpgm, test_programs) ] ) ), 
                      con = file.path( test_wd, xpgm, fsep = "/") )
  
  
  if ( ! all(file.exists( file.path( test_wd, test_programs, fsep = "/" ) )) )
    testthat::fail( "Unexpected test progrom exists" )
  


  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )

  
  # -- test
  
  result <- cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_programs ) )
  

  
  # -- expected

  # - job id
  expected_id <- test_id
    
  
  # - programs
  expected_programs <- test_programs

    
  # - logs
  expected_logs <- paste0( tools::file_path_sans_ext( test_programs ), ".Rout" ) 
  
  
  # - inputs
  expected_inputs <- file.path( test_jobpath, expected_id, ".work", test_input_refs )
  
  

  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )
  
  # - staged inputs
  testthat::expect_true( all(file.exists( expected_inputs ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, expected_id, ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {
    
    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )
    
    testthat::expect_equal( json_data[["type"]], "program" )
    
    testthat::expect_equal( json_data[["path"]], test_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]], 
                            digest::digest( file.path( test_jobpath, expected_id, ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] ) 
    
    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )
    
  }
  
  
})





testthat::test_that( "batchjob.localProgramAnnoInputDir", {
  
  
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
  
  
  
  # - test inputs
  
  
  test_input_parent <- file.path( "path", "to", "inputs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_input_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_input_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test inputs parent directory" )
  
  
  test_inputs <- replicate( 5, 
                            cxlib::cxlib_standardpath( base::tempfile( pattern = "test-input-", 
                                                                       tmpdir = file.path( test_wd, test_input_parent, fsep = "/"), 
                                                                       fileext = ".txt") ), 
                            simplify = TRUE ) 
  
  for ( xinput in test_inputs ) {
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9), 25 ) ), collapse = ""), 
                      con = xinput )
    
    if ( ! file.exists( xinput ) )
      testthat::fail( "Could not stage inputs" )
  }
  
  
  test_input_refs <- base::substring( test_inputs, base::nchar(test_wd) + 2 ) 
  
  
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_programs <- replicate( 5, 
                              file.path( test_program_parent, 
                                         base::basename( base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) ),
                                         fsep = "/" ),
                              simplify = TRUE )
  
  for ( xpgm in test_programs )
    base::writeLines( c( paste( "# Test program", xpgm ),
                         paste( "# @cx.input", test_input_parent  ) ), 
                      con = file.path( test_wd, xpgm, fsep = "/") )
  
  
  if ( ! all(file.exists( file.path( test_wd, test_programs, fsep = "/" ) )) )
    testthat::fail( "Unexpected test progrom exists" )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # -- test
  
  result <- cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_programs ) )
  
  
  
  # -- expected
  
  # - job id
  expected_id <- test_id
  
  
  # - programs
  expected_programs <- test_programs
  
  
  # - logs
  expected_logs <- paste0( tools::file_path_sans_ext( test_programs ), ".Rout" ) 
  
  
  # - inputs
  expected_inputs <- file.path( test_jobpath, expected_id, ".work", test_input_refs )
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )
  
  # - staged inputs
  testthat::expect_true( all(file.exists( expected_inputs ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, expected_id, ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {
    
    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )
    
    testthat::expect_equal( json_data[["type"]], "program" )
    
    testthat::expect_equal( json_data[["path"]], test_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]], 
                            digest::digest( file.path( test_jobpath, expected_id, ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] ) 
    
    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )
    
  }
  
  
})






testthat::test_that( "batchjob.localProgramAnnoOutputDir", {
  
  
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


  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )

  
  
  # - test outputs

  test_output_parent <- file.path( "path", "to", "outputs", fsep = "/" )
  

  test_output_dirs <- replicate( 5, 
                                 cxlib::cxlib_standardpath( base::tempfile( pattern = "test-output-", 
                                                                            tmpdir = file.path( test_jobpath, test_id, ".work", test_output_parent, fsep = "/"), 
                                                                            fileext = "") ), 
                                 simplify = TRUE ) 


  if ( any(dir.exists( test_output_dirs ) ) )
    testthat::fail( "Unexpected test output directory exists" )
       
    
  test_output_refs <- base::substring( test_output_dirs, base::nchar(file.path( test_jobpath, test_id, ".work", fsep = "/")) + 2 ) 

  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_programs <- replicate( 5, 
                              file.path( test_program_parent, 
                                         base::basename( base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) ),
                                         fsep = "/" ),
                              simplify = TRUE )
  
  for ( xpgm in test_programs )
    base::writeLines( c( paste( "# Test program", xpgm ),
                         paste( "# @cx.output", test_output_refs[ match( xpgm, test_programs) ] ) ), 
                      con = file.path( test_wd, xpgm, fsep = "/") )
  
  
  if ( ! all(file.exists( file.path( test_wd, test_programs, fsep = "/" ) )) )
    testthat::fail( "Unexpected test progrom exists" )
  
  
  
  
  
  # -- test
  
  result <- cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_programs ) )
  

  
  # -- expected
  
  # - job id
  expected_id <- test_id
  
  
  # - programs
  expected_programs <- test_programs
  
  
  # - logs
  expected_logs <- paste0( tools::file_path_sans_ext( test_programs ), ".Rout" ) 
  
  
  # - outputs
  expected_outputs <- file.path( test_jobpath, expected_id, ".work", test_output_refs )
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )
  
  # - staged outputs
  testthat::expect_true( all(dir.exists( expected_outputs ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, expected_id, ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {
    
    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )
    
    testthat::expect_equal( json_data[["type"]], "program" )
    
    testthat::expect_equal( json_data[["path"]], test_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]], 
                            digest::digest( file.path( test_jobpath, expected_id, ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] ) 
    
    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )
    
  }
  
  
})

