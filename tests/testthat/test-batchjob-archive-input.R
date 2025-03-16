#
#
#  Tests for cxlib::cxlib_batchjob()
#
#  Input archive
#
#



testthat::test_that( "batchjob.inputArchivePrograms", {
  
  
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
    base::writeLines( paste( "# Empty test program", xpgm ), 
                      con = file.path( test_wd, xpgm, fsep = "/") )
  
  
  if ( ! all(file.exists( file.path( test_wd, test_programs, fsep = "/" ) )) )
    testthat::fail( "Unexpected test program exists" )
  
  
  # - generate archive 
  
  test_archive <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip") )
  
  zip::zip( test_archive, test_programs )



  # - job ID
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  

  
  # -- test

  result <- cxlib::cxlib_batchjob( list( "id" = test_id, 
                                         "programs" = test_programs,
                                         "archive" = test_archive ) )



  # -- expected


  # - job id

  expected_id <- test_id


  # - programs

  expected_programs <- test_programs



  # - logs

  expected_logs <- paste0( tools::file_path_sans_ext( test_programs ), ".Rout" )



  # -- assertions


  # - staged programs

  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )


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





testthat::test_that( "batchjob.inputArchiveAddPrograms", {
  
  
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
    base::writeLines( paste( "# Empty test program", xpgm ), 
                      con = file.path( test_wd, xpgm, fsep = "/") )
  
  
  if ( ! all(file.exists( file.path( test_wd, test_programs, fsep = "/" ) )) )
    testthat::fail( "Unexpected test program exists" )
  
  
  # - generate archive 
  
  test_archive <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip") )
  
  zip::zip( test_archive, test_programs )
  
  
  
  # - job ID
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job
  
  test_job <- cxlib::cxlib_batchjob( list( "id" = test_id, 
                                           "programs" = utils::head( test_programs, n = 2 ) ) ) 
  
  
  # -- test
  
  test_job$add( list( "programs" = utils::tail( test_programs, n = 3 ),
                      "archive" = test_archive ) )
  
  
  
  # -- expected
  
  
  # - job id
  
  expected_id <- test_id
  
  
  # - programs
  
  expected_programs <- test_programs
  
  
  
  # - logs
  
  expected_logs <- paste0( tools::file_path_sans_ext( test_programs ), ".Rout" )
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )
  
  
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





testthat::test_that( "batchjob.inputArchiveProgramsAnnoInputsOutputs", {
  

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
  
  
  
  # - test archive source
  
  test_archive_src <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-archive-source-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_archive_src ) || ! dir.create( test_archive_src, recursive = TRUE ) )
    testthat::fail( "Could not stage test archive source directory" )
  
  
  
  
  # - test inputs
  
  test_input_parent <- file.path( "path", "to", "inputs", fsep = "/" )
  
  if ( dir.exists( file.path( test_archive_src, test_input_parent, fsep = "/" ) ) || ! dir.create( file.path( test_archive_src, test_input_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test input parent directory" )
  
  test_inputs <- replicate( 5,
                            base::tempfile( pattern = "test-input-", tmpdir = file.path( test_archive_src, test_input_parent, fsep = "/"), fileext = ".txt" ),
                            simplify = TRUE )
  
  
  for ( xinput in test_inputs )
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 35 ) , collapse = ""), 
                      con = xinput )
  
  
  test_input_refs <- base::substring( test_inputs, base::nchar( test_archive_src ) + 2 )
  

  
  # - test outputs
  
  test_output_parent <- file.path( "path", "to", fsep = "/" )
  
  test_outputs <- replicate( 5,
                             base::tempfile( pattern = "test-output-", tmpdir = file.path( test_archive_src, test_output_parent, fsep = "/"), fileext = "" ),
                             simplify = TRUE )

  test_output_refs <- base::substring( test_outputs, base::nchar( test_archive_src ) + 2 )
  
    

  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_archive_src, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_archive_src, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_programs <- replicate( 5, 
                              base::tempfile( pattern = "test-program-", 
                                              tmpdir = file.path( test_archive_src, test_program_parent, fsep = "/"), 
                                              fileext = ".R" ),
                              simplify = TRUE )
  
  
  
  test_program_annotations <- list()
  
  
  for ( xpgm in test_programs ) {
    
    test_program_annotations[[ xpgm ]] <- list( "input" = test_input_refs[ match( xpgm, test_programs) ], 
                                                "output" = test_output_refs[ match( xpgm, test_programs) ] )
    
    pgm_lines <- c( paste( "# Empty test program", xpgm ),
                    paste( "# @cx.input", test_program_annotations[[xpgm]][["input"]] ), 
                    paste( "# @cx.output", test_program_annotations[[xpgm]][["output"]]  ) )
    
    # cat( c( "-----------", pgm_lines, "-----------"), sep = "\n" )
    
    base::writeLines( pgm_lines, con = xpgm )
  
  }
  
  
  if ( ! all(file.exists( test_programs) ) )
    testthat::fail( "Test programs could not be staged" )
  
  
  test_program_refs <- base::substring( test_programs, base::nchar( test_archive_src ) + 2 )
  
  names(test_program_annotations) <- test_program_refs
  
  
  
  # - generate archive 
  
  test_archive <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip") )
  
  zip::zip( test_archive, c( test_program_refs, test_input_refs ), root = test_archive_src )
  

  # - job ID
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  
  # -- test
  
  result <- cxlib::cxlib_batchjob( list( "id" = test_id, 
                                         "programs" = test_program_refs,
                                         "archive" = test_archive ) )
  
  
  
  # -- expected
  
  
  # - job id
  
  expected_id <- test_id
  
  
  # - programs
  
  expected_programs <- test_program_refs
  
  
  
  # - logs
  
  expected_logs <- paste0( tools::file_path_sans_ext( test_program_refs ), ".Rout" )
  

  # - annotations
  expected_annotations <- test_program_annotations
  

    
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, expected_id, ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, expected_id, ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {
    
    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )

    # it is a program
    testthat::expect_equal( json_data[["type"]], "program" )
    
    # sha1 on program
    testthat::expect_equal( json_data[["path"]], expected_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]],
                            digest::digest( file.path( test_jobpath, expected_id, ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    # log
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] )
    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )

    # annotations    
    testthat::expect_equal( json_data[["annotations"]], expected_annotations[[ json_data[["path"]] ]])
    
  }
  
  
})


