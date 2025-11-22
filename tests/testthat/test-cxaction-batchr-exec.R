#
#
# Tests for cxaction cx.batch.r and cx.batch
# 
# Program execution
#


#' @cx.testsfor cxlib:::.cxaction_batchr()



testthat::test_that( "cxaction.batchr.execSimpleRProgramDefaults", {
  
  #' @cx.tests Execute R program using CX batch action with no errors or warnings
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - working directory
  
  test_wd <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_wd) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  
  # - program
  
  test_pgm_lines <- c( "# test program", 
                       paste( "print(", base::dQuote( paste( sample( base::letters, sample(5:100,1), replace = TRUE), collapse = "" ), q = FALSE),")" ) )
  
  test_pgm <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( test_wd, "programs", fsep = "/"), fileext = ".R" ) )
  
  if ( dir.exists(base::dirname(test_pgm)) ||
       ! dir.create( base::dirname(test_pgm), recursive = TRUE ) ||
       inherits( try( base::writeLines( test_pgm_lines, con = test_pgm ), silent = FALSE ), "try-error" ) ||
       ! file.exists( test_pgm ) )
    testthat::fail( "Could not stage test program" )
  
  
  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
  
  
  
  # - test SHA-1  
  
  test_sha <- digest::digest( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), sample(100:500,1), replace = TRUE), collapse = "" ), 
                              algo = "sha1", 
                              file = FALSE )
  
  if ( test_sha == test_pgm_sha )
    testthat::fail( "Unexpected test SHA-1 equals the program file SHA-1" )
  
  
  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = "cx.batch", 
                      "attributes" = list( "path" = test_pgm_ref,
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )
  

  # -- expected 
  
  # - identify log
  exp_logpath <- list.files( test_wd, pattern = ".*\\.Rout$", recursive = TRUE, full.names = FALSE, include.dirs = FALSE )

  expected_log <- list( "path" = exp_logpath,
                        "sha" = digest::digest( file.path( test_wd, exp_logpath, fsep = "/" ), algo = "sha1", file = TRUE) ) 

  # - result list
  expected_parms <- test_parms
  
  expected_resultnode <- list( "status" = "completed",
                               "log" = expected_log,
                               "files.input" = list( list( "path" = test_pgm_ref,
                                                           "sha" = test_pgm_sha ) ), 
                               "files.created" = list( list( "path" = expected_log[["path"]],
                                                             "sha" = expected_log[["sha"]]) ),
                               "files.updated" = list(),
                               "files.deleted" = list(),
                               "messages" = "None" )
  
 

  # -- assertions

  # - test parameters copy-forward
  testthat::expect_equal( result[ base::names(test_parms) ], expected_parms )

  # - result details
  testthat::expect_equal( result[["results"]][ base::names(expected_resultnode) ], expected_resultnode[ base::names(expected_resultnode) ] )
  
  
  # - log
  act_loglines <- base::readLines( file.path( test_wd, expected_log[["path"]], fsep = "/") )
  
  # - log errors
  testthat::expect_false( any(grepl( "^error:", act_loglines, ignore.case = TRUE)) )
  
  # - log warnings
  testthat::expect_false( any(grepl( "^warning:", act_loglines, ignore.case = TRUE)) )
  
  
  
})





testthat::test_that( "cxaction.batchr.execRProgramMultiInputsDefaults", {
  
  #' @cx.tests Execute R program using CX batch action with multiple inputs
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - working directory
  
  test_wd <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_wd) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  
  # - inputs
  
  if ( dir.exists( file.path( test_wd, "inputs", fsep = "/") ) || ! dir.create( file.path( test_wd, "inputs", fsep = "/"), recursive = TRUE ) )
    testthat::fail( "Could not stage input file directory" )
  
  test_input_filenames <- replicate( 10, 
                                     cxlib::cxlib_standardpath( base::tempfile( pattern = "test-input-", tmpdir = file.path( test_wd, "inputs", fsep = "/"), fileext = ".txt") ), 
                                     simplify = TRUE )
  
  test_inputs <- base::unlist( sapply( test_input_filenames, function(x) {
    
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), sample( 50:500, 1 ), replace = TRUE) , collapse = ""), 
                      con = x )
    
    digest::digest( x, algo = "sha1", file = TRUE)
    
  }, USE.NAMES = TRUE), use.names = TRUE )
  

  base::names(test_inputs) <- base::substring( base::names(test_inputs), base::nchar(test_wd) + 2 )
  
  
  # - program
  
  test_pgm_lines <- c( "# test program", " " )
  
  for ( xinput in base::names(test_inputs) )
    test_pgm_lines <- append( test_pgm_lines, 
                              c( paste( "if ( ! file.exists(", base::dQuote( xinput, q = FALSE), ") ) {" ), 
                                 paste( "   stop( \"Input file", xinput, "could not be found\" )" ), 
                                 "} else {",
                                 paste( "   message( \"Input file", xinput, "exists\" )" ), 
                                 "}",
                                 " " ) )


  test_pgm <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( test_wd, "programs", fsep = "/"), fileext = ".R" ) )
  
  if ( dir.exists(base::dirname(test_pgm)) ||
       ! dir.create( base::dirname(test_pgm), recursive = TRUE ) ||
       inherits( try( base::writeLines( test_pgm_lines, con = test_pgm ), silent = FALSE ), "try-error" ) ||
       ! file.exists( test_pgm ) )
    testthat::fail( "Could not stage test program" )
  

  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
  
  
  
  # - test SHA-1  
  
  test_sha <- digest::digest( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), sample(100:500,1), replace = TRUE), collapse = "" ), 
                              algo = "sha1", 
                              file = FALSE )
  
  if ( test_sha == test_pgm_sha )
    testthat::fail( "Unexpected test SHA-1 equals the program file SHA-1" )
  
  
  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = "cx.batch", 
                      "attributes" = list( "path" = test_pgm_ref,
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )
  
  
  # -- expected 
  
  # - identify log
  exp_logpath <- list.files( test_wd, pattern = ".*\\.Rout$", recursive = TRUE, full.names = FALSE, include.dirs = FALSE )
  
  expected_log <- list( "path" = exp_logpath,
                        "sha" = digest::digest( file.path( test_wd, exp_logpath, fsep = "/" ), algo = "sha1", file = TRUE) ) 
  
  # - inputs
  expected_input_lst <- test_inputs
  expected_input_lst[ test_pgm_ref ] <- test_pgm_sha
  
  expected_inputs <- lapply( base::sort(base::names( expected_input_lst )), function(x) {
    list( "path" = x, 
          "sha" = base::unname(expected_input_lst[[x]]) )
  })
  
  
  # - result list
  expected_parms <- test_parms
  
  expected_resultnode <- list( "status" = "completed",
                               "log" = expected_log,
                               "files.input" = expected_inputs, 
                               "files.created" = list( list( "path" = expected_log[["path"]],
                                                             "sha" = expected_log[["sha"]]) ),
                               "files.updated" = list(),
                               "files.deleted" = list(),
                               "messages" = "None" )
  

  
  # -- assertions
  
  # - test parameters copy-forward
  testthat::expect_equal( result[ base::names(test_parms) ], expected_parms )
  
  # - result details
  testthat::expect_equal( result[["results"]][ base::names(expected_resultnode) ], expected_resultnode[ base::names(expected_resultnode) ] )
  
  
  # - log
  act_loglines <- base::readLines( file.path( test_wd, expected_log[["path"]], fsep = "/") )
  
  # - log errors
  testthat::expect_false( any(grepl( "^error:", act_loglines, ignore.case = TRUE)) )

  # - log warnings
  testthat::expect_false( any(grepl( "^warning:", act_loglines, ignore.case = TRUE)) )

  
})





testthat::test_that( "cxaction.batchr.execRProgramMultiInputsDefaultsCreateUpdateFiles", {
  
  #' @cx.tests Execute R program using CX batch action with multiple inputs to create new and update existing files
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - working directory
  
  test_wd <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_wd) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  
  # - inputs
  
  if ( dir.exists( file.path( test_wd, "inputs", fsep = "/") ) || ! dir.create( file.path( test_wd, "inputs", fsep = "/"), recursive = TRUE ) )
    testthat::fail( "Could not stage input file directory" )
  
  test_input_filenames <- replicate( 10, 
                                     cxlib::cxlib_standardpath( base::tempfile( pattern = "test-input-", tmpdir = file.path( test_wd, "inputs", fsep = "/"), fileext = ".txt") ), 
                                     simplify = TRUE )
  
  test_inputs <- base::unlist( sapply( test_input_filenames, function(x) {
    
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), sample( 50:500, 1 ), replace = TRUE) , collapse = ""), 
                      con = x )
    
    digest::digest( x, algo = "sha1", file = TRUE)
    
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  
  base::names(test_inputs) <- base::substring( base::names(test_inputs), base::nchar(test_wd) + 2 )
  
  
  # - outputs
  
  if ( dir.exists( file.path( test_wd, "outputs", fsep = "/") ) || ! dir.create( file.path( test_wd, "outputs", fsep = "/"), recursive = TRUE ) )
    testthat::fail( "Could not stage output directory" )
  

  test_output_filenames <- replicate( 10, 
                                      cxlib::cxlib_standardpath( base::tempfile( pattern = "test-output-", tmpdir = file.path( test_wd, "outputs", fsep = "/"), fileext = ".txt") ), 
                                      simplify = TRUE )
  
  test_outputs <- base::unlist( sapply( test_output_filenames, function(x) {
    
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), sample( 50:500, 1 ), replace = TRUE) , collapse = ""), 
                      con = x )
    
    digest::digest( x, algo = "sha1", file = TRUE)
    
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  
  base::names(test_outputs) <- base::substring( base::names(test_outputs), base::nchar(test_wd) + 2 )
  
  
    

  # - program
  
  test_pgm_lines <- c( "# test program", " " )
  
  test_filesto_update <- utils::head( base::names(test_outputs), n = 2 )
  
  for ( xoutput in test_filesto_update  )
    test_pgm_lines <- append( test_pgm_lines, 
                              c( "base::writeLines( paste( sample( base::letters, sample(100:1000, 1), replace = TRUE), collapse = \"\"),",
                                 paste( "                  con = ", base::dQuote( xoutput, q = FALSE), ")" ),
                                 " " ) )

    
  test_pgm <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( test_wd, "programs", fsep = "/"), fileext = ".R" ) )
  
  if ( dir.exists(base::dirname(test_pgm)) ||
       ! dir.create( base::dirname(test_pgm), recursive = TRUE ) ||
       inherits( try( base::writeLines( test_pgm_lines, con = test_pgm ), silent = FALSE ), "try-error" ) ||
       ! file.exists( test_pgm ) )
    testthat::fail( "Could not stage test program" )
  
  
  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
  
  
  
  # - test SHA-1  
  
  test_sha <- digest::digest( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), sample(100:500,1), replace = TRUE), collapse = "" ), 
                              algo = "sha1", 
                              file = FALSE )
  
  if ( test_sha == test_pgm_sha )
    testthat::fail( "Unexpected test SHA-1 equals the program file SHA-1" )
  
  
  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = "cx.batch", 
                      "attributes" = list( "path" = test_pgm_ref,
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )
  
  
  # -- expected 

  # - identify log
  exp_logpath <- list.files( test_wd, pattern = ".*\\.Rout$", recursive = TRUE, full.names = FALSE, include.dirs = FALSE )

  expected_log <- list( "path" = exp_logpath,
                        "sha" = digest::digest( file.path( test_wd, exp_logpath, fsep = "/" ), algo = "sha1", file = TRUE) )

  # # - inputs
  expected_input_lst <- append( test_inputs, test_outputs )
  expected_input_lst[ test_pgm_ref ] <- test_pgm_sha

  expected_inputs <- lapply( base::sort(base::names( expected_input_lst )), function(x) {
    list( "path" = x,
          "sha" = base::unname(expected_input_lst[[x]]) )
  })


  
  
  # - updated
  
  expected_update <- lapply( base::sort(test_filesto_update), function(x) {
    list( "path" = x, 
          "sha" = digest::digest( file.path( test_wd, x, fsep = "/" ), algo = "sha1", file = TRUE ) )
  })
    


  # - result list
  expected_parms <- test_parms

  expected_resultnode <- list( "status" = "completed",
                               "log" = expected_log,
                               "files.input" = expected_inputs,
                               "files.created" = list( expected_log ),
                               "files.updated" = expected_update,
                               "files.deleted" = list(),
                               "messages" = "None" )



  # -- assertions
  
  # - test parameters copy-forward
  testthat::expect_equal( result[ base::names(test_parms) ], expected_parms )

  # - result details
  testthat::expect_equal( result[["results"]][ base::names(expected_resultnode) ], expected_resultnode[ base::names(expected_resultnode) ] )


  # - log
  act_loglines <- base::readLines( file.path( test_wd, expected_log[["path"]], fsep = "/") )

  # - log errors
  testthat::expect_false( any(grepl( "^error:", act_loglines, ignore.case = TRUE)) )

  # - log warnings
  testthat::expect_false( any(grepl( "^warning:", act_loglines, ignore.case = TRUE)) )

})






testthat::test_that( "cxaction.batchr.execRProgramMultiInputsDefaultsCreateUpdateDeleteFiles", {
  
  #' @cx.tests Execute R program using CX batch action with multiple inputs to create new and update and delete existing files
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - working directory
  
  test_wd <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_wd) || ! dir.create( test_wd, recursive = TRUE ) )
    testthat::fail( "Could not stage test working directory" )
  
  
  # - inputs
  
  if ( dir.exists( file.path( test_wd, "inputs", fsep = "/") ) || ! dir.create( file.path( test_wd, "inputs", fsep = "/"), recursive = TRUE ) )
    testthat::fail( "Could not stage input file directory" )
  
  test_input_filenames <- replicate( 10, 
                                     cxlib::cxlib_standardpath( base::tempfile( pattern = "test-input-", tmpdir = file.path( test_wd, "inputs", fsep = "/"), fileext = ".txt") ), 
                                     simplify = TRUE )
  
  test_inputs <- base::unlist( sapply( test_input_filenames, function(x) {
    
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), sample( 50:500, 1 ), replace = TRUE) , collapse = ""), 
                      con = x )
    
    digest::digest( x, algo = "sha1", file = TRUE)
    
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  
  base::names(test_inputs) <- base::substring( base::names(test_inputs), base::nchar(test_wd) + 2 )
  
  
  # - outputs
  
  if ( dir.exists( file.path( test_wd, "outputs", fsep = "/") ) || ! dir.create( file.path( test_wd, "outputs", fsep = "/"), recursive = TRUE ) )
    testthat::fail( "Could not stage output directory" )
  
  
  test_output_filenames <- replicate( 10, 
                                      cxlib::cxlib_standardpath( base::tempfile( pattern = "test-output-", tmpdir = file.path( test_wd, "outputs", fsep = "/"), fileext = ".txt") ), 
                                      simplify = TRUE )
  
  test_outputs <- base::unlist( sapply( test_output_filenames, function(x) {
    
    base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), sample( 50:500, 1 ), replace = TRUE) , collapse = ""), 
                      con = x )
    
    digest::digest( x, algo = "sha1", file = TRUE)
    
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  
  base::names(test_outputs) <- base::substring( base::names(test_outputs), base::nchar(test_wd) + 2 )
  
  
  
  
  # - program
  
  test_pgm_lines <- c( "# test program", " " )
  
  test_filesto_update <- utils::head( base::names(test_outputs), n = 2 )
  
  for ( xoutput in test_filesto_update  )
    test_pgm_lines <- append( test_pgm_lines, 
                              c( "base::writeLines( paste( sample( base::letters, sample(100:1000, 1), replace = TRUE), collapse = \"\"),",
                                 paste( "                  con = ", base::dQuote( xoutput, q = FALSE), ")" ),
                                 " " ) )
  
  
  test_filesto_delete <- utils::tail( base::names(test_outputs), n = 2 )
  
  for ( xoutput in test_filesto_delete  )
    test_pgm_lines <- append( test_pgm_lines, 
                              c( paste( "file.remove( ", base::dQuote( xoutput, q = FALSE), ")" ),
                                 " " ) )
  


  test_pgm <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( test_wd, "programs", fsep = "/"), fileext = ".R" ) )
  
  if ( dir.exists(base::dirname(test_pgm)) ||
       ! dir.create( base::dirname(test_pgm), recursive = TRUE ) ||
       inherits( try( base::writeLines( test_pgm_lines, con = test_pgm ), silent = FALSE ), "try-error" ) ||
       ! file.exists( test_pgm ) )
    testthat::fail( "Could not stage test program" )
  
  
  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
  
  
  
  # - test SHA-1  
  
  test_sha <- digest::digest( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), sample(100:500,1), replace = TRUE), collapse = "" ), 
                              algo = "sha1", 
                              file = FALSE )
  
  if ( test_sha == test_pgm_sha )
    testthat::fail( "Unexpected test SHA-1 equals the program file SHA-1" )
  
  
  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = "cx.batch", 
                      "attributes" = list( "path" = test_pgm_ref,
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )
  
  
  # -- expected 
  
  # - identify log
  exp_logpath <- list.files( test_wd, pattern = ".*\\.Rout$", recursive = TRUE, full.names = FALSE, include.dirs = FALSE )
  
  expected_log <- list( "path" = exp_logpath,
                        "sha" = digest::digest( file.path( test_wd, exp_logpath, fsep = "/" ), algo = "sha1", file = TRUE) )
  
  # # - inputs
  expected_input_lst <- append( test_inputs, test_outputs )
  expected_input_lst[ test_pgm_ref ] <- test_pgm_sha
  
  expected_inputs <- lapply( base::sort(base::names( expected_input_lst )), function(x) {
    list( "path" = x,
          "sha" = base::unname(expected_input_lst[[x]]) )
  })
  
  
  
  
  # - updated
  
  expected_update <- lapply( base::sort(test_filesto_update), function(x) {
    list( "path" = x, 
          "sha" = digest::digest( file.path( test_wd, x, fsep = "/" ), algo = "sha1", file = TRUE ) )
  })
  
  
  # - deleted
  
  expected_deleted <- lapply( base::sort(test_filesto_delete), function(x) {
    list( "path" = x, 
          "sha" = base::unname(test_outputs[x]) )
  })
  

  
  # - result list
  expected_parms <- test_parms
  
  expected_resultnode <- list( "status" = "completed",
                               "log" = expected_log,
                               "files.input" = expected_inputs,
                               "files.created" = list( expected_log ),
                               "files.updated" = expected_update,
                               "files.deleted" = expected_deleted,
                               "messages" = "None" )
  
  
  
  # -- assertions
  
  # - test parameters copy-forward
  testthat::expect_equal( result[ base::names(test_parms) ], expected_parms )
  
  # - result details
  testthat::expect_equal( result[["results"]][ base::names(expected_resultnode) ], expected_resultnode[ base::names(expected_resultnode) ] )
  
  
  # - log
  act_loglines <- base::readLines( file.path( test_wd, expected_log[["path"]], fsep = "/") )
  
  # - log errors
  testthat::expect_false( any(grepl( "^error:", act_loglines, ignore.case = TRUE)) )
  
  # - log warnings
  testthat::expect_false( any(grepl( "^warning:", act_loglines, ignore.case = TRUE)) )
  
})



# cat( c( "--------------------------", readLines( file.path( test_wd, expected_resultnode[["log"]][["path"]], fsep = "/" ) ), "--------------------------"), sep = "\n" )



