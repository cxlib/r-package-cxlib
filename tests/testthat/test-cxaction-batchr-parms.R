#
#
# Tests for cxaction cx.batch.r and cx.batch
#
#


#' @cx.testsfor cxlib:::.cxaction_batchr()



testthat::test_that( "cxaction.batchr.missingParms", {
  
  #' @cx.tests Missing parameters for CX action batchr results in an error
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxaction_batchr(), regexp = "^The action is missing one or more required standard definition properties$" )
  
})





testthat::test_that( "cxaction.batchr.parmNotList", {
  
  #' @cx.tests Parameters of an invalid type for CX action batchr results in an error
  
  
  # -- stage
  
  test_parms <- c( "id" = 1, "type" = 2, "attributes" = 3, "job.id" = 4 )

  if ( ! inherits( test_parms, "numeric" ) )
    testthat::fail( "Could not stage a numeric parameter list of valid elemenent names" )

  
  # -- test
  testthat::expect_error( cxlib:::.cxaction_batchr( test_parms ), regexp = "^The action is missing one or more required standard definition properties$" )
  
})






testthat::test_that( "cxaction.batchr.parmReqMissing", {
  
  #' @cx.tests Missing required parameters for CX action batchr results in an error
  
  
  # -- stage
  
  test_parms <- sapply( c( "id", "type", "attributes", "job.id" ), function(x) {
    paste( sample( base::letters, sample( 5:40, 1), replace = TRUE ), collapse = "")
  }, USE.NAMES = TRUE )



  # -- test and assertion
  
  for ( xitem in base::names(test_parms) )
   testthat::expect_error( cxlib:::.cxaction_batchr( as.list(test_parms[ ! base::names(test_parms) %in% xitem ]) ), 
                           regexp = "^The action is missing one or more required standard definition properties$" )
  
})




testthat::test_that( "cxaction.batchr.parmInvalidType", {
  
  #' @cx.tests Invalid action reference for CX action batchr results in an error
  
  
  # -- stage
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                 "type" = paste( sample( base::letters, sample( 5:40, 1), replace = TRUE ), collapse = ""), 
                 "attributes" = list(), 
                 "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  if ( test_parms[["type"]] %in% c( "cx.batch.r", "cx.batch") )
    testthat::fail( "Unexcpted random type equals a valid action reference" )
  
  
  
  # -- test and assertion
  
  testthat::expect_error( cxlib:::.cxaction_batchr( test_parms ), 
                          regexp = "^The action is not supported$" )
  
})




testthat::test_that( "cxaction.batchr.parmWorkAreaNULL", {
  
  #' @cx.tests Work area specified as a NULL value for CX action batchr results in an error
  
  
  # -- stage
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list(), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  

  # -- test and assertion
  
  testthat::expect_error( cxlib:::.cxaction_batchr( test_parms, work.area = NULL ), 
                          regexp = "^The work area directory is missing, invalid or does not exist$" )
  
})






testthat::test_that( "cxaction.batchr.parmWorkAreaNA", {
  
  #' @cx.tests Work area specified as NA for CX action batchr results in an error
  
  
  # -- stage

  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list(), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  # -- test and assertion
  
  testthat::expect_error( cxlib:::.cxaction_batchr( test_parms, work.area = NA ), 
                          regexp = "^The work area directory is missing, invalid or does not exist$" )
  
})





testthat::test_that( "cxaction.batchr.parmWorkAreaEmptyString", {
  
  #' @cx.tests Work area specified as an empty string for CX action batchr results in an error
  
  
  # -- stage
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list(), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  # -- test and assertion
  
  testthat::expect_error( cxlib:::.cxaction_batchr( test_parms, work.area = "   " ), 
                          regexp = "^The work area directory is missing, invalid or does not exist$" )
  
})







testthat::test_that( "cxaction.batchr.parmWorkAreaDirecrtoryDoesNotExist", {
  
  #' @cx.tests Specified work area directory that does not exist for CX action batchr results in an error
  
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - working directory
  
  test_wd <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-working-directory-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_wd ) )
    testthat::fail( "Unexpected test working directory exists" )
  
  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list(), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test and assertion
  
  testthat::expect_error( cxlib:::.cxaction_batchr( test_parms, work.area = test_wd ), 
                          regexp = "^The work area directory is missing, invalid or does not exist$" )
  
})





testthat::test_that( "cxaction.batchr.parmActionAttrMissing", {
  
  #' @cx.tests Action attributes missing for CX action batchr results in an error
  
  
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
  
  
  # - parameters 
  #   note: attributes should be empty
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list(), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test and assertion
  
  testthat::expect_error( cxlib:::.cxaction_batchr( test_parms, work.area = test_wd ), 
                          regexp = "^One or more required action attributes missing$" )
  
})





testthat::test_that( "cxaction.batchr.parmActionAttrMissing", {
  
  #' @cx.tests A required missing action attribute for CX action batchr results in an error
  
  
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

  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
    
  
  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  

  
  # - parameters 

  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list( "path" = test_pgm_ref,
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test and assertions
  for ( xitem in c( "path", "path.sha") ) { 
    
    test_scenario <- test_parms
    test_scenario[["attributes"]] <- test_parms[["attributes"]][ ! base::names(test_parms[["attributes"]]) %in% xitem ]
    
    testthat::expect_error( cxlib:::.cxaction_batchr( test_scenario, work.area = test_wd ), regexp = "^One or more required action attributes missing$" )
  }
  

})





testthat::test_that( "cxaction.batchr.parmActionProgramPathInvalidType", {
  
  #' @cx.tests An invalid type for action attribute path for CX action batchr results in an error state
  
  
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
  
  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
  
  
  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  
  
  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list( "path" = as.numeric(1),
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )

 
  # -- expected 
  
  expected_result <- test_parms
  
  expected_result[["results"]] <- list( "status" = "error", 
                                        "start" = NA, 
                                        "end" = NA,
                                        "log" = list( "path" = NA, 
                                                      "sha" = NA),
                                        "files.input" = list( list( "path" = test_pgm_ref, 
                                                                    "sha" = test_pgm_sha ) ), 
                                        "files.created" = list(), 
                                        "files.updated" = list(),
                                        "files.deleted" = list(),
                                        "messages" = "Program path does not exist in compute work area" )


  # -- assertions
  
  testthat::expect_equal( result, expected_result )
  

})




testthat::test_that( "cxaction.batchr.parmActionProgramPathNULL", {
  
  #' @cx.tests Action attribute path equal to NULL for CX action batchr results in an error state
  
  
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
  
  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
  
  
  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  
  
  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list( "path" = NULL,
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )
  
  
  # -- expected 
  
  expected_result <- test_parms
  
  expected_result[["results"]] <- list( "status" = "error", 
                                        "start" = NA, 
                                        "end" = NA,
                                        "log" = list( "path" = NA, 
                                                      "sha" = NA),
                                        "files.input" = list( list( "path" = test_pgm_ref, 
                                                                    "sha" = test_pgm_sha ) ), 
                                        "files.created" = list(), 
                                        "files.updated" = list(),
                                        "files.deleted" = list(),
                                        "messages" = "Program path does not exist in compute work area" )
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_result )
  
  
})




testthat::test_that( "cxaction.batchr.parmActionProgramPathNA", {
  
  #' @cx.tests Action attribute path equal to NA for CX action batchr results in an error state
  
  
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
  
  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
  
  
  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  
  
  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list( "path" = NA,
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )
  
  
  # -- expected 
  
  expected_result <- test_parms
  
  expected_result[["results"]] <- list( "status" = "error", 
                                        "start" = NA, 
                                        "end" = NA,
                                        "log" = list( "path" = NA, 
                                                      "sha" = NA),
                                        "files.input" = list( list( "path" = test_pgm_ref, 
                                                                    "sha" = test_pgm_sha ) ), 
                                        "files.created" = list(), 
                                        "files.updated" = list(),
                                        "files.deleted" = list(),
                                        "messages" = "Program path does not exist in compute work area" )
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_result )
  
  
})





testthat::test_that( "cxaction.batchr.parmActionProgramPathEmptyString", {
  
  #' @cx.tests Action attribute path equal to an empty string for CX action batchr results in an error state
  
  
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
  
  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
  
  
  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  
  
  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list( "path" = "   ",
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )
  
  
  # -- expected 
  
  expected_result <- test_parms
  
  expected_result[["results"]] <- list( "status" = "error", 
                                        "start" = NA, 
                                        "end" = NA,
                                        "log" = list( "path" = NA, 
                                                      "sha" = NA),
                                        "files.input" = list( list( "path" = test_pgm_ref, 
                                                                    "sha" = test_pgm_sha ) ), 
                                        "files.created" = list(), 
                                        "files.updated" = list(),
                                        "files.deleted" = list(),
                                        "messages" = "Program path does not exist in compute work area" )
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_result )
  
  
})




testthat::test_that( "cxaction.batchr.parmActionProgramPathNotExist", {
  
  #' @cx.tests Action attribute path not exists for CX action batchr results in an error state
  
  
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
  
  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
  
  
  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  
  
  # - alternative test program path
  test_pgm_altpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( test_wd, "programs", fsep = "/"), fileext = ".R" ) )
  
  if ( file.exists( test_pgm_altpath) )
    testthat::fail( "Unexpected alternative test program path exists" )
  
  test_pgm_altpath_ref <- base::substring( test_pgm_altpath, base::nchar(test_wd) + 2 )
  
  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = sample( c( "cx.batch.r", "cx.batch"), 1), 
                      "attributes" = list( "path" = test_pgm_altpath_ref,
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )
  

  # -- expected 
  

  # - result list  
  expected_result <- test_parms
  
  expected_result[["results"]] <- list( "status" = "error", 
                                        "start" = NA, 
                                        "end" = NA,
                                        "log" = list( "path" = NA, 
                                                      "sha" = NA),
                                        "files.input" = list( list( "path" = test_pgm_ref, 
                                                                    "sha" = test_pgm_sha ) ), 
                                        "files.created" = list(), 
                                        "files.updated" = list(),
                                        "files.deleted" = list(),
                                        "messages" = "Program path does not exist in compute work area" )
  
  # - program path
  expected_pathattr <- test_pgm_altpath_ref
  
  
  # -- assertions

  # - alternative test path reported in results
  testthat::expect_equal( result[["attributes"]][["path"]], expected_pathattr )
    
  # - result
  testthat::expect_equal( result, expected_result )

})








testthat::test_that( "cxaction.batchr.parmCxBatchActionProgramPathInvalidFileExt", {
  
  #' @cx.tests Action attribute path refers to path with file extension not .R for general CX action cx.batch results in an error state
  
  
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
  
  test_pgm <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", tmpdir = file.path( test_wd, "programs", fsep = "/"), fileext = ".notR" ) )
  
  if ( dir.exists(base::dirname(test_pgm)) ||
       ! dir.create( base::dirname(test_pgm), recursive = TRUE ) ||
       inherits( try( base::writeLines( test_pgm_lines, con = test_pgm ), silent = FALSE ), "try-error" ) ||
       ! file.exists( test_pgm ) )
    testthat::fail( "Could not stage test program" )
  
  
  test_pgm_sha <- digest::digest( test_pgm, algo = "sha1", file = TRUE )
  
  
  # note: + 2 is to start reference after "/" in "<test_wd>/" ... make it relative
  test_pgm_ref <- base::substring( test_pgm, base::nchar(test_wd) + 2 )
  
  

  
  # - parameters 
  
  test_parms <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"), 
                      "type" = "cx.batch", 
                      "attributes" = list( "path" = test_pgm_ref,
                                           "path.sha" = test_pgm_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )
  

  
  # -- expected 
  
  
  # - result list  
  expected_result <- test_parms
  
  expected_result[["results"]] <- list( "status" = "error", 
                                        "start" = NA, 
                                        "end" = NA,
                                        "log" = list( "path" = NA, 
                                                      "sha" = NA),
                                        "files.input" = list( list( "path" = test_pgm_ref, 
                                                                    "sha" = test_pgm_sha ) ), 
                                        "files.created" = list(), 
                                        "files.updated" = list(),
                                        "files.deleted" = list(),
                                        "messages" = "Expecting a program with file extension .R for action cx.batch" )
  

  
  # -- assertions

  # - result
  testthat::expect_equal( result, expected_result )
  
})






testthat::test_that( "cxaction.batchr.parmCxBatchActionProgramPathDigestNotEqual", {
  
  #' @cx.tests Action path SHA-1 attribute not equal to SHA-1 of file referenced by the path attribute for CX action batchr results in an error state
  
  
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
                                           "path.sha" = test_sha ), 
                      "job.id" = cxlib:::.cxlib_referenceid( type = "job") )
  
  
  
  
  
  # -- test 
  
  result <- cxlib:::.cxaction_batchr( test_parms, work.area = test_wd )
  

  
  # -- expected 
  
  
  # - result list  
  expected_result <- test_parms
  
  expected_result[["results"]] <- list( "status" = "error", 
                                        "start" = NA, 
                                        "end" = NA,
                                        "log" = list( "path" = NA, 
                                                      "sha" = NA),
                                        "files.input" = list( list( "path" = test_pgm_ref, 
                                                                    "sha" = test_pgm_sha ) ), 
                                        "files.created" = list(), 
                                        "files.updated" = list(),
                                        "files.deleted" = list(),
                                        "messages" = "Program file SHA-1 integrity check failure" )
  

  # -- assertions
  
  # - result
  testthat::expect_equal( result, expected_result )
  
})





