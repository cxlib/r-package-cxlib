#
#  Tests for cxlib::cxlib_batch()
#
#
#
#

testthat::test_that( "batch.ProgramNotExist", {
  
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
  
  test_program <- file.path( test_program_parent, 
                             base::basename( base::tempfile( pattern = "test-program-", tmpdir = test_program_parent, fileext = ".R" ) ),
                             fsep = "/" )
  
  
  if ( file.exists( file.path( test_wd, test_program, fsep = "/" ) ) )
    testthat::fail( "Unexpected test program exists" )
  
  
  # -- test
  
  testthat::expect_error( cxlib::cxlib_batch( test_program, silent = TRUE ),
                          regexp = "^One or more specified programs do not exist$" )
  
  
})




testthat::test_that( "batch.oneOfProgramsNotExist", {
  
  
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
  
  for ( xpgm in utils::head( test_programs, n = length(test_programs) - 1 ) )
    base::writeLines( paste( "# Empty test program", xpgm ), 
                      con = file.path( test_wd, xpgm, fsep = "/") )
  
  
  if ( file.exists( file.path( test_wd, utils::tail( test_programs, n = 1 ), fsep = "/" ) ) )
    testthat::fail( "Unexpected test program exists" )
  
  
  # -- test
  
  testthat::expect_error( cxlib::cxlib_batch( test_programs, silent = TRUE ), 
                          regexp = "^One or more specified programs do not exist$" )
  
})





testthat::test_that( "batch.localPrograms", {
  
  
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
  
  
  
  # -- test
  
  result <- cxlib::cxlib_batch( test_programs, silent = TRUE )
  
  
  
  # -- expected
  
  # - programs
  
  expected_programs <- test_programs
  
  
  
  # - logs
  
  expected_logs <- paste0( tools::file_path_sans_ext( test_programs ), ".Rout" )
  
  
  
  # -- assertions
  
  
  # - programs
  
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_programs, fsep = "/" ) ) ) )
  
  
  # - log
  
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_logs, fsep = "/" ) ) ) )

})




testthat::test_that( "batch.localProgramsAltLog", {
  
  
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
  
  
  # - test log
  
  test_log_parent <- file.path( "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_log_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test log parent directory" )
  

  
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
  
  
  
  # -- test
  
  result <- cxlib::cxlib_batch( test_programs, options = list( "logs" = test_log_parent ), silent = TRUE )
  
  
  
  # -- expected
  
  # - programs
  expected_programs <- test_programs

  
  # - logs
  expected_logs <- file.path( test_log_parent, base::basename( paste0( tools::file_path_sans_ext( test_programs ), ".Rout" ) ), fsep = "/" )
  

  
  # -- assertions
  
  
  # - programs
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_programs, fsep = "/" ) ) ) )
  
  
  # - log
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_logs, fsep = "/" ) ) ) )
  
  
})





testthat::test_that( "batch.localProgramsAltLogAltExt", {
  
  
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
  
  
  # - test log
  
  test_log_parent <- file.path( "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_log_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test log parent directory" )
  
  
  # - test log extension
  
  test_logext <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 10 ) , collapse = "" )
  
  
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
  
  
  
  # -- test
  
  result <- cxlib::cxlib_batch( test_programs, options = list( "logs" = test_log_parent, "log.fileext" = test_logext ), silent = TRUE )
  
  
  
  # -- expected
  
  # - programs
  expected_programs <- test_programs
  
  
  # - logs
  expected_logs <- file.path( test_log_parent, base::basename( paste0( tools::file_path_sans_ext( test_programs ), ".", test_logext ) ), fsep = "/" )
  

  
  # -- assertions
  
  
  # - programs
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_programs, fsep = "/" ) ) ) )
  
  
  # - log
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_logs, fsep = "/" ) ) ) )

})






testthat::test_that( "batch.localCleaned", {
  
  
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
  
  
  # - test log
  
  test_log_parent <- file.path( "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_log_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test log parent directory" )
  
  
  # - test log extension
  
  test_logext <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 10 ) , collapse = "" )
  
  
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
  
  
  
  # -- test
  
  result <- cxlib::cxlib_batch( test_programs, options = list( "logs" = test_log_parent, "log.fileext" = test_logext ), silent = TRUE )
  
  
  
  # -- expected
  
  # - programs
  expected_programs <- test_programs
  
  
  # - logs
  expected_logs <- file.path( test_log_parent, base::basename( paste0( tools::file_path_sans_ext( test_programs ), ".", test_logext ) ), fsep = "/" )
  
  
  
  # -- assertions
  
  
  # - programs
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_programs, fsep = "/" ) ) ) )
  
  
  # - log
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_logs, fsep = "/" ) ) ) )
  
  
  # - cleaned 
  
  testthat::expect_length( list.files( test_jobpath, recursive = TRUE, all.files = TRUE, include.dirs = TRUE ), 0 )

})




testthat::test_that( "batch.localCleanedAltWork", {
  
  
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
  

  # - test work path
  
  test_workpath <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-workpath-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_workpath ) || ! dir.create( test_workpath, recursive = TRUE ) )
    testthat::fail( "Could not stage test work directory" )
  
  
    
  # - cxlib properties
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ),
                       paste0( "WORKPATH=", test_workpath ) ),
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
  
  
  # - test log
  
  test_log_parent <- file.path( "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_log_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test log parent directory" )
  
  
  # - test log extension
  
  test_logext <- paste( sample( c( base::LETTERS, base::letters, as.character(0:9) ), 10 ) , collapse = "" )
  
  
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
  
  
  
  # -- test
  
  result <- cxlib::cxlib_batch( test_programs, options = list( "logs" = test_log_parent, "log.fileext" = test_logext ), silent = TRUE )
  
  
  
  # -- expected
  
  # - programs
  expected_programs <- test_programs
  
  
  # - logs
  expected_logs <- file.path( test_log_parent, base::basename( paste0( tools::file_path_sans_ext( test_programs ), ".", test_logext ) ), fsep = "/" )
  
  
  
  # -- assertions
  
  
  # - programs
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_programs, fsep = "/" ) ) ) )
  
  
  # - log
  testthat::expect_true( all( file.exists( file.path( test_wd, expected_logs, fsep = "/" ) ) ) )
  
  
  # - cleaned 
  
  testthat::expect_length( list.files( test_jobpath, recursive = TRUE, all.files = TRUE, include.dirs = TRUE ), 0 )
  
})
