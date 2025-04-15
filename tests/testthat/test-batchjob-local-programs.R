#
#
#  Tests for cxlib::cxlib_batchjob()
#
#  Local programs
#
#
#



testthat::test_that( "batchjob.localProgramNotExist", {
  
  
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
  
  testthat::expect_error( cxlib::cxlib_batchjob( test_program ), 
                          regexp = paste( "^Program", test_program, "does not exist$") )
  
})



testthat::test_that( "batchjob.localJobDefProgramNotExist", {
  
  
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
  
  testthat::expect_error( cxlib::cxlib_batchjob( list( "programs" = test_program )), 
                          regexp = paste( "^Program", test_program, "does not exist$") )
  
})




testthat::test_that( "batchjob.localOneOfProgramsNotExist", {
  
  
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
  
  testthat::expect_error( cxlib::cxlib_batchjob( test_programs ), 
                          regexp = paste( "^Program", utils::tail( test_programs, n = 1 ), "does not exist$" ) )
  
})




testthat::test_that( "batchjob.localJobDefOneOfProgramsNotExist", {
  
  
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
  
  testthat::expect_error( cxlib::cxlib_batchjob( list( "programs" = test_programs ) ), 
                          regexp = paste( "^Program", utils::tail( test_programs, n = 1 ), "does not exist$" ) )
  
})





testthat::test_that( "batchjob.localPrograms", {
  
  
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
  
  result <- cxlib::cxlib_batchjob( test_programs )



  # -- expected

  # - programs
  
  expected_programs <- test_programs

  
  
  # - logs
  
  expected_logs <- paste0( tools::file_path_sans_ext( test_programs ), ".Rout" )
  
  
    
  # -- assertions
  
  
  # - staged programs

  testthat::expect_true( all( file.exists( file.path( test_jobpath, result$.attr[["id"]], ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, result$.attr[["id"]], ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {

    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )
    
    testthat::expect_equal( json_data[["type"]], "program" )
    
    testthat::expect_equal( json_data[["path"]], test_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]], 
                            digest::digest( file.path( test_jobpath, result$.attr[["id"]], ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] ) 

    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )

  }
    
  
})






testthat::test_that( "batchjob.localJobDefPrograms", {
  
  
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
  
  result <- cxlib::cxlib_batchjob( list( "programs" = test_programs ) )
  
  
  
  # -- expected
  
  # - programs
  
  expected_programs <- test_programs
  
  
  
  # - logs
  
  expected_logs <- paste0( tools::file_path_sans_ext( test_programs ), ".Rout" )
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, result$.attr[["id"]], ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, result$.attr[["id"]], ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {
    
    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )
    
    testthat::expect_equal( json_data[["type"]], "program" )
    
    testthat::expect_equal( json_data[["path"]], test_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]], 
                            digest::digest( file.path( test_jobpath, result$.attr[["id"]], ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] ) 
    
    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )
    
  }
  
  
})






testthat::test_that( "batchjob.localJobDefProgramsOptionAltLogPath", {
  
  
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
  
  
  # - test log
  
  test_log_parent <- file.path( "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_log_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test log parent directory" )
  

  
  # -- test
  
  result <- cxlib::cxlib_batchjob( list( "programs" = test_programs, 
                                         "options" = list( "logs" = test_log_parent ) ) )
  
  
  
  # -- expected
  
  # - programs
  
  expected_programs <- test_programs
  
  
  
  # - logs
  
  expected_logs <- file.path( test_log_parent, base::basename( paste0( tools::file_path_sans_ext( test_programs ), ".Rout" ) ), fsep = "/" )
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, result$.attr[["id"]], ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, result$.attr[["id"]], ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {
    
    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )
    
    testthat::expect_equal( json_data[["type"]], "program" )
    
    testthat::expect_equal( json_data[["path"]], test_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]], 
                            digest::digest( file.path( test_jobpath, result$.attr[["id"]], ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] ) 
    
    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )
    
  }
  
  
})






testthat::test_that( "batchjob.localJobDefProgramsOptionAltLogExt", {
  
  
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
  
  result <- cxlib::cxlib_batchjob( list( "programs" = test_programs, 
                                         "options" = list( "log.fileext" = "log" ) ) )
  
  
  
  # -- expected
  
  # - programs
  
  expected_programs <- test_programs
  
  
  
  # - logs
  
  expected_logs <- paste0( tools::file_path_sans_ext( test_programs ), ".log" )
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, result$.attr[["id"]], ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, result$.attr[["id"]], ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {
    
    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )
    
    testthat::expect_equal( json_data[["type"]], "program" )
    
    testthat::expect_equal( json_data[["path"]], test_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]], 
                            digest::digest( file.path( test_jobpath, result$.attr[["id"]], ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] ) 
    
    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )
    
  }
  
  
})





testthat::test_that( "batchjob.localJobDefProgramsOptionAltLogPathExt", {
  
  
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
  
  
  # - test log
  
  test_log_parent <- file.path( "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_log_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test log parent directory" )
  
  
  
  # -- test
  
  result <- cxlib::cxlib_batchjob( list( "programs" = test_programs, 
                                         "options" = list( "logs" = test_log_parent,
                                                           "log.fileext" = "log" ) ) )
  
  
  
  # -- expected
  
  # - programs
  
  expected_programs <- test_programs
  
  
  
  # - logs
  
  expected_logs <- file.path( test_log_parent, base::basename( paste0( tools::file_path_sans_ext( test_programs ), ".log" ) ), fsep = "/" )
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, result$.attr[["id"]], ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, result$.attr[["id"]], ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {
    
    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )
    
    testthat::expect_equal( json_data[["type"]], "program" )
    
    testthat::expect_equal( json_data[["path"]], test_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]], 
                            digest::digest( file.path( test_jobpath, result$.attr[["id"]], ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] ) 
    
    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )
    
  }
  
  
})




testthat::test_that( "batchjob.localJobDefProgramsPropAltLogPathExt", {
  
  
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
  
  
    
  # - test log
  
  test_log_parent <- file.path( "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_log_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test log parent directory" )
  
  
  
  # - cxlib properties
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ),
                       paste0( "LOGS=", test_log_parent ),
                       paste0( "LOG.FILEEXT=log" )),
                    con = file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) ) )  
    testthat::fail( "Could not stage cxlib.properties" )
  

  
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
  
  result <- cxlib::cxlib_batchjob( list( "programs" = test_programs ) )
  
  
  
  # -- expected
  
  # - programs
  
  expected_programs <- test_programs
  
  
  
  # - logs
  
  expected_logs <- file.path( test_log_parent, base::basename( paste0( tools::file_path_sans_ext( test_programs ), ".log" ) ), fsep = "/" )
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, result$.attr[["id"]], ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, result$.attr[["id"]], ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {
    
    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )
    
    testthat::expect_equal( json_data[["type"]], "program" )
    
    testthat::expect_equal( json_data[["path"]], test_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]], 
                            digest::digest( file.path( test_jobpath, result$.attr[["id"]], ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] ) 
    
    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )
    
  }
  
  
})




testthat::test_that( "batchjob.localJobDefProgramsPropAltLogPathExtOptionPrecedence", {
  
  
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
  
  
  
  # - test log
  
  test_log_parent <- file.path( "path", "to", "logs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_log_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_log_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test log parent directory" )
  

  # - test log alternative (options)
  
  test_log_parent_alt <- file.path( "path", "to", "logs-from-opt", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_log_parent_alt, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_log_parent_alt, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage alternative test log parent directory" )
  
    
  
  # - cxlib properties
  
  base::writeLines( c( "# test properties", 
                       paste0( "PATH=", test_jobpath ),
                       paste0( "LOGS=", test_log_parent ),
                       paste0( "LOG.FILEEXT=log" )),
                    con = file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) )
  
  if ( ! file.exists( file.path( test_lbpath, "cxlib", "cxlib.properties", fsep = "/" ) ) )  
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  
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
  
  result <- cxlib::cxlib_batchjob( list( "programs" = test_programs, 
                                         "options" = list( "logs" = test_log_parent_alt,
                                                           "log.fileext" = "Rlog")) )
  
  
  
  # -- expected
  
  # - programs
  
  expected_programs <- test_programs
  
  
  
  # - logs
  
  expected_logs <- file.path( test_log_parent_alt, base::basename( paste0( tools::file_path_sans_ext( test_programs ), ".Rlog" ) ), fsep = "/" )
  
  
  
  # -- assertions
  
  
  # - staged programs
  
  testthat::expect_true( all( file.exists( file.path( test_jobpath, result$.attr[["id"]], ".work", expected_programs, fsep = "/" ) ) ) )
  
  
  # - staged actions
  
  for ( xaction in list.files( file.path( test_jobpath, result$.attr[["id"]], ".job", fsep = "/" ), pattern = "^\\d+-action-", full.names = TRUE ) ) {
    
    action_idx <- as.numeric( gsub( "(\\d+)-action-.*", "\\1", base::basename(xaction) ) )
    
    json_data <- jsonlite::fromJSON( xaction )
    
    testthat::expect_equal( json_data[["type"]], "program" )
    
    testthat::expect_equal( json_data[["path"]], test_programs[ action_idx ] )
    testthat::expect_equal( json_data[["sha1"]], 
                            digest::digest( file.path( test_jobpath, result$.attr[["id"]], ".work", json_data[["path"]], fsep = "/" ), algo = "sha1", file = TRUE ) )
    
    testthat::expect_equal( json_data[["log"]][["path"]], expected_logs[ action_idx ] ) 
    
    testthat::expect_true( is.na( json_data[["log"]][["sha1"]] ) )
    
  }
  
  
})

