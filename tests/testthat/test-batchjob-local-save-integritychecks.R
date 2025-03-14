#
#
#  Tests for cxlib::cxlib_batchjob()
#
#  Save local file system
#
#  Integrity checks
#




testthat::test_that( "batchjob.integrityProgramMissing", {
  
  
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
  
  
  

  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  # inert test program
  base::writeLines( "# test program", con = test_program )

  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
    
  
  test_program_ref <- base::substring( test_program, base::nchar(test_wd) + 2 )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_ref ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  
  # - delete test program
  file.remove( test_program )
  
  if ( file.exists( test_program ) )
    testthat::fail( "Could not delete test program" )
   
  
  
  # -- test
  
  testthat::expect_error( test_job$save(), 
                          regexp = paste( "^The source for the program", test_program_ref, "no longer exists$" ) )

})




testthat::test_that( "batchjob.integrityProgramUpdate", {
  
  
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
  
  
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  # inert test program
  base::writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_wd) + 2 )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_ref ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  
  # - update test program

  cat( paste( "# job", test_id), file = test_program, append = TRUE )

  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist after update" )
  
  
  
  # -- test
  
  testthat::expect_error( test_job$save(), 
                          regexp = paste( "^The program", test_program_ref, "has changed since submitted or integrity cannot be verified$" ) )

})




testthat::test_that( "batchjob.integrityProgramMissingLogEntry", {
  
  
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
  
  
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  # inert test program
  base::writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_wd) + 2 )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_ref ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  
  # - delete log entry 

  # identify action
  job_action <- utils::head( list.files( file.path( test_jobpath, test_id, ".job", fsep = "/" ), 
                                         pattern = "^\\d+-action-.*-completed.json$", 
                                         recursive = FALSE, 
                                         full.names = FALSE ) )

  # drop log entry  
  action_details <- jsonlite::fromJSON( file.path( test_jobpath, test_id, ".job", job_action, fsep = "/" ), simplifyDataFrame = FALSE )

  action_details <- action_details[ names(action_details) != "log" ]
  
  base::writeLines( jsonlite::toJSON( action_details ), con = file.path( test_jobpath, test_id, ".job", job_action, fsep = "/" ) )

  
  
  
  # -- test
  
  testthat::expect_error( test_job$save(), 
                          regexp = paste( "^Expecting log details for a program action$" ) )
  
})





testthat::test_that( "batchjob.integrityProgramMissingLogPathEntry", {
  
  
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
  
  
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  # inert test program
  base::writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_wd) + 2 )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_ref ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  
  # - delete log entry 
  
  # identify action
  job_action <- utils::head( list.files( file.path( test_jobpath, test_id, ".job", fsep = "/" ), 
                                         pattern = "^\\d+-action-.*-completed.json$", 
                                         recursive = FALSE, 
                                         full.names = FALSE ) )
  
  # drop log entry  
  action_details <- jsonlite::fromJSON( file.path( test_jobpath, test_id, ".job", job_action, fsep = "/" ), simplifyDataFrame = FALSE )
  
  action_details[["log"]] <- action_details[["log"]][ names(action_details[["log"]]) != "path" ]
  
  base::writeLines( jsonlite::toJSON( action_details ), con = file.path( test_jobpath, test_id, ".job", job_action, fsep = "/" ) )
  
  
  
  
  # -- test
  
  testthat::expect_error( test_job$save(), 
                          regexp = paste( "^Expecting log details for a program action$" ) )
  
})





testthat::test_that( "batchjob.integrityProgramLogPathEntryNA", {
  
  
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
  
  
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  # inert test program
  base::writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_wd) + 2 )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_ref ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  
  # - delete log entry 
  
  # identify action
  job_action <- utils::head( list.files( file.path( test_jobpath, test_id, ".job", fsep = "/" ), 
                                         pattern = "^\\d+-action-.*-completed.json$", 
                                         recursive = FALSE, 
                                         full.names = FALSE ) )
  
  # drop log entry  
  action_details <- jsonlite::fromJSON( file.path( test_jobpath, test_id, ".job", job_action, fsep = "/" ), simplifyDataFrame = FALSE )
  
  action_details[["log"]][["path"]] <- NA
   
  base::writeLines( jsonlite::toJSON( action_details ), con = file.path( test_jobpath, test_id, ".job", job_action, fsep = "/" ) )
  
  
  
  
  # -- test
  
  testthat::expect_error( test_job$save(), 
                          regexp = paste( "^Expecting log file to exist as a result of a program action$" ) )
  
})





testthat::test_that( "batchjob.integrityProgramLogPathEntryNULL", {
  
  
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
  
  
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  # inert test program
  base::writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_wd) + 2 )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_ref ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  
  # - delete log entry 
  
  # identify action
  job_action <- utils::head( list.files( file.path( test_jobpath, test_id, ".job", fsep = "/" ), 
                                         pattern = "^\\d+-action-.*-completed.json$", 
                                         recursive = FALSE, 
                                         full.names = FALSE ) )
  
  # drop log entry  
  action_details <- jsonlite::fromJSON( file.path( test_jobpath, test_id, ".job", job_action, fsep = "/" ), simplifyDataFrame = FALSE )
  
  action_details[["log"]][["path"]] <- NULL
    
  base::writeLines( jsonlite::toJSON( action_details ), con = file.path( test_jobpath, test_id, ".job", job_action, fsep = "/" ) )
  
  
  
  
  # -- test
  
  testthat::expect_error( test_job$save(), 
                          regexp = paste( "^Expecting log details for a program action$" ) )
  
})





testthat::test_that( "batchjob.integrityProgramLogPathFileNotExist", {
  
  
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
  
  
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  # inert test program
  base::writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_wd) + 2 )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_ref ) )
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  
  # - log entry points to non-existent file  
  
  # identify action
  job_action <- utils::head( list.files( file.path( test_jobpath, test_id, ".job", fsep = "/" ), 
                                         pattern = "^\\d+-action-.*-completed.json$", 
                                         recursive = FALSE, 
                                         full.names = FALSE ) )
  
  # drop log entry  
  action_details <- jsonlite::fromJSON( file.path( test_jobpath, test_id, ".job", job_action, fsep = "/" ), simplifyDataFrame = FALSE )
  
  action_details[["log"]][["path"]] <- paste0( tools::file_path_sans_ext( action_details[["log"]][["path"]] ), 
                                               paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 10), collapse = "") )
    
  base::writeLines( jsonlite::toJSON( action_details ), con = file.path( test_jobpath, test_id, ".job", job_action, fsep = "/" ) )
  
  
  
  
  # -- test
  
  testthat::expect_error( test_job$save(), 
                          regexp = paste( "^Expecting log file to exist as a result of a program action$" ) )
  
})





testthat::test_that( "batchjob.integrityProgramConflictWithNewLog", {
  
  
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
  
  
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  # inert test program
  base::writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_wd) + 2 )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_ref ) )
  

  # - stage conflicting log
  
  test_log <- paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout" )
  
  test_log_atsrc <- file.path( test_wd, test_log )
  
  if ( file.exists( test_log_atsrc ) )
    testthat::fail( "Unexpected test log exists" )
  
  base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 40), collapse = ""),
                    con = test_log_atsrc )
  

  # - execute actions
  test_job$submit( wait = TRUE )

  
  
  # -- test
  
  testthat::expect_error( test_job$save(), 
                          regexp = paste( "^A new program log exists while it did not exist when the program action was submitted$" ) )
  
})




testthat::test_that( "batchjob.integrityProgramConflictWithUpdatedLog", {
  
  
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
  
  
  
  
  # - test program
  
  test_program_parent <- file.path( "path", "to", "programs", fsep = "/" )
  
  if ( dir.exists( file.path( test_wd, test_program_parent, fsep = "/" ) ) || ! dir.create( file.path( test_wd, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_wd, test_program_parent, fsep = "/"), 
                                                             fileext = ".R" ) )
  
  # inert test program
  base::writeLines( "# test program", con = test_program )
  
  if ( ! file.exists( test_program ) )
    testthat::fail( "Unexpected test program does not exist" )
  
  
  test_program_ref <- base::substring( test_program, base::nchar(test_wd) + 2 )
  

    
  # - stage initial log
  
  test_log <- paste0( tools::file_path_sans_ext( test_program_ref ), ".Rout" )
  
  test_log_atsrc <- file.path( test_wd, test_log )
  
  if ( file.exists( test_log_atsrc ) )
    testthat::fail( "Unexpected test log exists" )
  
  base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 40), collapse = ""),
                    con = test_log_atsrc )
  
  
  
  # - pre-execution working directory inventory  
  
  test_preinv <- sapply( list.files( test_wd, recursive = TRUE, full.names = FALSE ), function(x) {
    digest::digest( file.path( test_wd, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # - test job id
  test_id <- cxlib:::.cxlib_referenceid( type = "job" )
  
  
  # - stage job  
  test_job <-  cxlib::cxlib_batchjob( list( "id" = test_id, "programs" = test_program_ref ) )
  
  
  # - stage conflicting log
  
  base::writeLines( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 40), collapse = ""),
                    con = test_log_atsrc )
  
  if ( ! file.exists( test_log_atsrc ) )
    testthat::fail( "Test log does not exists" )
  
  
  
  # - execute actions
  test_job$submit( wait = TRUE )
  
  
  
  # -- test
  
  testthat::expect_error( test_job$save(), 
                          regexp = paste( "^The program log in the output location has been updated and could indicate that the program was executed in parallel$" ) )
  
})


