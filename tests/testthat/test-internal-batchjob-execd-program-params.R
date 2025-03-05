#
#  Tests for cxlib:::.cxlib_batchjob_execd_program()
#  
#
#
#


testthat::test_that( "batchjob.execdProgramNoParams", {

  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program(), 
                          regexp = "^Action missing or invalid$" )

})



testthat::test_that( "batchjob.execdProgramActionNull", {
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( NULL ), 
                          regexp = "^Action missing or invalid$" )
  
})



testthat::test_that( "batchjob.execdProgramActionNA", {
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( NA ), 
                          regexp = "^Action missing or invalid$" )
  
})



testthat::test_that( "batchjob.execdProgramActionNotList", {
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( as.character("xyz") ), 
                          regexp = "^Action missing or invalid$" )
  
})



testthat::test_that( "batchjob.execdProgramActionEmptyListMissingOption", {

  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( list() ), 
                          regexp = "^Expected options missing$" )
  
})



testthat::test_that( "batchjob.execdProgramActionEmptyListOptionNULL", {
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( list(), NULL ), 
                          regexp = "^Expected options missing$" )
  
})


testthat::test_that( "batchjob.execdProgramActionEmptyListOptionNA", {
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( list(), NA ), 
                          regexp = "^Expected options missing$" )
  
})



testthat::test_that( "batchjob.execdProgramActionEmptyListOptionNotList", {
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( list(), as.character("xyz") ), 
                          regexp = "^Expected options missing$" )
  
})



testthat::test_that( "batchjob.execdProgramActionEmptyListOptionWorkAreaNotDefined", {
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( list(), list() ), 
                          regexp = "^Work area is not defined or does not exist$" )
  
})



testthat::test_that( "batchjob.execdProgramActionEmptyListOptionWorkAreaNotExist", {
  
  # -- stage
  
  test_workarea <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-area-", tmpdir = base::tempdir(), fileext = "") )
  
  if ( dir.exists(test_workarea) || file.exists(test_workarea) )
    testthat::fail( "Unexpected work area exists" )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( list(), list( "work.area" = test_workarea ) ), 
                          regexp = "^Work area is not defined or does not exist$" )
  
})




testthat::test_that( "batchjob.execdProgramActionEmptyListOptionJobIDMissing", {
  
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
  

  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( list(), list( "work.area" = test_workarea ) ), 
                          regexp = "^Job is not identifiable$" )
  
})



testthat::test_that( "batchjob.execdProgramActionEmptyListOptionJobIDNULL", {
  
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
  
  
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = NULL )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( list(), test_opts ), 
                          regexp = "^Job is not identifiable$" )
  
})



testthat::test_that( "batchjob.execdProgramActionEmptyListOptionJobIDNotCharacter", {
  
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
  
  
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = list("abc" = "def") )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( list(), test_opts ), 
                          regexp = "^Job is not identifiable$" )
  
})




testthat::test_that( "batchjob.execdProgramActionEmptyListOptionJobIDNotUUIDFormat", {
  
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
  
  #   note: job ID is right length but not in a UUID format
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), base::nchar(uuid::UUIDgenerate()) ), collapse = "" ) )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( list(), test_opts ), 
                          regexp = "^Job is not identifiable$" )
  
})



testthat::test_that( "batchjob.execdProgramActionMissingActionID", {
  
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
  
  
  
  # -- test action
  test_action <- list()
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^Action ID is missing or in an invalid format$" )
  
})


testthat::test_that( "batchjob.execdProgramActionMissingActionIDInvalidFormat", {
  
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
  
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid( type = "raw") ) 
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^Action ID is missing or in an invalid format$" )
  
})





testthat::test_that( "batchjob.execdProgramActionMissingType", {
  
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
  
  

  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid() ) 
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
 
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^The submitted action is not a program action$" )
  
})



testthat::test_that( "batchjob.execdProgramActionTypeNotEqualProgram", {
  
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
  
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(), 
                       "type" = paste( sample( c( base::LETTERS, base::letters, as.character( 0:9) ), 20 ), collapse = "" ) )
  
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^The submitted action is not a program action$" )
  
})



testthat::test_that( "batchjob.execdProgramActionTypeProgramPathMissing", {
  
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
  
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(), 
                       "type" = "program" )
  
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^The program path missing from program action$" )
  
})




testthat::test_that( "batchjob.execdProgramActionTypeProgramPathMissing", {
  
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
  
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(),
                       "type" = "program" )
  
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^The program path missing from program action$" )
  
})





testthat::test_that( "batchjob.execdProgramActionTypeProgramPathNotRProgram", {
  
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
  
  

  # - test program path
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-area-", tmpdir = test_root, fileext = ".txt") )
  
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(),
                       "type" = "program", 
                       "path" = test_program )
  
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ),
                          regexp = "^The program file extension not of a supported program type$" )
  
})



testthat::test_that( "batchjob.execdProgramActionTypeProgramPathRProgramNotExist", {
  
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
  
  
  
  # - test program path
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-work-area-", tmpdir = test_root, fileext = ".R") )
  
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(),
                       "type" = "program", 
                       "path" = test_program )
  
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^Program does not exist$" )
  
})




testthat::test_that( "batchjob.execdProgramActionTypeProgramPathRProgramExistMissingHash", {
  
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
  
  
  
  # - test program
  
  test_program_parent <- "path/to/programs"
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                             fileext = ".R") )
  
  base::writeLines( "# test program", con = test_program )
  

  test_program_ref <- base::substring( test_program, base::nchar( test_workarea ) + 2 )
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(),
                       "type" = "program", 
                       "path" = test_program_ref )
  
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^Program could not be verified or has changed since action was defined$" )
  
})




testthat::test_that( "batchjob.execdProgramActionTypeProgramPathRProgramExistHashNotEqual", {
  
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
  
  
  
  # - test program
  
  test_program_parent <- "path/to/programs"
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                             fileext = ".R") )
  
  base::writeLines( "# test program", con = test_program )
  
  
  test_program_ref <- base::substring( test_program, base::nchar( test_workarea ) + 2 )
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(),
                       "type" = "program", 
                       "path" = test_program_ref, 
                       "sha1" = digest::digest( paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 40 ), collapse = "" ), algo = "sha1", file = FALSE ) )
  
  if ( digest::digest( test_program, algo = "sha1", file = TRUE ) == test_action[["sha1"]] )
    testthat::fail( "Could not generate a program with SHA-1 different from random string" )
  
    
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^Program could not be verified or has changed since action was defined$" )
  
})




testthat::test_that( "batchjob.execdProgramActionTypeProgramPathRProgramMissingLogDef", {
  
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
  
  
  
  # - test program
  
  test_program_parent <- "path/to/programs"
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                             fileext = ".R") )
  
  base::writeLines( "# test program", con = test_program )
  
  
  test_program_ref <- base::substring( test_program, base::nchar( test_workarea ) + 2 )
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(),
                       "type" = "program", 
                       "path" = test_program_ref, 
                       "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ) )
  

  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^Log path is not defined$" )
  
})




testthat::test_that( "batchjob.execdProgramActionTypeProgramPathRProgramMissingLogPath", {
  
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
  
  
  
  # - test program
  
  test_program_parent <- "path/to/programs"
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                             fileext = ".R") )
  
  base::writeLines( "# test program", con = test_program )
  
  
  test_program_ref <- base::substring( test_program, base::nchar( test_workarea ) + 2 )
  
  
  # -- test action
  test_action <- list( "id" = cxlib:::.cxlib_referenceid(),
                       "type" = "program", 
                       "path" = test_program_ref, 
                       "sha1" = digest::digest( test_program, algo = "sha1", file = TRUE ),
                       "log" = list() )
  
  
  # -- test options
  test_opts <- list( "work.area" = test_workarea, 
                     "job.id" = uuid::UUIDgenerate() )
  
  
  # -- test
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^Log path is not defined$" )
  
})




testthat::test_that( "batchjob.execdProgramActionTypeProgramPathRProgramMissingLogPathDirNotExist", {
  
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
  
  
  
  # - test program
  
  test_program_parent <- "path/to/programs"
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                             fileext = ".R") )
  
  base::writeLines( "# test program", con = test_program )
  
  
  test_program_ref <- base::substring( test_program, base::nchar( test_workarea ) + 2 )
  
  
  # - test log
  
  test_log_parent <- "path/to/logs"
  
  if ( dir.exists( file.path( test_workarea, test_log_parent, fsep = "/") ) )
    testthat::fail( "Unexpected test log directory exists" )

  test_log <- file.path( test_log_parent, "any.log", fsep = "/" )
    

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
  
  testthat::expect_error( cxlib:::.cxlib_batchjob_execd_program( test_action, test_opts ), 
                          regexp = "^Log directory does not exist$" )
  
})



testthat::test_that( "batchjob.execdProgramActionRProgram", {
  
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
  
  
  
  # - test program
  
  test_program_parent <- "path/to/programs"
  
  if ( dir.exists( file.path( test_workarea, test_program_parent, fsep = "/" ) ) ||
       ! dir.create( file.path( test_workarea, test_program_parent, fsep = "/" ), recursive = TRUE ) )
    testthat::fail( "Could not stage test program parent directory" )
  
  test_program <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-program-", 
                                                             tmpdir = file.path( test_workarea, test_program_parent, fsep = "/" ), 
                                                             fileext = ".R") )
  
  base::writeLines( "# test program", con = test_program )
  
  
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
  
  # - add inputs
  expected_results <- append( expected_results, 
                              list( "files.input" = list( test_action[ c( "path", "sha1") ] ), 
                                    "files.created" = list( expected_results[["log"]] ),
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



