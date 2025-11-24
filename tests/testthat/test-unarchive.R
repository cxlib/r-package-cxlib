#
#
# Tests for cxlib un-archive
#
#


#' @cx.testsfor cxlib::cxlib_unarchive()



testthat::test_that( "unarchive.noParms", {
  
  #' @cx.tests Not specifying an archive file results in an error
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive(), regexp = "^Zip archive file not specified or an invalid value$" )
  
})



testthat::test_that( "unarchive.archiveFileNull", {
  
  #' @cx.tests Specifying an archive file equal to NULL results in an error
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( NULL ), regexp = "^Zip archive file not specified or an invalid value$" )
  
})


testthat::test_that( "unarchive.archiveFileNA", {
  
  #' @cx.tests Specifying an archive file equal to NA results in an error
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( NA ), regexp = "^Zip archive file not specified or an invalid value$" )
  
})



testthat::test_that( "unarchive.archiveFileEmptyString", {
  
  #' @cx.tests Specifying an archive file equal as an empty string results in an error
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( "  " ), regexp = "^Zip archive file not specified or an invalid value$" )
  
})



testthat::test_that( "unarchive.archiveFileNotExist", {
  
  #' @cx.tests Specifying an archive file that does not exist results in an error
  
  
  # -- stage 

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  if ( file.exists(test_archive) )
    testthat::fail( "Unexpected test archive exists" )
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( test_archive ), regexp = "^The Zip archive does not exist$" )
  
})




testthat::test_that( "unarchive.archiveExtDirNull", {
  
  #' @cx.tests Specifying an archive extract directory equal to NULL results in an error
  
  
  # -- stage 

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  base::writeLines( "some random string", con = test_archive)

  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( test_archive, extract.dir = NULL ), regexp = "^Zip archive extract target directory not specified, an invalid value or does not exist$" )
  
})




testthat::test_that( "unarchive.archiveExtDirNA", {
  
  #' @cx.tests Specifying an archive extract directory equal to NA results in an error
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  base::writeLines( "some random string", con = test_archive)
  
  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( test_archive, extract.dir = NA ), regexp = "^Zip archive extract target directory not specified, an invalid value or does not exist$" )
  
})




testthat::test_that( "unarchive.archiveExtDirEmptyString", {
  
  #' @cx.tests Specifying an archive extract directory equal to an empty string results in an error
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  base::writeLines( "some random string", con = test_archive)
  
  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( test_archive, extract.dir = "   " ), regexp = "^Zip archive extract target directory not specified, an invalid value or does not exist$" )
  
})




testthat::test_that( "unarchive.archiveExtDirNotExist", {
  
  #' @cx.tests Specifying an archive extract directory that does not exist results in an error
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  base::writeLines( "some random string", con = test_archive)
  
  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  
  
  # - extract directory
  
  test_extdir <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-extdir-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_extdir) )
    testthat::fail( "Unexpeted test extract directory exists" )
  

  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( test_archive, extract.dir = test_extdir ), regexp = "^Zip archive extract target directory not specified, an invalid value or does not exist$" )
  
})





testthat::test_that( "unarchive.archiveNotZipArchiveFails", {
  
  #' @cx.tests Specifying an archive file that is not a Zip archive file results in an error
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  base::writeLines( "some random string", con = test_archive)
  
  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  
  
  # - extract directory
  
  test_extdir <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-extdir-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_extdir ) && ! dir.create( test_extdir, recursive = TRUE ) )
    testthat::fail("Could note stage test extract directory")
  

  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( test_archive, extract.dir = test_extdir ), 
                          regexp = paste0( "^Could not read archive file ", test_archive , "$" ) )
  
})



testthat::test_that( "unarchive.emptyArchive", {
  
  #' @cx.tests Specifying an empty archive file results in no files or directories in extract directory
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test content
  
  test_src<- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-source-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_src ) && ! dir.create( test_src, recursive = TRUE ) )
    testthat::fail("Could note stage test source directory")
  
  

  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  zip::zip( test_archive, character(0), root = test_src, mode = "mirror" )

  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  

  # - extract directory
  
  test_extdir <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-extdir-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_extdir ) && ! dir.create( test_extdir, recursive = TRUE ) )
    testthat::fail("Could note stage test extract directory")
  
  
  # -- test
  result <- cxlib::cxlib_unarchive( test_archive, extract.dir = test_extdir )
  
  
  # -- expected
  
  # - test extraction directory
  #   note: list.dirs() returns root directory when recursive = TRUE
  expected_extdirs <- test_extdir
  
  
  
  # -- assertions
  
  # - result returns no entries
  testthat::expect_true( inherits( result, "character") )
  testthat::expect_length( result, 0 )
  
  # - no files in extract directory
  testthat::expect_length( list.files( test_extdir, recursive = TRUE, include.dirs = FALSE), 0 )
  
  # - no directories in extract directory
  #   note: list.dirs() returns root directory when recursive = TRUE
  testthat::expect_equal( list.dirs( test_extdir, recursive = TRUE, full.names = TRUE ), expected_extdirs )

})






testthat::test_that( "unarchive.archiveMissingDigests", {
  
  #' @cx.tests Specifying an archive file with missing SHA-1 and MD-5 digests results in an error
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test content
  
  test_src <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-source-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_src ) && ! dir.create( test_src, recursive = TRUE ) )
    testthat::fail("Could note stage test source directory")
  
  base::writeLines( paste( sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(50:1024, 1), replace = TRUE ), collapse = "" ),
                    con = cxapp::cxapp_standardpath( base::tempfile( pattern = "test-file-", tmpdir = test_src, fileext = ".txt" ) ) ) 

  test_srcfiles <- list.files( test_src, recursive = TRUE, full.names = FALSE, include.dirs = FALSE )
  


  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  zip::zip( test_archive, list.files( test_src, recursive = TRUE, full.names = FALSE, include.dirs = FALSE), root = test_src, mode = "mirror" )
  
  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  
  
  # - extract directory
  
  test_extdir <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-extdir-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_extdir ) && ! dir.create( test_extdir, recursive = TRUE ) )
    testthat::fail("Could note stage test extract directory")
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( test_archive, extract.dir = test_extdir ), regexp = "^SHA-1 or MD-5 digests for archive not avialble$" )
  
})




testthat::test_that( "unarchive.archiveFilesOnly", {
  
  #' @cx.tests Extracting an archive file with files only
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test content directories
  
  test_src <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-source-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_src ) && ! dir.create( test_src, recursive = TRUE ) )
    testthat::fail("Could note stage test source directory")
  
  test_subdirs <- replicate( 5, 
                             paste( sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(10:50, 1), replace = TRUE ), collapse = "" ), 
                             simplify = TRUE )
  
  for ( xpath in test_subdirs )
    if ( ! dir.exists(file.path( test_src, xpath, fsep = "/")) && ! dir.create( file.path( test_src, xpath, fsep = "/"), recursive = TRUE) )
      testthat::fail( "Could not stage source subdirectory" )


  # - test content files  
    
  test_files <- replicate( 2, 
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "test-file-", 
                                                                      tmpdir = file.path( test_src, sample( test_subdirs, 1), fsep = "/"), 
                                                                      fileext = ".txt" ) ), 
                           simplify = TRUE )
  

  for ( xsrcfile in test_files )
    base::writeLines( paste( sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(50:1024, 1), replace = TRUE ), collapse = "" ),
                      con = xsrcfile ) 
  
  test_srcfiles <- list.files( test_src, recursive = TRUE, full.names = FALSE, include.dirs = FALSE )
  

  
  
  # - test content digests
  
  test_shadigests <- base::unlist(sapply( base::sort(test_srcfiles), function(x) {
    digest::digest( file.path( test_src, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  
  base::writeLines(
    base::unlist(lapply( test_srcfiles, function(x) {
      paste0( test_shadigests[ x ], "  ", x )
    })), 
    con = file.path( test_src, "sha", fsep = "/" )
  )
  
  
  test_md5digests <- base::unlist(sapply( base::sort(test_srcfiles), function(x) {
    digest::digest( file.path( test_src, x, fsep = "/"), algo = "md5", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  base::writeLines(
    base::unlist(lapply( test_srcfiles, function(x) {
      paste0( test_md5digests[ x ], "  ", x )
    })), 
    con = file.path( test_src, "md5", fsep = "/" )
  )
  
  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  zip::zip( test_archive, list.files( test_src, recursive = TRUE, full.names = FALSE, include.dirs = FALSE), root = test_src, mode = "mirror" )
  
  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  
  

  
  # - extract directory
  
  test_extdir <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-extdir-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_extdir ) && ! dir.create( test_extdir, recursive = TRUE ) )
    testthat::fail("Could note stage test extract directory")
  
  
  # -- test
  result <- cxlib::cxlib_unarchive( test_archive, extract.dir = test_extdir )


  
  # -- expected
  
  # - files extracted
  expected_files <- test_srcfiles
  
  
  # - expected SHA-1
  expected_sha <-test_shadigests
  
  # - expected MD-5
  expected_md5 <- test_md5digests
  
  
  # -- assertions
  
  # - result
  testthat::expect_equal( base::sort(result), base::sort(expected_files))
  
  # - extracted files
  testthat::expect_equal( base::sort(list.files( test_extdir, recursive = TRUE, full.names = FALSE, include.dirs = FALSE)), base::sort(expected_files))
  
  
  # - extracted files SHA-1
  act_sha <- base::unlist(sapply( expected_files, function(x) {
    digest::digest( file.path( test_extdir, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  testthat::expect_equal( act_sha[ base::sort(base::names(act_sha))], expected_sha[ base::sort(base::names(expected_sha)) ] )
  
  
  # - extracted files MD-5
  act_md5 <- base::unlist(sapply( expected_files, function(x) {
    digest::digest( file.path( test_extdir, x, fsep = "/"), algo = "md5", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  testthat::expect_equal( act_md5[ base::sort(base::names(act_md5))], expected_md5[ base::sort(base::names(expected_md5)) ] )
  
  
})




testthat::test_that( "unarchive.archiveFilesOnlySHAMissmatch", {
  
  #' @cx.tests Extracting an archive file with SHA-1 digest mismatch fails with data integrity error
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test content directories
  
  test_src <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-source-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_src ) && ! dir.create( test_src, recursive = TRUE ) )
    testthat::fail("Could note stage test source directory")
  
  test_subdirs <- replicate( 5, 
                             paste( sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(10:50, 1), replace = TRUE ), collapse = "" ), 
                             simplify = TRUE )
  
  for ( xpath in test_subdirs )
    if ( ! dir.exists(file.path( test_src, xpath, fsep = "/")) && ! dir.create( file.path( test_src, xpath, fsep = "/"), recursive = TRUE) )
      testthat::fail( "Could not stage source subdirectory" )
  
  
  # - test content files  
  
  test_files <- replicate( 2, 
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "test-file-", 
                                                                      tmpdir = file.path( test_src, sample( test_subdirs, 1), fsep = "/"), 
                                                                      fileext = ".txt" ) ), 
                           simplify = TRUE )
  
  
  for ( xsrcfile in test_files )
    base::writeLines( paste( sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(50:1024, 1), replace = TRUE ), collapse = "" ),
                      con = xsrcfile ) 
  
  test_srcfiles <- list.files( test_src, recursive = TRUE, full.names = FALSE, include.dirs = FALSE )
  
  
  
  
  # - test content digests
  
  test_shadigests <- base::unlist(sapply( base::sort(test_srcfiles), function(x) {
    digest::digest( file.path( test_src, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  
  test_shafailfile <- sample( test_srcfiles, 1 )
  
  corrupt_shadigests <- test_shadigests
  corrupt_shadigests[ test_shafailfile ] <- digest::digest( paste( c( "corrupted-", 
                                                                      sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(50:1024, 1), replace = TRUE )), collapse = "" ),  
                                                            algo = "sha1", file = FALSE )

  base::writeLines(
    base::unlist(lapply( test_srcfiles, function(x) {
      paste0( corrupt_shadigests[ x ], "  ", x )
    })), 
    con = file.path( test_src, "sha", fsep = "/" )
  )
  
  
  test_md5digests <- base::unlist(sapply( base::sort(test_srcfiles), function(x) {
    digest::digest( file.path( test_src, x, fsep = "/"), algo = "md5", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  base::writeLines(
    base::unlist(lapply( test_srcfiles, function(x) {
      paste0( test_md5digests[ x ], "  ", x )
    })), 
    con = file.path( test_src, "md5", fsep = "/" )
  )
  
  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  zip::zip( test_archive, list.files( test_src, recursive = TRUE, full.names = FALSE, include.dirs = FALSE), root = test_src, mode = "mirror" )
  
  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  
  
  
  
  # - extract directory
  
  test_extdir <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-extdir-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_extdir ) && ! dir.create( test_extdir, recursive = TRUE ) )
    testthat::fail("Could note stage test extract directory")
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( test_archive, extract.dir = test_extdir ), regexp = "^SHA-1 integrity check failed for an archive file$" )
  

  # -- assertions
  
  # - extracted files
  testthat::expect_length( list.files( test_extdir, recursive = TRUE, full.names = FALSE, include.dirs = FALSE), 0 )
  
  
})





testthat::test_that( "unarchive.archiveFilesOnlyMD5Missmatch", {
  
  #' @cx.tests Extracting an archive file with MD-5 digest mismatch fails with data integrity error
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test content directories
  
  test_src <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-source-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_src ) && ! dir.create( test_src, recursive = TRUE ) )
    testthat::fail("Could note stage test source directory")
  
  test_subdirs <- replicate( 5, 
                             paste( sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(10:50, 1), replace = TRUE ), collapse = "" ), 
                             simplify = TRUE )
  
  for ( xpath in test_subdirs )
    if ( ! dir.exists(file.path( test_src, xpath, fsep = "/")) && ! dir.create( file.path( test_src, xpath, fsep = "/"), recursive = TRUE) )
      testthat::fail( "Could not stage source subdirectory" )
  
  
  # - test content files  
  
  test_files <- replicate( 2, 
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "test-file-", 
                                                                      tmpdir = file.path( test_src, sample( test_subdirs, 1), fsep = "/"), 
                                                                      fileext = ".txt" ) ), 
                           simplify = TRUE )
  
  
  for ( xsrcfile in test_files )
    base::writeLines( paste( sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(50:1024, 1), replace = TRUE ), collapse = "" ),
                      con = xsrcfile ) 
  
  test_srcfiles <- list.files( test_src, recursive = TRUE, full.names = FALSE, include.dirs = FALSE )
  
  
  
  
  # - test content digests
  
  test_shadigests <- base::unlist(sapply( base::sort(test_srcfiles), function(x) {
    digest::digest( file.path( test_src, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  
  base::writeLines(
    base::unlist(lapply( test_srcfiles, function(x) {
      paste0( test_shadigests[ x ], "  ", x )
    })), 
    con = file.path( test_src, "sha", fsep = "/" )
  )
  
  
  test_md5digests <- base::unlist(sapply( base::sort(test_srcfiles), function(x) {
    digest::digest( file.path( test_src, x, fsep = "/"), algo = "md5", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  
  test_md5failfile <- sample( test_srcfiles, 1 )
  
  corrupt_md5digests <- test_md5digests
  corrupt_md5digests[ test_md5failfile ] <- digest::digest( paste( c( "corrupted-", 
                                                                      sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(50:1024, 1), replace = TRUE )), collapse = "" ),  
                                                            algo = "md5", file = FALSE )

  base::writeLines(
    base::unlist(lapply( test_srcfiles, function(x) {
      paste0( corrupt_md5digests[ x ], "  ", x )
    })), 
    con = file.path( test_src, "md5", fsep = "/" )
  )
  
  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  zip::zip( test_archive, list.files( test_src, recursive = TRUE, full.names = FALSE, include.dirs = FALSE), root = test_src, mode = "mirror" )
  
  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  
  
  
  
  # - extract directory
  
  test_extdir <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-extdir-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_extdir ) && ! dir.create( test_extdir, recursive = TRUE ) )
    testthat::fail("Could note stage test extract directory")
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_unarchive( test_archive, extract.dir = test_extdir ), regexp = "^MD-5 integrity check failed for an archive file$" )
  
  
  # -- assertions
  
  # - extracted files
  testthat::expect_length( list.files( test_extdir, recursive = TRUE, full.names = FALSE, include.dirs = FALSE), 0 )
  
  
})





testthat::test_that( "unarchive.archiveFilesWithDirectories", {
  
  #' @cx.tests Extracting an archive file with files and a specified empty directory structure
  
  
  # -- stage 
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test content directories
  
  test_src <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-source-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_src ) && ! dir.create( test_src, recursive = TRUE ) )
    testthat::fail("Could note stage test source directory")
  
  test_subdirs <- replicate( 5, 
                             paste( sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(10:50, 1), replace = TRUE ), collapse = "" ), 
                             simplify = TRUE )
  
  for ( xpath in test_subdirs )
    if ( ! dir.exists(file.path( test_src, xpath, fsep = "/")) && ! dir.create( file.path( test_src, xpath, fsep = "/"), recursive = TRUE) )
      testthat::fail( "Could not stage source subdirectory" )
  
  
  # - test content files  
  
  test_files <- replicate( 2, 
                           cxapp::cxapp_standardpath( base::tempfile( pattern = "test-file-", 
                                                                      tmpdir = file.path( test_src, sample( test_subdirs, 1), fsep = "/"), 
                                                                      fileext = ".txt" ) ), 
                           simplify = TRUE )
  
  
  for ( xsrcfile in test_files )
    base::writeLines( paste( sample( c( base::letters, base::LETTERS, as.character(0:9) ), sample(50:1024, 1), replace = TRUE ), collapse = "" ),
                      con = xsrcfile ) 
  
  test_srcfiles <- list.files( test_src, recursive = TRUE, full.names = FALSE, include.dirs = FALSE )
  
  
  
  
  # - test content digests
  
  test_shadigests <- base::unlist(sapply( base::sort(test_srcfiles), function(x) {
    digest::digest( file.path( test_src, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  
  base::writeLines(
    base::unlist(lapply( test_srcfiles, function(x) {
      paste0( test_shadigests[ x ], "  ", x )
    })), 
    con = file.path( test_src, "sha", fsep = "/" )
  )
  
  
  test_md5digests <- base::unlist(sapply( base::sort(test_srcfiles), function(x) {
    digest::digest( file.path( test_src, x, fsep = "/"), algo = "md5", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  base::writeLines(
    base::unlist(lapply( test_srcfiles, function(x) {
      paste0( test_md5digests[ x ], "  ", x )
    })), 
    con = file.path( test_src, "md5", fsep = "/" )
  )
  
  
  # - test empty directories
  
  test_emptydirs <- replicate( 2, 
                               cxapp::cxapp_standardpath( base::tempfile( pattern = "test-dir-", 
                                                                          tmpdir = file.path( test_src, sample( test_subdirs, 1), fsep = "/"), 
                                                                          fileext = "" ) ), 
                               simplify = TRUE )
  
  
  test_emptydirs <- append( test_emptydirs, 
                            replicate( 3, 
                                       cxapp::cxapp_standardpath( base::tempfile( pattern = "test-dir-", 
                                                                                  tmpdir = test_src, 
                                                                                  fileext = "" ) ), 
                                       simplify = TRUE ) )  
  
  if ( any( dir.exists(test_emptydirs) ) )
    testthat::fail( "Could not generate non-existent empty directories" )
  
  
  for ( xpath in test_emptydirs )
    if ( ! dir.exists(xpath) && ! dir.create( xpath, recursive = TRUE) )
      testthat::fail( "Could not stage empty directories" )
    
  test_emptydirs_ref <- base::substring( test_emptydirs, base::nchar(test_src) + 2 )
  

  
  # - test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  zip::zip( test_archive, 
            c( list.files( test_src, recursive = TRUE, full.names = FALSE, include.dirs = FALSE), 
               test_emptydirs_ref),
            include_directories = TRUE,
            root = test_src, 
            mode = "mirror" )
  
  if ( ! file.exists(test_archive) )
    testthat::fail( "Could not stage test archive" )
  
  #print(zip::zip_list( test_archive))
  
  
  # - extract directory
  
  test_extdir <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-extdir-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_extdir ) && ! dir.create( test_extdir, recursive = TRUE ) )
    testthat::fail("Could note stage test extract directory")
  
  
  # -- test
  result <- cxlib::cxlib_unarchive( test_archive, extract.dir = test_extdir )
  
  
  
  # -- expected
  
  # - files extracted
  expected_files <- test_srcfiles
  
  # - directories extracted
  expected_dirs <- character(0)
  
  for ( expdir in base::sort(c( base::dirname(expected_files), test_emptydirs_ref ), decreasing = TRUE ) )
    if ( (length(expected_dirs) == 0) || all( ! base::startsWith( expected_dirs, paste0( expdir, "/")) ) )
      expected_dirs <- base::unique(append( expected_dirs, expdir ))
  
  #print(expected_dirs)
  
  # - expected SHA-1
  expected_sha <-test_shadigests
  
  # - expected MD-5
  expected_md5 <- test_md5digests
  
  
  # -- assertions
  
  # - result
  testthat::expect_equal( base::sort(result), base::sort(expected_files))
  
  # - extracted files
  testthat::expect_equal( base::sort(list.files( test_extdir, recursive = TRUE, full.names = FALSE, include.dirs = FALSE)), base::sort(expected_files))
  
  # - extracted dirs
  act_dirs <- character(0)
  
  for ( xdir in base::sort(list.dirs( test_extdir, recursive = TRUE, full.names = FALSE), decreasing = TRUE ) )
    if ( (length(act_dirs) == 0) || all( ! base::startsWith( act_dirs, paste0( xdir, "/")) ) )
      act_dirs <- base::unique(append( act_dirs, xdir ))

  # note: list.dirs(..., recursive = TRUE, full.names = FALSE) includes the root directory as ""
  testthat::expect_equal( base::sort(act_dirs[ base::trimws(act_dirs) != ""]), base::sort(expected_dirs) )
  
  # - extracted files SHA-1
  act_sha <- base::unlist(sapply( expected_files, function(x) {
    digest::digest( file.path( test_extdir, x, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  testthat::expect_equal( act_sha[ base::sort(base::names(act_sha))], expected_sha[ base::sort(base::names(expected_sha)) ] )
  
  
  # - extracted files MD-5
  act_md5 <- base::unlist(sapply( expected_files, function(x) {
    digest::digest( file.path( test_extdir, x, fsep = "/"), algo = "md5", file = TRUE )
  }, USE.NAMES = TRUE), use.names = TRUE )
  
  testthat::expect_equal( act_md5[ base::sort(base::names(act_md5))], expected_md5[ base::sort(base::names(expected_md5)) ] )
  
  
})



