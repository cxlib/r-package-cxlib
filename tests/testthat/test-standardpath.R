#
#  Tests for cxlib::cxlib_standardpath
#
#
#

testthat::test_that( "stdpath.noParms", {
  
  # -- test 
  
  testthat::expect_error( cxlib::cxlib_standardpath(), regexp = "^argument \"x\" is missing, with no default$" )
  
})




testthat::test_that( "stdpath.empty", {
  
  
  # -- test
  
  result <- cxlib::cxlib_standardpath( character(0) )
  
  
  # -- assertions
  testthat::expect_true( inherits( result, "character" ) )
  testthat::expect_length( result, 0 )
  
})




testthat::test_that( "stdpath.emptyNormalize", {
  
  
  # -- test
  
  result <- cxlib::cxlib_standardpath( character(0), normalize = TRUE )
  
  
  # -- assertions
  testthat::expect_true( inherits( result, "character" ) )
  testthat::expect_length( result, 0 )
  
})




testthat::test_that( "stdpath.singlePathForwardSlash", {
  
  # -- stage 
  
  test_path <- paste( base::replicate( 10, 
                                       paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 15 ), collapse = "" ), 
                                       simplify = TRUE ), 
                      collapse = "/" )
    
  
  # -- test
  
  result <- cxlib::cxlib_standardpath( test_path )
  

  # -- expected
  
  expected_path <- test_path
  
    
  # -- assertions
  testthat::expect_equal( result, expected_path )

})




testthat::test_that( "stdpath.singlePathBackwardSlash", {
  
  # -- stage 
  
  test_path <- paste( base::replicate( 10, 
                                       paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 15 ), collapse = "" ), 
                                       simplify = TRUE ), 
                      collapse = "\\" )
  
  
  # -- test
  
  result <- cxlib::cxlib_standardpath( test_path )
  
  
  # -- expected
  
  expected_path <- base::gsub( "\\\\", "/", test_path )
  
  
  # -- assertions
  testthat::expect_equal( result, expected_path )
  
})



testthat::test_that( "stdpath.singlePathBackwardSlashOverload", {
  
  # -- stage 
  
  test_path <- paste( base::replicate( 10, 
                                       paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 15 ), collapse = "" ), 
                                       simplify = TRUE ), 
                      collapse = "\\\\" )
  
  
  # -- test
  
  result <- cxlib::cxlib_standardpath( test_path )
  
  
  # -- expected
  
  expected_path <- base::gsub( "/{2,}", "/", base::gsub( "\\\\", "/", test_path ) )
  
  
  # -- assertions
  testthat::expect_equal( result, expected_path )
  
})




testthat::test_that( "stdpath.multiplePaths", {
  
  # -- stage 
  
  test_paths_forward <- replicate( 5, paste( base::replicate( 10, 
                                                              paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 15 ), collapse = "" ), 
                                                              simplify = TRUE ), 
                                             collapse = "/" ), 
                                   simplify = TRUE )
  
  test_paths_backward <- replicate( 5, paste( base::replicate( 10, 
                                                               paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 15 ), collapse = "" ), 
                                                               simplify = TRUE ), 
                                              collapse = "\\" ), 
                                    simplify = TRUE )
  
  test_paths <- append( test_paths_forward, test_paths_backward )
  
  
  # -- test
  
  result <- cxlib::cxlib_standardpath( test_paths )
  
  
  # -- expected
  
  expected_paths <- base::gsub( "/{2,}", "/", base::gsub( "\\\\", "/", test_paths ) )
  
  
  # -- assertions
  testthat::expect_equal( result, expected_paths )
  
})





testthat::test_that( "stdpath.normalizeDirectory", {

  # -- stage
  
  test_root <- gsub( "\\\\", "/", base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit( {
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )


  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not create test area" )
  
  
  # - set up target directory
  
  test_trgt <- gsub( "\\\\", "/", base::tempfile( pattern = "test-target-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_trgt ) && ! dir.create( test_trgt, recursive = TRUE ) )
    testthat::fail( "Could not stage target directory in test area" )
  
  
  # - set up linked directory
  test_lnk <- gsub( "\\\\", "/", base::tempfile( pattern = "test-link-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_lnk ) && ! file.symlink( test_trgt, test_lnk ) )
    testthat::fail( "Could not establish link" )
  
  
  # -- test
  
  result <- cxlib::cxlib_standardpath( test_lnk, normalize = TRUE )

  
  # -- expected
  
  expected_path <- test_trgt
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_path )
  testthat::expect_true( dir.exists( result ) )
  

})




testthat::test_that( "stdpath.normalizeDirectoryNotExist", {
  
  # -- stage
  
  test_root <- gsub( "\\\\", "/", base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit( {
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not create test area" )
  
  
  # - set up target directory
  
  test_trgt <- gsub( "\\\\", "/", base::tempfile( pattern = "test-target-", tmpdir = test_root, fileext = "" ) )

  if ( dir.exists( test_trgt ) )
    testthat::fail( "Unexpected test directory exists" )
  
  
  # - set up linked directory
  test_lnk <- gsub( "\\\\", "/", base::tempfile( pattern = "test-link-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_lnk ) && ! file.symlink( test_trgt, test_lnk ) )
    testthat::fail( "Could not establish link" )
  
  
  # -- test
  
  result <- base::suppressWarnings( cxlib::cxlib_standardpath( test_lnk, normalize = TRUE ) )
  
  
  # -- expected
  
  expected_path <- test_lnk
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_path )
  testthat::expect_false( dir.exists( result ) )
  
  
})



testthat::test_that( "stdpath.normalizeFile", {
  
  # -- stage
  
  test_root <- gsub( "\\\\", "/", base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit( {
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not create test area" )
  
  
  # - set up target directory
  
  test_trgt_parent <- gsub( "\\\\", "/", base::tempfile( pattern = "test-target-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_trgt_parent ) && ! dir.create( test_trgt_parent, recursive = TRUE ) )
    testthat::fail( "Could not stage target directory in test area" )
  
  
  test_trgt <- gsub( "\\\\", "/", base::tempfile( pattern = "test-file-", tmpdir = test_trgt_parent, fileext = ".txt" ) )
  
  base::writeLines( "# test file", con = test_trgt )
  
  if ( ! file.exists( test_trgt ) )
    testthat::fail( "Test target file could not be created" )
  
  
  
  # - set up linked directory
  test_lnk <- gsub( "\\\\", "/", base::tempfile( pattern = "test-link-", tmpdir = test_root, fileext = ".txt" ) )
  
  if ( ! dir.exists( test_lnk ) && ! file.symlink( test_trgt, test_lnk ) )
    testthat::fail( "Could not establish link" )
  
  
  # -- test
  
  result <- cxlib::cxlib_standardpath( test_lnk, normalize = TRUE )
  
  
  # -- expected
  
  expected_path <- test_trgt
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_path )
  testthat::expect_false( dir.exists( result ) )
  testthat::expect_true( file.exists( result ) )
  
  
})



testthat::test_that( "stdpath.normalizeFileNotExist", {
  
  # -- stage
  
  test_root <- gsub( "\\\\", "/", base::tempfile( pattern = "test-root-", tmpdir = base::tempdir(), fileext = "" ) )
  
  on.exit( {
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail( "Could not create test area" )
  
  
  # - set up target directory
  
  test_trgt_parent <- gsub( "\\\\", "/", base::tempfile( pattern = "test-target-", tmpdir = test_root, fileext = "" ) )
  test_trgt <- gsub( "\\\\", "/", base::tempfile( pattern = "test-file-", tmpdir = test_trgt_parent, fileext = ".txt" ) )
  

  if ( file.exists( test_trgt) )
    testthat::fail( "Unexpected test file exists" )
  
  
  # - set up linked directory
  test_lnk <- gsub( "\\\\", "/", base::tempfile( pattern = "test-link-", tmpdir = test_root, fileext = ".txt" ) )
  
  if ( ! dir.exists( test_lnk ) && ! file.symlink( test_trgt, test_lnk ) )
    testthat::fail( "Could not establish link" )
  
  
  # -- test
  
  result <- base::suppressWarnings( cxlib::cxlib_standardpath( test_lnk, normalize = TRUE ) )
  
  
  # -- expected
  
  expected_path <- test_lnk
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_path )
  testthat::expect_false( dir.exists( result ) )
  testthat::expect_false( file.exists( result ) )
  
  
})


