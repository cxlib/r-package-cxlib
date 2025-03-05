#
#  High-level tests for cxlib::cxlib_vault()
#
#
#

testthat::test_that( "vault.noneConfig", {
  
  # -- test
  testthat::expect_error( cxlib::cxlib_vault(), regexp = "^No vault service is configured$" )
  
}) 



testthat::test_that( "vault.invalidVaultName", {
  
  # -- stage
  
  test_root <- cxlib::cxlib_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # update .libPaths
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  .libPaths( c( test_root, .libPaths() ) )
  
  
  # random vault reference
  test_reference <- "thisisnotavault"
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       paste0( "VAULT = ", test_reference ) ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  
  
    
  # -- test
  testthat::expect_error( cxlib::cxlib_vault(), regexp = paste( "^Vault service", base::toupper(test_reference) , "is not supported$" ) )
  
}) 

