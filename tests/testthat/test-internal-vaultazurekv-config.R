#
#  Tests for cxlib:::.cxlib_vaultazurekv()
#
#



testthat::test_that( "vaultazurekv.initNoConfig", {
  
  # .. test
  testthat::expect_error( cxlib:::.cxlib_vaultazurekv(), regexp = "^Vault configuration is not Azure Key Vault$" )
  
})





testthat::test_that( "vaultazurekv.initNoVaultURL", {
  
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
  
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = AZUREKV" ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_vaultazurekv(), regexp = "^Azure Key Vault URL not configured$" )
  
})






testthat::test_that( "vaultazurekv.initNoPropOAuthURL", {
  
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
  
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = AZUREKV",
                       "AZUREKV.URL = something" ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_vaultazurekv(), regexp = "^Required Azure Key Vault property AZUREKV.OAUTH.URL not defined$" )
  
})




testthat::test_that( "vaultazurekv.initNoPropOAuthClientID", {
  
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
  
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = AZUREKV",
                       "AZUREKV.URL = something", 
                       "AZUREKV.OAUTH.URL = anything"),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_vaultazurekv(), regexp = "^Required Azure Key Vault property AZUREKV.OAUTH.CLIENTID not defined$" )
  
})





testthat::test_that( "vaultazurekv.initNoPropOAuthClientSecret", {
  
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
  
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = AZUREKV",
                       "AZUREKV.URL = something", 
                       "AZUREKV.OAUTH.URL = anything", 
                       "AZUREKV.OAUTH.CLIENTID = any"),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_vaultazurekv(), regexp = "^Required Azure Key Vault property AZUREKV.OAUTH.CLIENTSECRET not defined$" )
  
})



testthat::test_that( "vaultazurekv.initNoPropOAuthScope", {
  
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
  
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = AZUREKV",
                       "AZUREKV.URL = something", 
                       "AZUREKV.OAUTH.URL = anything", 
                       "AZUREKV.OAUTH.CLIENTID = any", 
                       "AZUREKV.OAUTH.CLIENTSECRET = any" ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_vaultazurekv(), regexp = "^Required Azure Key Vault property AZUREKV.OAUTH.SCOPE not defined$" )
  
})



