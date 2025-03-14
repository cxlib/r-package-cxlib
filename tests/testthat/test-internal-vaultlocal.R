#
#  Tests for cxlib:::.cxlib_vaultlocal()
#
#


testthat::test_that( "vaultlocal.initNoConfig", {
  
  # .. test
  testthat::expect_error( cxlib:::.cxlib_vaultlocal(), regexp = "^Vault configuration is not local$" )
  
})




testthat::test_that( "vaultlocal.initNoVaultDirectoryPath", {

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
                       "VAULT = LOCAL" ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  testthat::expect_error( cxlib:::.cxlib_vaultlocal(), regexp = "^The vault root directory is not defined or does not exist$" )
  
})




testthat::test_that( "vaultlocal.initConfigDirectoryNotExist", {

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
  
  
  # vault directory
  
  test_vault_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists( test_vault_path ) || file.exists( test_vault_path ) )
    testthat::fail( "Unexpected test vault directory exists" )
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ) ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  

  # -- test
  testthat::expect_error( cxlib:::.cxlib_vaultlocal(), regexp = "^The vault root directory is not defined or does not exist$" )
  
})




testthat::test_that( "vaultlocal.emptyList", {

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
  
  
  # vault directory
  
  test_vault_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_vault_path ) && ! dir.create( test_vault_path, recursive = TRUE ) )
    testthat::fail( "Could not stage test vault directory" )
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ) ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # -- test
  
  vault <- cxlib:::.cxlib_vaultlocal()
  
  result <- vault$list()
  
  
  # -- assertions
  
  testthat::expect_true( inherits( result, "character") )
  testthat::expect_length( result , 0 )

})





testthat::test_that( "vaultlocal.list", {
  
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
  
  
  # vault directory
  
  test_vault_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_vault_path ) && ! dir.create( test_vault_path, recursive = TRUE ) )
    testthat::fail( "Could not stage test vault directory" )
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ) ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # inject secrets
  
  test_secrets <- base::replicate( 20, 
                                   paste( base::replicate( 5, 
                                                           paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "-", "_"), 10 ), collapse = ""),
                                                           simplify = TRUE),
                                          collapse = "/" )
                                   , simplify = TRUE )
  
  for ( xsecret in test_secrets ) {
    
    secrets_file <- file.path( test_vault_path, xsecret, fsep = "/" )
    
    if ( ! dir.create( base::dirname(secrets_file), recursive = TRUE ) )
      testthat::fail( "Failed to create hierarchy for secret" )
    
    base::writeLines( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), ".", "-", "_"), 40 ), collapse = ""),
                      con = secrets_file )
    
    if ( ! file.exists( secrets_file ) )
      testthat::fail( "Could not stage secret" )
    
  }
  
  
  # -- test
  
  vault <- cxlib:::.cxlib_vaultlocal()
  
  result <- vault$list()

  
  # -- expected
  
  expected_secrets <- sort(paste0( "/", test_secrets ))


  # -- assertions

  testthat::expect_equal( result, expected_secrets )  

})





testthat::test_that( "vaultlocal.secretNotExist", {
  
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
  
  
  # vault directory
  
  test_vault_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_vault_path ) && ! dir.create( test_vault_path, recursive = TRUE ) )
    testthat::fail( "Could not stage test vault directory" )
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ) ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # inject secrets
  
  test_secrets <- base::replicate( 20, 
                                   paste( base::replicate( 5, 
                                                           paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "-", "_"), 10 ), collapse = ""),
                                                           simplify = TRUE),
                                          collapse = "/" )
                                   , simplify = TRUE )
  
  for ( xsecret in test_secrets ) {
    
    secrets_file <- file.path( test_vault_path, xsecret, fsep = "/" )
    
    if ( ! dir.create( base::dirname(secrets_file), recursive = TRUE ) )
      testthat::fail( "Failed to create hierarchy for secret" )
    
    base::writeLines( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), ".", "-", "_"), 40 ), collapse = ""),
                      con = secrets_file )
    
    if ( ! file.exists( secrets_file ) )
      testthat::fail( "Could not stage secret" )
    
  }
  
  
  # reference secret

  test_ref_secret <- paste( base::replicate( 5, 
                                             paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "-", "_"), 10 ), collapse = ""),
                                             simplify = TRUE),
                            collapse = "/" ) 
  
  
  if ( test_ref_secret %in% test_secrets )
    testthat::fail( "Could not generate a secret that does not exist" )
  
  

  # -- test
  
  vault <- cxlib:::.cxlib_vaultlocal()
  
  result <- vault$secret( paste0( "/", test_ref_secret ), unset = NA )
  

  # -- assertions

  testthat::expect_true( is.na(result) )  

})



testthat::test_that( "vaultlocal.secret", {
  
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
  
  
  # vault directory
  
  test_vault_path <- cxlib::cxlib_standardpath( base::tempfile( pattern = "test-vault-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_vault_path ) && ! dir.create( test_vault_path, recursive = TRUE ) )
    testthat::fail( "Could not stage test vault directory" )
  
  
  # inject cxlib properties file in .libPaths
  test_cxlib_path <- file.path( test_root, "cxlib", fsep = "/" )
  
  if ( ! dir.exists( test_cxlib_path ) && ! dir.create( test_cxlib_path, recursive = TRUE ) )
    testthat::fail("Could not stage cxlib in test area")
  
  base::writeLines( c( "# test properties file",
                       "VAULT = LOCAL", 
                       paste0( "VAULT.DATA = ", test_vault_path ) ),
                    con = file.path( test_cxlib_path, "cxlib.properties", fsep = "/") )
  
  if ( ! file.exists( file.path( test_cxlib_path, "cxlib.properties", fsep = "/") ) )
    testthat::fail( "Could not stage cxlib.properties" )
  
  
  # inject secrets
  
  test_secrets <- base::replicate( 20, 
                                   paste( base::replicate( 5, 
                                                           paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "-", "_"), 10 ), collapse = ""),
                                                           simplify = TRUE),
                                          collapse = "/" )
                                   , simplify = TRUE )
  
  for ( xsecret in test_secrets ) {
    
    secrets_file <- file.path( test_vault_path, xsecret, fsep = "/" )
    
    if ( ! dir.create( base::dirname(secrets_file), recursive = TRUE ) )
      testthat::fail( "Failed to create hierarchy for secret" )
    
    base::writeLines( paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), ".", "-", "_"), 40 ), collapse = ""),
                      con = secrets_file )
    
    if ( ! file.exists( secrets_file ) )
      testthat::fail( "Could not stage secret" )
    
  }
  
  
  # reference secret
  
  test_ref_secret <- paste( base::replicate( 5, 
                                             paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "-", "_"), 10 ), collapse = ""),
                                             simplify = TRUE),
                            collapse = "/" ) 
  
  
  if ( test_ref_secret %in% test_secrets )
    testthat::fail( "Could not generate a secret that does not exist" )
  
  
  
  # -- test
  
  vault <- cxlib:::.cxlib_vaultlocal()
  
  result <- vault$secret( paste0( "/", utils::tail( test_secrets, n = 1 ) ), unset = NA )
  
  
  # -- expectations
  
  expected_secret_value <- base::readLines( file.path( test_vault_path, utils::tail( test_secrets, n = 1 ), fsep = "/" ) )
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_secret_value )
  
})





