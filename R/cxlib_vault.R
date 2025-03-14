#' Connect to a key vault
#' 
#' @returns A standard reference class to access vault secrets
#' 
#' @description
#' This function returns a reference class object the represents the vault.
#' 
#' \strong{\emph{It is on the role of the developer and implementer to ensure 
#' that the secrets stored in the vault and their use are appropriately protected.}}
#' 
#' The vault service is defined using the cxlib property `VAULT`. If the property 
#' is not defined, the request results in an error. 
#' 
#' A local vault can be enabled with `VAULT` equal to \code{LOCAL}. See 
#' \link{.cxlib_vaultlocal} for configuration options.
#' 
#' A connection to Azure Key Vault can be enabled with `VAULT` equal to
#' \code{AZUREKV}. See \link{.cxlib_vaultazurekv} for configuration options.
#' 
#' @export


cxlib_vault <- function() {
  
  # -- configuration
  #    note: uses standard configuration
  cfg <- cxlib::cxlib_config()
  
  
  # -- identify vault
  vaultsvc <- cfg$option( "vault", unset = NA )
  
  if ( is.na(vaultsvc) )
    stop( "No vault service is configured" )


  if ( base::toupper(vaultsvc) == "LOCAL" )
    return(invisible( cxlib:::.cxlib_vaultlocal() ))
    
  if ( base::toupper(vaultsvc) == "AZUREKV" )
    return(invisible( cxlib:::.cxlib_vaultazurekv() ))
  
  
  stop( "Vault service ", base::toupper(vaultsvc), " is not supported" )
  
}