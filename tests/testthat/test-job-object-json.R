#
#
# Tests for cxlib jobs
# 
# JSON
#


#' @cx.testsfor cxlib::cxlib_job()




testthat::test_that( "job.toJson", {
  
  #' @cx.tests Job definition exported to JSON

  
  # -- stage
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_obj$add, test_args )
    
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  

  
  # -- test
  result <- test_obj$toJSON()
  

  # -- expected

  # - derive the expected job ID
  expected_jobid <- test_obj$.attr[["id"]]

  # - derive expected actions
  expected_actions <- test_obj$.attr[["actions"]] 


  # - expected as imported from JSON 
  expected_jsonlst <- list( "schema" = "cx.job.definition", 
                            "version" = "0.1", 
                            "job" = list( "id" = expected_jobid, 
                                          "actions" = list() ) )
  
  # add actions
  for ( xaction in expected_actions ) {
    
    exp_actiondef <- list( "id" = xaction[["id"]], 
                           "type" = xaction[["type"]], 
                           "attributes" = list() ) 
    
    # action attributes
    exp_actiondef[["attributes"]] <- lapply( base::names(xaction[["attributes"]]), function(x) {
      list( "name" = x, 
            "value" = base::unname(xaction[["attributes"]][[x]]) )
    })
    
    
    expected_jsonlst[["job"]][["actions"]][[ length(expected_jsonlst[["job"]][["actions"]]) + 1 ]] <- exp_actiondef
  }



  
  # -- assertions

  # - verify exported JSON can be imported  
  act_jsonlst <- jsonlite::fromJSON( result, simplifyVector = FALSE  )
  testthat::expect_equal( act_jsonlst, expected_jsonlst )
  
})
  





testthat::test_that( "job.fromJson", {
  
  #' @cx.tests Job definition imported from JSON
  
  
  # -- stage
  

  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_job$add, test_args )
    
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  
  
  # - generate JSON
  
  test_jsonlst <- list( "schema" = "cx.job.definition", 
                        "version" = "0.1", 
                        "job" = list( "id" = test_job$.attr[["id"]], 
                                      "actions" = list() ) )
  
  # add actions
  for ( xaction in test_job$.attr[["actions"]] ) {
    
    test_actiondef <- list( "id" = xaction[["id"]], 
                            "type" = xaction[["type"]], 
                            "attributes" = list() ) 
    
    # action attributes
    test_actiondef[["attributes"]] <- lapply( base::names(xaction[["attributes"]]), function(x) {
      list( "name" = x, 
            "value" = base::unname(xaction[["attributes"]][[x]]) )
    })
    
    
    test_jsonlst[["job"]][["actions"]][[ length(test_jsonlst[["job"]][["actions"]]) + 1 ]] <- test_actiondef
  }
  
  
  test_json <- jsonlite::toJSON( test_jsonlst, pretty = TRUE, auto_unbox = TRUE )
  
  
  
  # - test job object
  test_obj <- cxlib::cxlib_job()


  # -- test
  result <- test_obj$fromJSON( test_json )

  
  # -- expected

  # - derive the expected job ID
  expected_jobdef <- test_job$.attr

  # - job id
  expected_jobid <- test_job$.attr[["id"]]
  
  # -  job hash
  expected_hash <- test_job$hash()
  

  # -- assertions

  # - surrogate for now is internal store is restored
  testthat::expect_equal( test_obj$.attr, expected_jobdef )
  
  # - job id
  testthat::expect_equal( test_obj$.attr[["id"]], expected_jobid )
  
  # - job hash
  testthat::expect_equal( test_obj$hash(), expected_hash )
  

})





testthat::test_that( "job.fromJsonMissingSchema", {
  
  #' @cx.tests Job definition imported from JSON when schema reference is missing results in error
  
  
  # -- stage
  
  
  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_job$add, test_args )
    
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  
  
  # - generate JSON
  
  test_jsonlst <- list( "version" = "0.1", 
                        "job" = list( "id" = test_job$.attr[["id"]], 
                                      "actions" = list() ) )
  
  # add actions
  for ( xaction in test_job$.attr[["actions"]] ) {
    
    test_actiondef <- list( "id" = xaction[["id"]], 
                            "type" = xaction[["type"]], 
                            "attributes" = list() ) 
    
    # action attributes
    test_actiondef[["attributes"]] <- lapply( base::names(xaction[["attributes"]]), function(x) {
      list( "name" = x, 
            "value" = base::unname(xaction[["attributes"]][[x]]) )
    })
    
    
    test_jsonlst[["job"]][["actions"]][[ length(test_jsonlst[["job"]][["actions"]]) + 1 ]] <- test_actiondef
  }
  
  
  test_json <- jsonlite::toJSON( test_jsonlst, pretty = TRUE, auto_unbox = TRUE )
  
  
  
  # - test job object
  test_obj <- cxlib::cxlib_job()
  
  
  # -- test
  testthat::expect_error( test_obj$fromJSON( test_json ), regexp = "^Missing or invalid schema reference$" )
  
  
})






testthat::test_that( "job.fromJsonInvalidSchema", {
  
  #' @cx.tests Job definition imported from JSON when schema reference is invalid results in error
  
  
  # -- stage
  
  
  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_job$add, test_args )
    
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  
  
  # - generate JSON
  
  test_jsonlst <- list( "schema" = "this.schema.is.invalid", 
                        "version" = "0.1", 
                        "job" = list( "id" = test_job$.attr[["id"]], 
                                      "actions" = list() ) )
  
  # add actions
  for ( xaction in test_job$.attr[["actions"]] ) {
    
    test_actiondef <- list( "id" = xaction[["id"]], 
                            "type" = xaction[["type"]], 
                            "attributes" = list() ) 
    
    # action attributes
    test_actiondef[["attributes"]] <- lapply( base::names(xaction[["attributes"]]), function(x) {
      list( "name" = x, 
            "value" = base::unname(xaction[["attributes"]][[x]]) )
    })
    
    
    test_jsonlst[["job"]][["actions"]][[ length(test_jsonlst[["job"]][["actions"]]) + 1 ]] <- test_actiondef
  }
  
  
  test_json <- jsonlite::toJSON( test_jsonlst, pretty = TRUE, auto_unbox = TRUE )
  
  
  
  # - test job object
  test_obj <- cxlib::cxlib_job()
  
  
  # -- test
  testthat::expect_error( test_obj$fromJSON( test_json ), regexp = "^Missing or invalid schema reference$" )

  
})




testthat::test_that( "job.fromJsonMissingSchemaVersion", {
  
  #' @cx.tests Job definition imported from JSON when schema version reference is missing results in error
  
  
  # -- stage
  
  
  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_job$add, test_args )
    
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  
  
  # - generate JSON
  
  test_jsonlst <- list( "schema" = "cx.job.definition", 
                        "job" = list( "id" = test_job$.attr[["id"]], 
                                      "actions" = list() ) )
  
  # add actions
  for ( xaction in test_job$.attr[["actions"]] ) {
    
    test_actiondef <- list( "id" = xaction[["id"]], 
                            "type" = xaction[["type"]], 
                            "attributes" = list() ) 
    
    # action attributes
    test_actiondef[["attributes"]] <- lapply( base::names(xaction[["attributes"]]), function(x) {
      list( "name" = x, 
            "value" = base::unname(xaction[["attributes"]][[x]]) )
    })
    
    
    test_jsonlst[["job"]][["actions"]][[ length(test_jsonlst[["job"]][["actions"]]) + 1 ]] <- test_actiondef
  }
  
  
  test_json <- jsonlite::toJSON( test_jsonlst, pretty = TRUE, auto_unbox = TRUE )
  
  
  
  # - test job object
  test_obj <- cxlib::cxlib_job()
  
  
  # -- test
  testthat::expect_error( test_obj$fromJSON( test_json ), regexp = "^Schema version missing or not supported$" )
  
  
})




testthat::test_that( "job.fromJsonInvalidSchemaVersion", {
  
  #' @cx.tests Job definition imported from JSON when schema version reference is invalid results in error
  
  
  # -- stage
  
  
  # - test job
  test_job <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_job, "cxlib_job") )
  testthat::expect_equal( attr( class(test_job), "package"), "cxlib" )
  
  
  # - type references
  #   note: using test type as a poor mans look up index ... need uniqueness
  test_types <-  utils::head( base::unique(replicate( 100,  
                                                      paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                                                      simplify = TRUE )), 
                              n = 10 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  
  for ( xtype in test_types ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_job$add, test_args )
    
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  
  
  # - generate JSON
  
  test_jsonlst <- list( "schema" = "cx.job.definition", 
                        "version" = paste( as.character(sample( 0:100, 10)), collapse = "." ), 
                        "job" = list( "id" = test_job$.attr[["id"]], 
                                      "actions" = list() ) )
  
  # add actions
  for ( xaction in test_job$.attr[["actions"]] ) {
    
    test_actiondef <- list( "id" = xaction[["id"]], 
                            "type" = xaction[["type"]], 
                            "attributes" = list() ) 
    
    # action attributes
    test_actiondef[["attributes"]] <- lapply( base::names(xaction[["attributes"]]), function(x) {
      list( "name" = x, 
            "value" = base::unname(xaction[["attributes"]][[x]]) )
    })
    
    
    test_jsonlst[["job"]][["actions"]][[ length(test_jsonlst[["job"]][["actions"]]) + 1 ]] <- test_actiondef
  }
  
  
  test_json <- jsonlite::toJSON( test_jsonlst, pretty = TRUE, auto_unbox = TRUE )
  
  
  
  # - test job object
  test_obj <- cxlib::cxlib_job()
  
  
  # -- test
  testthat::expect_error( test_obj$fromJSON( test_json ), regexp = "^Schema version missing or not supported$" )
  
  
})
