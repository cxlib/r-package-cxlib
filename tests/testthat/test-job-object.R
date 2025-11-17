#
#
# Tests for cxlib jobs
#
#


#' @cx.testsfor cxlib::cxlib_job()




testthat::test_that( "job.defaultParms", {
  
  #' @cx.tests Default empty job definition is identifiable   
  

  # -- test
  result <- cxlib::cxlib_job()


  # -- assertions

  # - job ID defined (UUID format)
  testthat::expect_true( uuid::UUIDvalidate(result$.attr[["id"]]) )  

})




testthat::test_that( "job.initNoDefaultActions", {
  
  #' @cx.tests A default job definition has no default actions   
  
  
  # -- stage
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )

    
  # -- test
  result <- test_obj$actions()
  
  
  # -- assertions
  testthat::expect_length( result, 0)
  testthat::expect_length( result$.attr[["actions"]], 0)
  
  
})





testthat::test_that( "job.addSingleActionMissingType", {

  #' @cx.tests Add action to job definition without specifying type results in an error
  
  
  # -- stage 
  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # -- test
  testthat::expect_error( test_obj$add(), regexp = "^The specified action type is missing or invalid$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})




testthat::test_that( "job.addSingleActionTypeNull", {

  #' @cx.tests Add action to job definition with type equal to NULL results in an error
    
  
  # -- stage 

  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # -- test
  testthat::expect_error( test_obj$add(NULL), regexp = "^The specified action type is missing or invalid$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})



testthat::test_that( "job.addSingleActionTypeNA", {
  
  #' @cx.tests Add action to job definition with type equal to NA results in an error

    
  # -- stage 

  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # -- test
  testthat::expect_error( test_obj$add(NA), regexp = "^The specified action type is missing or invalid$" )

  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})



testthat::test_that( "job.addSingleActionTypeEmptyCharVector", {

  #' @cx.tests Add action to job definition with type equal to a character vector with no elements results in an error

  
  # -- stage 

  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # -- test
  testthat::expect_error( test_obj$add(character(0)), regexp = "^The specified action type is missing or invalid$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})




testthat::test_that( "job.addSingleActionTypeEmptyString", {
  
  #' @cx.tests Add action to job definition with type equal to an empty string results in an error

    
  # -- stage 

  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # -- test
  testthat::expect_error( test_obj$add("  "), regexp = "^The specified action type is missing or invalid$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})




testthat::test_that( "job.addSingleActionTypeInvalidTypeReference", {

  #' @cx.tests Add action to job definition with type in an invalid reference format results in an error
  
    
  
  # -- stage 

    
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  # - invalid type reference
  #   note: underscore not a valid character for an action type
  test_type <- paste( replicate( 5, paste( sample( base::letters, 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "_" )
  

  # -- test
  testthat::expect_error( test_obj$add(test_type), regexp = "^The action type is in an invalid format$" )
  
  
  # -- assertions
  testthat::expect_length( test_obj$actions(), 0)
  testthat::expect_length( test_obj$.attr[["actions"]], 0)
  
  
})




testthat::test_that( "job.addSingleActionTypeNoAttributes", {

  #' @cx.tests Add action to job definition with valid type reference and no action attributes
  
  
  # -- stage 

  
  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  # - type reference
  #   note: period a valid character for an action type
  test_type <- paste( replicate( 5, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." )
  
  
  # -- test
  result <- test_obj$add(test_type)

  
  # -- expected
  expected_actiontype <- test_type
  
  
  # -- assertions
  
  testthat::expect_length( result, 1)
  testthat::expect_length( test_obj$actions(), 1)
  testthat::expect_length( test_obj$.attr[["actions"]], 1)
  
  
  # - result action type
  testthat::expect_equal( test_obj$actions()[[1]][["type"]], expected_actiontype )
  
  
})





testthat::test_that( "job.addSingleActionTypeAttrAsList", {

  #' @cx.tests Add action to job definition with valid type reference and no action attributes

  
  # -- stage 

  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  # - type reference
  #   note: period a valid character for an action type
  test_type <- paste( replicate( 5, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." )
  
  
  # - test attributes
  test_attrs <- as.list( replicate( sample( 2:15, 1),
                                    paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
  
  base::names(test_attrs) <- replicate( length(test_attrs),
                                        paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
  

  # -- test
  result <- test_obj$add(test_type, test_attrs)

  
  # -- expected
  expected_actiontype <- test_type
  
  expected_attrs <- test_attrs
  
  
  # -- assertions
  
  testthat::expect_length( result, 1)
  testthat::expect_length( test_obj$actions(), 1)
  testthat::expect_length( test_obj$.attr[["actions"]], 1)

    
  # - result action type
  testthat::expect_equal( test_obj$actions()[[1]][["type"]], expected_actiontype )
  
  
  # - action attributes
  testthat::expect_equal( base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])), base::sort(base::names(expected_attrs)) )
  testthat::expect_equal( test_obj$.attr[["actions"]][[1]][["attributes"]][ base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])) ],
                          expected_attrs[ base::sort(base::names(expected_attrs)) ] )

})







testthat::test_that( "job.addSingleActionTypeAttrAsArgs", {

  #' @cx.tests Add action to job definition with valid type reference and action attributes specified as named arguments

  
  # -- stage 
  

  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  # - type reference
  #   note: period a valid character for an action type
  test_type <- paste( replicate( 5, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." )
  
  
  # - test attributes
  test_attrs <- as.list( replicate( sample( 2:15, 1),
                                    paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
  
  base::names(test_attrs) <- replicate( length(test_attrs),
                                        paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
  
  
  # - test arguments 
  test_args <- list( test_type, 
                     test_attrs )
  

  # -- test
  result <- do.call( test_obj$add, test_args )

  
  # -- expected
  expected_actiontype <- test_type
  
  expected_attrs <- test_attrs
  
  
  # -- assertions
  
  testthat::expect_length( result, 1)
  testthat::expect_length( test_obj$actions(), 1)
  testthat::expect_length( test_obj$.attr[["actions"]], 1)
  
  
  # - result action type
  testthat::expect_equal( test_obj$actions()[[1]][["type"]], expected_actiontype )
  
  
  # - action attributes
  testthat::expect_equal( base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])), base::sort(base::names(expected_attrs)) )
  testthat::expect_equal( test_obj$.attr[["actions"]][[1]][["attributes"]][ base::sort(base::names(test_obj$.attr[["actions"]][[1]][["attributes"]])) ],
                          expected_attrs[ base::sort(base::names(expected_attrs)) ] )
  
})







testthat::test_that( "job.addMultiActionTypeAttrAsArgs", {
  
  #' @cx.tests Default append action to job definition that has existing actions using action attributes specified as named arguments

  
  # -- stage 

  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )

    
  # - type references
  test_types <-  replicate( 5, 
                            paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                            simplify = TRUE )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  

  # - stage actions
  for ( xtype in utils::head( test_types, n = length(test_types) - 1 ) ) {

    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_obj$add, test_args )
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  


  # - test arguments
  test_args <- list( utils::tail( test_types, n = 1),
                     test_actions[[ utils::tail( test_types, n = 1) ]] )



  # -- test
  result <- do.call( test_obj$add, test_args )


  # -- expected

  expected_actiontypes <- test_types

  expected_actions <- test_actions


  # -- assertions

  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))

  for ( act_action in test_obj$.attr[["actions"]] ) {

    # - result action type
    testthat::expect_true( act_action[["type"]] %in% expected_actiontypes )


    # - action attributes
    testthat::expect_equal( base::sort(base::names(act_action[["attributes"]])), base::sort(base::names(expected_actions[[ act_action[["type"]] ]] )) )
    testthat::expect_equal( act_action[["attributes"]][ base::sort(base::names(act_action[["attributes"]])) ],
                            expected_actions[[ act_action[["type"]] ]][ base::sort(base::names(expected_actions[[ act_action[["type"]] ]] )) ] )
    
  }


})






testthat::test_that( "job.addMultiActionTypeAttrAsArgs", {
  
  #' @cx.tests Default append action to job definition that has existing actions using action attributes specified as list of named elements

  
  # -- stage 

  # - test job
  test_obj <- cxlib::cxlib_job()
  
  testthat::expect_true( inherits( test_obj, "cxlib_job") )
  testthat::expect_equal( attr( class(test_obj), "package"), "cxlib" )
  
  
  # - type references
  test_types <-  replicate( 5, 
                            paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." ), 
                            simplify = TRUE )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  # - stage actions
  for ( xtype in utils::head( test_types, n = length(test_types) - 1 ) ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_obj$add, test_args )
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  
  

  # -- test
  result <- test_obj$add( utils::tail( test_types, n = 1), test_actions[[ utils::tail( test_types, n = 1) ]] )
  
  
  # -- expected
  
  expected_actiontypes <- test_types
  
  expected_actions <- test_actions
  
  
  # -- assertions
  
  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))
  
  for ( act_action in test_obj$.attr[["actions"]] ) {
    
    # - result action type
    testthat::expect_true( act_action[["type"]] %in% expected_actiontypes )
    
    
    # - action attributes
    testthat::expect_equal( base::sort(base::names(act_action[["attributes"]])), base::sort(base::names(expected_actions[[ act_action[["type"]] ]] )) )
    testthat::expect_equal( act_action[["attributes"]][ base::sort(base::names(act_action[["attributes"]])) ],
                            expected_actions[[ act_action[["type"]] ]][ base::sort(base::names(expected_actions[[ act_action[["type"]] ]] )) ] )
    
  }
  
  
})




testthat::test_that( "job.addActionAfterZero", {
  
  #' @cx.tests Default append action to job definition at position zero
  
  
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
                              n = 5 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  

  
  # - stage actions
  for ( xtype in utils::head( test_types, n = length(test_types) - 1 ) ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_obj$add, test_args )
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  

  
  # -- test
  result <- test_obj$add( utils::tail( test_types, n = 1), test_actions[[ utils::tail( test_types, n = 1) ]], after = 0 )


  # -- expected
  
  # - expected type sequence
  #   note: we are injecting last generate test_types into first position ... after = 0
  #   note: assuming action types gives a unique identifier
  expected_actiontypes <- c( utils::tail( test_types, n = 1 ), utils::head( test_types, n = length(test_types) - 1 ) ) 
  
  expected_actions <- test_actions[ expected_actiontypes ]


  # -- assertions
  
  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))
  
  for ( idx in 1:length(test_obj$actions()) ) {
    
    # note: using idx to ensore action sequence
    
    act_def <- test_obj$actions()[[ idx ]]

    # - action type
    testthat::expect_equal( act_def[["type"]], expected_actiontypes[[idx]] )

    # - action attributes        
    testthat::expect_equal( act_def[["attributes"]][ base::sort(base::names(act_def[["attributes"]])) ], expected_actions[[idx]][ base::sort(base::names(expected_actions[[idx]])) ] )
    
  }
  
  
  
})





testthat::test_that( "job.addActionAfterMax", {
  
  #' @cx.tests Default append action to job definition at position that is larger than list of actions
  
  
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
                              n = 5 )
  
  
  # - test attributes
  test_actions <- sapply( test_types, function(x) {
    
    test_attrs <- as.list( replicate( sample( 2:5, 1),
                                      paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ) )
    
    base::names(test_attrs) <- replicate( length(test_attrs),
                                          paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE )
    
    test_attrs
  })
  
  
  
  
  
  # - stage actions
  for ( xtype in utils::head( test_types, n = length(test_types) - 1 ) ) {
    
    # stage record    
    test_args <- list( xtype, 
                       test_actions[[xtype]] )
    
    test_stage <- do.call( test_obj$add, test_args )
    
    # clean up arguments
    base::rm(test_args)
    
  } 
  
  
  
  # -- test
  result <- test_obj$add( utils::tail( test_types, n = 1), test_actions[[ utils::tail( test_types, n = 1) ]], after = length(test_actions) + 1234 )
  
  
  # -- expected
  
  # - expected type sequence
  #   note: we are injecting last generate test_types into last position ... after is largen than length of actions
  #   note: assuming action types gives a unique identifier
  expected_actiontypes <- test_types
  
  expected_actions <- test_actions[ expected_actiontypes ]
  
  
  # -- assertions
  
  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))
  
  for ( idx in 1:length(test_obj$actions()) ) {
    
    # note: using idx to ensore action sequence
    
    act_def <- test_obj$actions()[[ idx ]]
    
    # - action type
    testthat::expect_equal( act_def[["type"]], expected_actiontypes[[idx]] )
    
    # - action attributes        
    testthat::expect_equal( act_def[["attributes"]][ base::sort(base::names(act_def[["attributes"]])) ], expected_actions[[idx]][ base::sort(base::names(expected_actions[[idx]])) ] )
    
  }
  
  
  
})





testthat::test_that( "job.addActionAfterPosNotZeroLast", {
  
  #' @cx.tests Default append action to job definition at position not zero and before last
  
  
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
  

  # - new action
  
  test_newaction_attrs <- as.list( replicate( sample( 2:5, 1),
                                             paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), 
                                             simplify = TRUE ) )
  
  base::names(test_newaction_attrs) <- replicate( length(test_newaction_attrs),
                                                  paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), 
                                                  simplify = TRUE )

  test_newaction_type <- paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." )
  


  
  # - insert position
  #   note: one "- 1" is for last record
  test_after <- sample( 2:(length(test_actions) - 1), 1 )


  # -- test
  result <- test_obj$add( test_newaction_type, test_newaction_attrs, after = test_after )



  # -- expected

  # - expected type sequence
  #   note: we are injecting last generate test_types after position x
  #   note: first block are actions 1 to x
  #   note: second block is our record to insert (the last one)
  #   note: third block are records from x+1 until next to last one
  expected_actiontypes <- c( test_types[ 1:test_after],
                             test_newaction_type,
                             test_types[ (test_after + 1):length(test_types) ] )

  expected_actions <- do.call( c, list( test_actions[ 1:test_after],
                                        list(test_newaction_attrs),
                                        test_actions[ (test_after + 1):length(test_actions) ] ) )
  
  base::names(expected_actions) <- expected_actiontypes
  

  # -- assertions

  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))

  for ( idx in 1:length(test_obj$actions()) ) {

    # note: using idx to ensore action sequence

    act_def <- test_obj$actions()[[ idx ]]

    # - action type
    testthat::expect_equal( act_def[["type"]], expected_actiontypes[[idx]] )

    # - action attributes
    testthat::expect_equal( act_def[["attributes"]][ base::sort(base::names(act_def[["attributes"]])) ], expected_actions[[idx]][ base::sort(base::names(expected_actions[[idx]])) ] )

  }
  
})





testthat::test_that( "job.addActionAfterPosByActionReference", {
  
  #' @cx.tests Default append action to job definition at position identified by action reference ID
  
  
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
  
  
  # - new action
  
  test_newaction_attrs <- as.list( replicate( sample( 2:5, 1),
                                              paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), 
                                              simplify = TRUE ) )
  
  base::names(test_newaction_attrs) <- replicate( length(test_newaction_attrs),
                                                  paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), 
                                                  simplify = TRUE )
  
  test_newaction_type <- paste( replicate( 2, paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE), collapse = ""), simplify = TRUE ), collapse = "." )
  
  
  
  
  # - insert position
  #   note: one "- 1" is for last record
  
  test_obj_actions <- test_obj$actions()
  
  test_after <- sample( 2:(length(test_obj_actions) - 1), 1 )
  test_after_ref <- test_obj_actions[[ test_after ]]$id
  


  # -- test
  result <- test_obj$add( test_newaction_type, test_newaction_attrs, after = test_after_ref )



  # -- expected

  # - expected type sequence
  #   note: we are injecting last generate test_types after position x
  #   note: first block are actions 1 to x
  #   note: second block is our record to insert (the last one)
  #   note: third block are records from x+1 until next to last one
  expected_actiontypes <- c( test_types[ 1:test_after],
                             test_newaction_type,
                             test_types[ (test_after + 1):length(test_types) ] )

  expected_actions <- do.call( c, list( test_actions[ 1:test_after],
                                        list(test_newaction_attrs),
                                        test_actions[ (test_after + 1):length(test_actions) ] ) )

  base::names(expected_actions) <- expected_actiontypes


  # -- assertions

  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))

  for ( idx in 1:length(test_obj$actions()) ) {

    # note: using idx to ensore action sequence

    act_def <- test_obj$actions()[[ idx ]]

    # - action type
    testthat::expect_equal( act_def[["type"]], expected_actiontypes[[idx]] )

    # - action attributes
    testthat::expect_equal( act_def[["attributes"]][ base::sort(base::names(act_def[["attributes"]])) ], expected_actions[[idx]][ base::sort(base::names(expected_actions[[idx]])) ] )

  }
  
})




testthat::test_that( "job.dropActionZero", {
  
  #' @cx.tests Drop action 0 results in an error
  
  
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
  testthat::expect_error( test_obj$drop( 0 ), regexp = "^Position index invalid$" )

})





testthat::test_that( "job.dropFirstAction", {
  
  #' @cx.tests Drop first action (position 1) in a job definition 
  
  
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
  result <- test_obj$drop( 1 )
  
  
  
  # -- expected

  expected_actions <- test_actions[ 2:length(test_actions) ] 
    

  
  # -- assertions
  
  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))
  
  for ( idx in 1:length(test_obj$actions()) ) {
    
    # note: using idx to ensore action sequence
    
    act_def <- test_obj$actions()[[ idx ]]
    
    # - action type
    testthat::expect_equal( act_def[["type"]], base::names(expected_actions[ idx ]) )
    
    # - action attributes
    testthat::expect_equal( act_def[["attributes"]][ base::sort(base::names(act_def[["attributes"]])) ], expected_actions[[idx]][ base::sort(base::names(expected_actions[[idx]])) ] )
    
  }  
})




testthat::test_that( "job.dropLastActionLargeIndex", {
  
  #' @cx.tests Drop last action in a job definition by reference of index greater than the number of actions
  
  
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
  result <- test_obj$drop( 10*length(test_actions) )
  
  
  
  # -- expected
  
  expected_actions <- test_actions[ 1:(length(test_actions) - 1) ] 
  
  
  
  # -- assertions
  
  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))
  
  for ( idx in 1:length(test_obj$actions()) ) {
    
    # note: using idx to ensore action sequence
    
    act_def <- test_obj$actions()[[ idx ]]
    
    # - action type
    testthat::expect_equal( act_def[["type"]], base::names(expected_actions[ idx ]) )
    
    # - action attributes
    testthat::expect_equal( act_def[["attributes"]][ base::sort(base::names(act_def[["attributes"]])) ], expected_actions[[idx]][ base::sort(base::names(expected_actions[[idx]])) ] )
    
  }  
})



testthat::test_that( "job.dropActionByIndex", {
  
  #' @cx.tests Drop action in a job definition by position reference
  
  
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
  
  
  # - drop index
  test_drop <- sample( 2:(length(test_actions)-1), 1 )
  
  
  # -- test
  result <- test_obj$drop( test_drop )
  
  
  
  # -- expected
  
  expected_actions <- do.call( c, list( test_actions[ 1:(test_drop - 1) ],
                                        test_actions[ (test_drop + 1):length(test_actions) ] ) )
    


  
  # -- assertions
  
  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))
  
  for ( idx in 1:length(test_obj$actions()) ) {
    
    # note: using idx to ensore action sequence
    
    act_def <- test_obj$actions()[[ idx ]]
    
    # - action type
    testthat::expect_equal( act_def[["type"]], base::names(expected_actions[ idx ]) )
    
    # - action attributes
    testthat::expect_equal( act_def[["attributes"]][ base::sort(base::names(act_def[["attributes"]])) ], expected_actions[[idx]][ base::sort(base::names(expected_actions[[idx]])) ] )
    
  }  
})




testthat::test_that( "job.dropActionByReference", {
  
  #' @cx.tests Drop action in a job definition by action reference ID
  
  
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
  
  
  # - drop index
  
  test_obj_actions <- test_obj$actions()
  
  test_drop <- sample( 2:(length(test_obj_actions)-1), 1 )
  test_drop_ref <- test_obj_actions[[ test_drop ]][["id"]]
  

  # -- test
  result <- test_obj$drop( test_drop_ref )
  

  
  # -- expected
  
  expected_actions <- do.call( c, list( test_actions[ 1:(test_drop - 1) ],
                                        test_actions[ (test_drop + 1):length(test_actions) ] ) )
  
  

  # -- assertions
  
  testthat::expect_length( result, length(expected_actions))
  testthat::expect_length( test_obj$actions(), length(expected_actions) )
  testthat::expect_length( test_obj$.attr[["actions"]], length(expected_actions))
  
  for ( idx in 1:length(test_obj$actions()) ) {
    
    # note: using idx to ensore action sequence
    
    act_def <- test_obj$actions()[[ idx ]]
    
    # - action type
    testthat::expect_equal( act_def[["type"]], base::names(expected_actions[ idx ]) )
    
    # - action attributes
    testthat::expect_equal( act_def[["attributes"]][ base::sort(base::names(act_def[["attributes"]])) ], expected_actions[[idx]][ base::sort(base::names(expected_actions[[idx]])) ] )
    
  }  
})
