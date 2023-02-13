solve_XOR_relationship <- function(
    XOR_root = NULL,
    XOR_branches = c(),
    rel_df = tibble(),
    snippet_dict,
    split_symbol = ">X>"){

  return_list <- list(
    snippet = NULL,
    activities = c(),
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = c()
  )

  ## Sanity check
  ## The root should be REQUIRED for the branches
  ## Given a root A and branches B, C, D and E:
  ## If B and C require A but D and E do not,
  ## then we create a structure
  ## >X>[A >X>[B,C]>X> ]>X> >X>[D, E]>X>
  ## If neither of B, C, D or E require A
  ## then we create a structure
  ## >X>[A]>X> >X>[B,C,D,E]>X>
  ## If B, C, D and E require A,
  ## then we create
  ## A >X>[B,C,D,E]>X>

  OPTIONAL_ROOT <- FALSE

  if(is.null(XOR_root)) XOR_root <- ""

  if(XOR_root != "" & XOR_root != "START"){
    reverse_branch_relations <- rel_df %>%
      filter(antecedent %in% XOR_branches,
             consequent == XOR_root)

    branches_requiring_root <- reverse_branch_relations %>%
      filter(rel == RScoreDict$REQUIRES) %>%
      pull(antecedent)

    branches_not_requiring_root <- XOR_branches[!(XOR_branches %in% branches_requiring_root)]

    XOR_branches <- branches_requiring_root

    if(length(XOR_branches) == 0){
      XOR_branches <- branches_not_requiring_root

      OPTIONAL_ROOT <- TRUE
    }
  }


  acts <- XOR_branches

  root_snippet <- ""
  closing_snippet <- ""

  ## If all branches join on the same point
  ## then we can include that in our XOR block
  join_points <- rel_df %>%
    filter(antecedent %in% XOR_branches,
           rel == RScoreDict$DIRECT_JOIN)

  orig_join_points <- join_points

  ## Unless that join point also has joins from
  ## other branches
  if(join_points$consequent %>% unique %>% length > 0){
    other_joins <- rel_df %>%
      filter(
        ! antecedent %in% XOR_branches,
        consequent %in% join_points$consequent,
        rel == RScoreDict$DIRECT_JOIN)


    join_points <- join_points %>%
      filter(! consequent %in% other_joins$consequent)
  }

  if(length(XOR_branches) == 1 && startsWith(XOR_branches, ">X>") && endsWith(XOR_branches, ">X>") && join_points %>% nrow == 0){
    join_points <- orig_join_points
  }

  if(join_points %>% nrow > 0){
    XOR_branches <- XOR_branches[XOR_branches %in% join_points$antecedent]

    closing_snippet <- paste(split_symbol, join_points$consequent[1], sep = " ")

    acts <- c(XOR_branches, join_points$consequent[1])
  } else {
    closing_snippet <- split_symbol
  }

  new_branches <- XOR_branches

  XOR_split_snippet = paste(split_symbol, "[",paste(new_branches, collapse = ","),"]", sep = "")
  snippet_name <- paste(XOR_split_snippet, closing_snippet, sep = "")


  other_branches_from_root <- rel_df %>%
    filter(antecedent == XOR_root,
           rel %in% c(RScoreDict$MAYBE_DIRECTLY_FOLLOWS,
                      RScoreDict$MAYBE_EVENTUALLY_FOLLOWS)) %>%
    filter(! consequent %in% XOR_branches)

  if(length(XOR_branches) == 1 || other_branches_from_root %>% nrow == 0){

    root_snippet <- XOR_root

    if(OPTIONAL_ROOT==TRUE & XOR_root != "" & !startsWith(XOR_root, "START ")){
      root_snippet <- paste(">X>[", root_snippet, "]>X>")

      snippet_dict[[XOR_root]] <-
        create_snippet(
          NULL,
          NULL,
          list(XOR_root),
          "XOR",
          snippet_dict
        )
    }

    snippet_name <- paste(root_snippet, snippet_name, sep = " ")
    acts <- c(acts, XOR_root)
  }

  snippet_acts = list()
  i <- 1
  for(act in XOR_branches){
    snippet_acts[i] <- act
    if(act %in% names(snippet_dict)){
      snippet_acts[[i]] <- snippet_dict[[act]]
    }
    i <- i+1
  }

  join_point_snip <- if(join_points %>% nrow() > 0) join_points$consequent[1] else NULL
  snippet_dict[[snippet_name]] <-
    create_snippet(
      if(root_snippet=="") "" else XOR_root,
      join_point_snip,
      snippet_acts,
      if(split_symbol == ">O>") "OR" else "XOR",
      snippet_dict
    )

  return_list <- list(
    snippet = snippet_name,
    activities = acts,
    rel_df = rel_df,
    snippet_dictionary = snippet_dict,
    messages = paste("Created process snippet:", snippet_name, sep = " ")
  )

  return(return_list)

}
