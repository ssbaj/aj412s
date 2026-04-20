
recode_values<-function (x, ..., from = NULL, to = NULL, default = NULL, unmatched = "default", ptype = NULL) 
{
     # Original Code from dplyr. It was borrowed for students taking my statistics course.
     
    check_dots_unnamed()
    args <- eval_formulas_or_from_and_to(..., from = from, to = to, 
        allow_empty_dots = FALSE)
    from <- args$from
    to <- args$to
    from_as_list_of_vectors <- args$from_as_list_of_vectors
    to_as_list_of_vectors <- args$to_as_list_of_vectors
    from_arg <- args$from_arg
    to_arg <- args$to_arg
    vec_recode_values(x = x, from = from, to = to, default = default, 
        unmatched = unmatched, from_as_list_of_vectors = from_as_list_of_vectors, 
        to_as_list_of_vectors = to_as_list_of_vectors, ptype = ptype, 
        x_arg = "x", from_arg = from_arg, to_arg = to_arg, default_arg = "default", 
        error_call = current_env())
}
