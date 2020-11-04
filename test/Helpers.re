let compare_expr = (result, expected, {Rely.expect, _}) => {
  let formatter = Reason_pprint_ast.createFormatter();
  let print_expr = expr =>
    expr
    |> Reason_migrate_parsetree.Migrate_410_409.copy_expression
    |> Reason_migrate_parsetree.Migrate_409_408.copy_expression
    |> Format.asprintf("%a", formatter#expression);

  let result = print_expr(result);
  let expected = print_expr(expected);
  expect.string(result).toEqual(expected);
};
