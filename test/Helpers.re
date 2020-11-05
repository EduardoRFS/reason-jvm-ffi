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
let compare_str = (result, expected, {Rely.expect, _}) => {
  let formatter = Reason_pprint_ast.createFormatter();
  let print_str = str =>
    str
    |> Reason_migrate_parsetree.Migrate_410_409.copy_structure
    |> Reason_migrate_parsetree.Migrate_409_408.copy_structure
    |> Format.asprintf("%a", formatter#structure([]));

  let result = print_str(result);
  let expected = print_str(expected);
  expect.string(result).toEqual(expected);
};
