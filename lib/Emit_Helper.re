include Migrate_parsetree;
include Ast_410;
include Ppxlib;
module Ast_builder =
  Ast_builder.Make({
    let loc = Location.none;
  });
include Ast_builder;

let lident = (~modules=?, label) => {
  let modules = Option.value(~default=[], modules);
  List.append(modules, [label]) |> String.concat(".") |> lident;
};
let evar = (~modules=?, label) => {
  let modules = Option.value(~default=[], modules);
  List.append(modules, [label]) |> String.concat(".") |> evar;
};
