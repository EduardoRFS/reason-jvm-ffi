include Ppxlib;
module Ast_builder =
  Ast_builder.Make({
    let loc = Location.none;
  });
include Ast_builder;
include Located;

let evar = (~modules=?, label) => {
  let modules = Option.value(~default=[], modules);
  List.append(modules, [label]) |> String.concat(".") |> evar;
};
