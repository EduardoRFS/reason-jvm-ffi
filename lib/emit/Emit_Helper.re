include Migrate_parsetree;
include Ast_410;
include Ppxlib;
module Ast_builder =
  Ast_builder.Make({
    let loc = Location.none;
  });
include Ast_builder;

let loc = Located.mk;

let lident = (~modules=?, label) => {
  let modules = Option.value(~default=[], modules);
  List.append(modules, [label]) |> String.concat(".") |> lident;
};
let evar = (~modules=?, label) => {
  let modules = Option.value(~default=[], modules);
  List.append(modules, [label]) |> String.concat(".") |> evar;
};

let unsafe_name = name => "unsafe_" ++ name;
let unsafe_t = unsafe_name("t");
let typ_unit = ptyp_constr(lident("unit") |> loc, []);

let pstr_type_alias = (name, original_name) => {
  let type_value =
    type_declaration(
      ~name=name |> loc,
      ~params=[],
      ~cstrs=[],
      ~kind=Ptype_abstract,
      ~private_=Public,
      ~manifest=Some(ptyp_constr(original_name |> loc, [])),
    );
  pstr_type(Nonrecursive, [type_value]);
};
