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

let pstr_value_alias = (name, expr) => {
  let binding = value_binding(~pat=pvar(name), ~expr);
  pstr_value(Nonrecursive, [binding]);
};
let psig_type_alias = (name, alias) => {
  let constr = ptyp_constr(Located.mk(alias), []);
  let declaration =
    type_declaration(
      ~name=Located.mk(name),
      ~params=[],
      ~cstrs=[],
      ~kind=Ptype_abstract,
      ~private_=Public,
      ~manifest=Some(constr),
    );
  psig_type(Nonrecursive, [declaration]);
};
let pexp_let_alias = (name, value, expr) => {
  let binding = value_binding(~pat=pvar(name), ~expr=value);
  pexp_let(Nonrecursive, [binding], expr);
};

let pexp_fun_helper = (args, ret) =>
  List.fold_right(
    ((label, arg), fn) => pexp_fun(label, None, arg, fn),
    args,
    ret,
  );
let ptyp_arrow_helper = (args, ret) =>
  List.fold_right(
    ((label, arg), fn) => ptyp_arrow(label, arg, fn),
    args,
    ret,
  );

let change_warning = number => {
  let expr = estring(string_of_int(number));
  let payload = PStr([pstr_eval(expr, [])]);
  attribute(~name=loc("ocaml.warning"), ~payload);
};
let pstr_open_alias = (~warning=false, ~override=Fresh, lid) => {
  let ident = pmod_ident(loc(lid));
  let info = {
    popen_expr: ident,
    popen_override: override,
    popen_loc: Ast_builder.loc,
    popen_attributes: warning ? [] : [change_warning(-33)],
  };
  pstr_open(info);
};

let psig_module_alias_module = (name, alias) => {
  module_declaration(
    ~name=loc(Some(name)),
    ~type_=pmty_typeof(pmod_ident(loc(alias))),
  )
  |> psig_module;
};
