open Reason_jvm_ffi_ir;
include Migrate_parsetree;
module Ast_builder =
  Ppxlib.Ast_builder.Make({
    let loc = Location.none;
  });
include Ast_builder;
include Ast_410.Asttypes;

module StringMap = Map.Make(String);
let id = a => a;
let loc = Located.mk;
let pexp_fun = (args, ret) =>
  List.fold_right(
    ((label, arg), fn) => Ast_builder.pexp_fun(label, None, arg, fn),
    args,
    ret,
  );

let jvm_type_to_runtimelib_name =
  fun
  | Boolean => "boolean"
  | Byte => "byte"
  | Char => "char"
  | Short => "short"
  | Int => "int"
  | Long => "long"
  | Float => "float"
  | Double => "double"
  | Object(_)
  | Array(_) => "object";

let this_id = "this";
let find_class_expr = string => [%expr
  Jvm_ffi_runtime.find_class([%e estring(string)])
];
