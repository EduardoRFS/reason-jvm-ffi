open Migrate_parsetree;
open Ast_410;
open Ast_helper;
open Fun;
open Java_Type;

type t = {
  name: string,
  static: bool,
  parameters: list((string, Java_Type.t)),
  return_type: Java_Type.t,
};
let to_jvm_signature = t => {
  let args =
    t.parameters
    |> List.map(((_, java_type)) => Java_Type.to_jvm_signature(java_type))
    |> String.concat("");
  let ret = Java_Type.to_jvm_signature(t.return_type);
  "(" ++ args ++ ")" ++ ret;
};

open Emit_Helper;
let emit_call = (clazz_id, object_id, method_id, args, t) => {
  let id_to_call = {
    let type_name =
      switch (t.return_type) {
      | Object(_)
      | Array(_) => "object"
      | java_type => Java_Type.to_code_name(java_type)
      };
    let function_name =
      t.static
        ? "call_static_" ++ type_name ++ "_method"
        : "call_" ++ type_name ++ "_method";
    evar(~modules=["Jni"], function_name);
  };
  let args =
    [t.static ? clazz_id : object_id, method_id, args]
    |> List.map(arg => (Asttypes.Nolabel, arg));
  Exp.apply(id_to_call, args);
};

let emit_argument = ((name, java_type)) => {
  let identifier = evar(name);
  let read_expr =
    switch (java_type) {
    | Object(_) => id([%expr [%e identifier]#get_jni_jobj])
    | Array(_) => failwith("TODO: ")
    // | Array(_) => [%expr Jni.array_to_jobj([%e identifier])]
    | _ => identifier
    };

  switch (java_type) {
  | Void => failwith("void cannot be an argument")
  | Boolean => id([%expr Boolean([%e read_expr])])
  | Byte => id([%expr Byte([%e read_expr])])
  | Char => id([%expr Char([%e read_expr])])
  | Short => id([%expr Short([%e read_expr])])
  | Int => id([%expr Int([%e read_expr])])
  | Long => id([%expr Long([%e read_expr])])
  | Float => id([%expr Float([%e read_expr])])
  | Double => id([%expr Double([%e read_expr])])
  | Object(_)
  | Array(_) => id([%expr Obj([%e read_expr])])
  };
};

let emit = t => {
  // TODO: escape these names
  let clazz_id = "jni_jclazz";
  let method_id = "jni_methodID";
  let object_id = "jni_jobj";

  let declare_method_id = {
    let name = t.name |> Const.string |> Exp.constant;
    let signature = to_jvm_signature(t) |> Const.string |> Exp.constant;
    %expr
    Jni.get_methodID([%e evar(clazz_id)], [%e name], [%e signature]);
  };
  let declare_function = {
    let arguments =
      t.parameters |> List.map(emit_argument) |> Ast_helper.Exp.array;
    let call =
      emit_call(
        evar(clazz_id),
        evar(object_id),
        evar(method_id),
        arguments,
        t,
      );

    let parameters =
      t.parameters
      |> List.map(((name, _)) => (Asttypes.Labelled(name), pvar(name)));
    let last_parameter = t.static ? punit : pvar(object_id);
    let parameters = [(Asttypes.Nolabel, last_parameter), ...parameters];

    List.fold_right(
      ((label, parameter), acc) => Exp.fun_(label, None, parameter, acc),
      parameters,
      call,
    );
  };

  %expr
  {
    let [%p pvar(method_id)] = [%e declare_method_id];
    %e
    declare_function;
  };
};

let emit_type = t => {
  let parameters =
    List.map(
      ((key, value)) => (key, Java_Type.emit_type(value)),
      t.parameters,
    );
  // TODO: extract this type to somewhere else
  let last_parameter =
    t.static ? lident("unit") : lident(~modules=["Jni"], "obj");
  let last_parameter = ptyp_constr(last_parameter |> Located.mk, []);
  let return_type = Java_Type.emit_type(t.return_type);
  List.fold_right(
    ((key, core_type), acc) => ptyp_arrow(Labelled(key), core_type, acc),
    parameters,
    ptyp_arrow(Nolabel, last_parameter, return_type),
  );
};
