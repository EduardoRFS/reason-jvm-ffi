open Basic_types;
open Emit_Helper;

let (let.ok) = Result.bind;

module Object_Type = {
  type t = Basic_types.class_name;

  let to_code_name = id =>
    List.append(id.package, [id.name]) |> String.concat(".");

  /** a fully qualified name, using / instead of . */
  let of_jvm_name = name =>
    switch (String.split_on_char('/', name) |> List.rev) {
    | [name, ...parts] =>
      let package = parts |> List.rev;
      Ok({package, name});
    | [] => Error("empty jvm_name")
    };
  let to_jvm_name = id => {
    let package = id.package |> String.concat("/");
    package ++ "/" ++ id.name;
  };

  // TODO: emit module type lid
  let emit_module_lid = (id: t) => {
    let last_module = String.capitalize_ascii(id.name);
    let modules = id.package |> List.map(String.capitalize_ascii);
    lident(~modules, last_module);
  };
  let emit_lid = (id: t) => Ldot(emit_module_lid(id), "t");
  let emit_sub_lid = (id: t) => Ldot(emit_module_lid(id), "sub");
  // TODO: improve that
  let emit_unsafe_lid = (id: t) =>
    Ldot(
      Ldot(Ldot(Ldot(emit_module_lid(id), "Unsafe"), "Please"), "Stop"),
      unsafe_t,
    );
  let emit_type = (id: t) => {
    let lid = emit_lid(id) |> Located.mk;
    ptyp_constr(lid, []);
  };
  let emit_sub_type = (index, id: t) => {
    // TODO: more than 26 parameters
    let char = Char.chr(97 + index);
    let name = String.make(1, char);
    let lid = emit_sub_lid(id);
    ptyp_constr(loc(lid), [ptyp_var(name)]);
  };
};

let rec to_code_name =
  fun
  | Void => "void"
  | Boolean => "boolean"
  | Byte => "byte"
  | Char => "char"
  | Short => "short"
  | Int => "int"
  | Long => "long"
  | Float => "float"
  | Double => "double"
  | Object(object_type) => Object_Type.to_code_name(object_type)
  | Array(java_type) => to_code_name(java_type) ++ "[]";

let find_required_class =
  fun
  | Object(object_type) => [object_type]
  | _ => [];

let rec emit_type = (~kind=`Return) =>
  fun
  | Void => [%type: unit]
  | Boolean => [%type: bool]
  // TODO: type aliases
  | Byte => [%type: int]
  | Char => [%type: int]
  | Short => [%type: int]
  | Int => [%type: int32]
  | Long => [%type: int64]
  | Float => [%type: float]
  | Double => [%type: float]
  | Object(object_type) =>
    switch (kind) {
    | `Parameter(index) => Object_Type.emit_sub_type(index, object_type)
    | _ => Object_Type.emit_type(object_type)
    }

  | Array(java_type) => [%type: list([%t emit_type(java_type)])];

let emit_camljava_jni_to_call = (kind, static, t) => {
  let type_name =
    switch (t) {
    | Object(_)
    | Array(_) => "object"
    | java_type => to_code_name(java_type)
    };
  let function_name =
    switch (kind, static) {
    | (`Method, true) => "call_static_" ++ type_name ++ "_method"
    | (`Method, false) => "call_" ++ type_name ++ "_method"
    | (`Getter, true) => "get_static_" ++ type_name ++ "_field"
    | (`Getter, false) => "get_" ++ type_name ++ "_field"
    | (`Setter, true) => "set_static_" ++ type_name ++ "_field"
    | (`Setter, false) => "set_" ++ type_name ++ "_field"
    };
  evar(~modules=["Jni"], function_name);
};
