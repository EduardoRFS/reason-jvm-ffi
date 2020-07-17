module Object_Type = {
  // TODO: it's not only a type
  type t = {
    package: string, // separeted by .
    name: string,
  };

  let to_code_name = id =>
    id.package ++ "." ++ String.capitalize_ascii(id.name);
  /** a fully qualified name, using / instead of . */
  let to_jvm_name = id => {
    let package =
      id.package |> String.split_on_char('.') |> String.concat("/");
    package ++ "/" ++ id.name;
  };
  let to_jvm_signature = id => {
    let full_name = to_code_name(id);
    "L" ++ full_name ++ ";";
  };

  open Emit_Helper;
  let emit_lid = id => {
    let last_module = String.capitalize_ascii(id.name);
    let modules =
      id.package
      |> String.split_on_char('.')
      |> List.map(String.capitalize_ascii);
    let modules = List.append(modules, [last_module]);
    lident(~modules, "t");
  };
  let emit_unsafe_lid = id => {
    let last_module = String.capitalize_ascii(id.name);
    let modules =
      id.package
      |> String.split_on_char('.')
      |> List.map(String.capitalize_ascii);
    let modules =
      List.append(modules, [last_module, "Unsafe", "Please", "Stop"]);
    lident(~modules, unsafe_t);
  };
  let emit_type = id => {
    let lid = emit_lid(id) |> Located.mk;
    ptyp_constr(lid, []);
  };
};

type t =
  | Void
  | Boolean
  | Byte
  | Char
  | Short
  | Int
  // TODO: maybe Camlint?
  | Long
  | Float
  | Double
  | Object(Object_Type.t)
  | Array(t);
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
let rec to_jvm_signature =
  fun
  | Void => "V"
  | Boolean => "Z"
  | Byte => "B"
  | Char => "C"
  | Short => "S"
  | Int => "I"
  | Long => "J"
  | Float => "F"
  | Double => "D"
  | Object(object_type) => Object_Type.to_jvm_signature(object_type)
  | Array(java_type) => "[" ++ to_jvm_signature(java_type);

open Emit_Helper;
let rec emit_type =
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
  | Object(object_type) => Object_Type.emit_type(object_type)
  | Array(java_type) => [%type: list([%t emit_type(java_type)])];
