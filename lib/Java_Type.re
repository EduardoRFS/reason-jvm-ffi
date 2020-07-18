module Object_Type = {
  // TODO: it's not only a type
  [@deriving (eq, ord)]
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
};

[@deriving (eq, ord)]
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

let find_required_class =
  fun
  | Object(object_type) => [object_type]
  | _ => [];
