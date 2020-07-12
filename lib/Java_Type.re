type t =
  | Void
  | Boolean
  | Byte
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Object(string)
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
  | Object(name) => name
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
  | Object(name) => "L" ++ name ++ ";"
  | Array(java_type) => "[" ++ to_jvm_signature(java_type);
