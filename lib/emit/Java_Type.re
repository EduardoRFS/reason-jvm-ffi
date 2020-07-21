let (let.ok) = Result.bind;

module Object_Type = {
  // TODO: it's not only a type
  [@deriving (show, eq, ord)]
  type t = {
    package: list(string),
    name: string,
  };

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
  let to_jvm_signature = id => {
    let full_name = to_jvm_name(id);
    "L" ++ full_name ++ ";";
  };

  /** so if you have a same package access it doesn't go through the full path */
  let relativize = (clazz, t) => {
    clazz.package == t.package ? {package: [], name: t.name} : t;
  };
};

[@deriving (show, eq, ord)]
type t =
  // TODO: void should probably be removed
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

let relativize = clazz =>
  fun
  | Object(object_type) => {
      let object_type = Object_Type.relativize(clazz, object_type);
      Object(object_type);
    }
  | java_type => java_type;
