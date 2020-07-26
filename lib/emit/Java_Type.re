open Basic_types;

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

// TODO: order of arguments is reversed
