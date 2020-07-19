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
  let of_jvm_signature = string =>
    switch (String.index_opt(string, ';')) {
    | Some(index) =>
      // TODO: subclass
      let full_name = String.sub(string, 1, index - 1);
      let.ok object_type = of_jvm_name(full_name);
      Ok((object_type, index));
    | None => Error("missing ; at object")
    };
  let to_jvm_signature = id => {
    let full_name = to_jvm_name(id);
    "L" ++ full_name ++ ";";
  };
};

[@deriving (show, eq, ord)]
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

let rec of_jvm_signature = string => {
  let.ok first_letter =
    switch (string) {
    | "" => Error("empty string")
    | string => Ok(string.[0])
    };
  // TODO: improve this
  switch (first_letter) {
  | 'V' => Ok((Void, 1))
  | 'Z' => Ok((Boolean, 1))
  | 'B' => Ok((Byte, 1))
  | 'C' => Ok((Char, 1))
  | 'S' => Ok((Short, 1))
  | 'I' => Ok((Int, 1))
  | 'J' => Ok((Long, 1))
  | 'F' => Ok((Float, 1))
  | 'D' => Ok((Double, 1))
  | '[' =>
    let remaining_string = String.sub(string, 1, String.length(string) - 1);
    let.ok (array_type, position) = of_jvm_signature(remaining_string);
    Ok((Array(array_type), position + 1));
  | 'L' =>
    let.ok (object_type, position) = Object_Type.of_jvm_signature(string);
    Ok((Object(object_type), position));
  | _ => Error("unknown type")
  };
};
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
