[@deriving show]
type jvm_classpath = string;

[@deriving show]
type jvm_type =
  | Boolean
  | Byte
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Object(jvm_classpath)
  | Array(jvm_type);

[@deriving show]
type jvm_field = {
  jf_classpath: jvm_classpath,
  jf_name: string,
  jf_type: jvm_type,
  jf_final: bool,
  jf_static: bool,
};

[@deriving show]
type jvm_method = {
  jm_classpath: jvm_classpath,
  jm_name: string,
  jm_parameters: list((option(string), jvm_type)),
  // TODO: constructors cannot have a return type?
  jm_return: option(jvm_type),
  // TODO: functions cannot be abstract
  jm_abstract: bool,
  jm_kind: [ | `Constructor | `Method | `Function],
};

[@deriving show]
type jvm_class = {
  jc_classpath: jvm_classpath,
  jc_fields: list(jvm_field),
  jc_methods: list(jvm_method),
};

let rec jvm_type_to_string =
  fun
  | Boolean => "Z"
  | Byte => "B"
  | Char => "C"
  | Short => "S"
  | Int => "I"
  | Long => "J"
  | Float => "F"
  | Double => "D"
  | Object(jvm_classpath) => "L" ++ jvm_classpath ++ ";"
  | Array(jvm_type) => "[" ++ jvm_type_to_string(jvm_type);
let jvm_method_to_signature = method => {
  let arguments =
    method.jm_parameters
    |> List.map(((_, jvm_type)) => jvm_type_to_string(jvm_type))
    |> String.concat("");
  let return =
    switch (method.jm_return) {
    | Some(jvm_type) => jvm_type_to_string(jvm_type)
    | None => "V"
    };
  "(" ++ arguments ++ ")" ++ return;
};
