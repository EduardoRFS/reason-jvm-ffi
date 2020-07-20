// TODO: access
[@deriving show]
type t = {
  name: string,
  static: bool,
  parameters: list((string, Java_Type.t)),
  return_type: Java_Type.t,
};

let (let.ok) = Result.bind;

let to_jvm_signature = t => {
  let args =
    t.parameters
    |> List.map(((_, java_type)) => Java_Type.to_jvm_signature(java_type))
    |> String.concat("");
  let ret = Java_Type.to_jvm_signature(t.return_type);
  "(" ++ args ++ ")" ++ ret;
};

let find_required_classes = t =>
  List.concat_map(
    ((_, java_type)) => Java_Type.find_required_class(java_type),
    t.parameters,
  )
  @ Java_Type.find_required_class(t.return_type);
