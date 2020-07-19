[@deriving show]
type t = {
  name: string,
  static: bool,
  parameters: list((string, Java_Type.t)),
  return_type: Java_Type.t,
};

let (let.ok) = Result.bind;

// TODO: make a proper lexer
// TODO: this function should have another name
let of_jvm_signature = (~name, ~static, string) => {
  let rec parse_parameters = ((acc, position), string) =>
    switch (string) {
    | "" => Ok((acc, position))
    | string =>
      let.ok (java_type, position) = Java_Type.of_jvm_signature(string);
      let string =
        String.sub(string, position, String.length(string) - position);
      parse_parameters(([java_type, ...acc], position), string);
    };
  let.ok first_letter =
    switch (string) {
    | "" => Error("empty string at Java_Method")
    // TODO: I think it being String.get actually matters nowadays
    | string => Ok(string.[0])
    };
  let.ok parameters =
    switch (first_letter) {
    | '(' =>
      switch (String.index_opt(string, ')')) {
      | Some(index) => Ok(String.sub(string, 1, index - 1))
      | None => Error("invalid parameters missing )")
      }
    | _ => Error("invalid jvm_signature missing (")
    };

  let.ok (parameters, position) = parse_parameters(([], 0), parameters);
  let string_remaining =
    String.sub(string, position + 2, String.length(string) - position - 2);
  // TODO: validate position
  let.ok (return_type, _) = Java_Type.of_jvm_signature(string_remaining);

  // TODO: recover the name
  let parameters =
    parameters
    |> List.mapi((index, param_type)
         // TODO: find a way to extract names
         => ("param_" ++ string_of_int(index), param_type));
  Ok({name, static, parameters, return_type});
};
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
