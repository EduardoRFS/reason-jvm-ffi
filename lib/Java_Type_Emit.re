open Emit_Helper;
open Java_Type;

module Object_Type_Emit = {
  open Object_Type;

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
  | Object(object_type) => Object_Type_Emit.emit_type(object_type)
  | Array(java_type) => [%type: list([%t emit_type(java_type)])];
