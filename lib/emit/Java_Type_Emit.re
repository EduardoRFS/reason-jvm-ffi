open Emit_Helper;
open Java_Type;

module Object_Type_Emit = {
  open Object_Type;

  let emit_module_lid = id => {
    let last_module = String.capitalize_ascii(id.name);
    let modules =
      id.package
      |> String.split_on_char('.')
      |> List.map(String.capitalize_ascii);
    lident(~modules, last_module);
  };
  let emit_lid = id => Ldot(emit_module_lid(id), "t");
  // TODO: improve that
  let emit_unsafe_lid = id =>
    Ldot(
      Ldot(Ldot(Ldot(emit_module_lid(id), "Unsafe"), "Please"), "Stop"),
      unsafe_t,
    );
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
