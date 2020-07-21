open Emit_Helper;
open Basic_types;
open Java_Type;

module Object_Type_Emit = {
  open Object_Type;

  // TODO: emit module type lid
  let emit_module_lid = (id: t) => {
    let last_module = String.capitalize_ascii(id.name);
    let modules = id.package |> List.map(String.capitalize_ascii);
    lident(~modules, last_module);
  };
  let emit_lid = (id: t) => Ldot(emit_module_lid(id), "t");
  // TODO: improve that
  let emit_unsafe_lid = (id: t) =>
    Ldot(
      Ldot(Ldot(Ldot(emit_module_lid(id), "Unsafe"), "Please"), "Stop"),
      unsafe_t,
    );
  let emit_type = (id: t) => {
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
