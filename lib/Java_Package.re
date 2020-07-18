type t = {
  name: string,
  packages: list(t),
  classes: list(Java_Class.t),
};

// TODO: class and package with same name
// com.github.eduardorfs.Something
// com.github.Eduardorfs

open Emit_Helper;

let rec emit_type = t => {
  let packages =
    t.packages |> List.map(package => emit_type(package) |> psig_module);
  let modules =
    t.classes
    |> List.map(package =>
         Java_Class.emit_module_type(package) |> psig_module
       );
  let signature = List.append(packages, modules);
  module_declaration(
    ~name=Located.mk(Some(t.name |> String.capitalize_ascii)),
    ~type_=pmty_signature(signature),
  );
};
