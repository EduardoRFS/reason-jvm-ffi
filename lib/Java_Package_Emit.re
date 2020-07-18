open Java_Package;
open Emit_Helper;

let rec emit_type = (env, t) => {
  let packages =
    t.packages
    |> StringMap.bindings
    |> List.rev_map(((_, package)) =>
         emit_type(env, package) |> psig_module
       );
  let modules =
    t.classes
    // TODO: handle exception
    |> StringMap.bindings
    |> List.rev_map(((_, class_id)) =>
         Java_Env.find(class_id, env)
         |> Java_Class_Emit.emit_module_type
         |> psig_module
       );
  let signature = List.append(packages, modules);
  module_declaration(
    ~name=Located.mk(Some(t.name |> String.capitalize_ascii)),
    ~type_=pmty_signature(signature),
  );
};
