open Java_Package;
open Emit_Helper;

let rec emit_package_type = (class_fn, t) => {
  let packages =
    t
    |> packages
    |> List.rev_map(package =>
         emit_package_type(class_fn, package) |> psig_module
       );
  let modules =
    // TODO: handle exception
    t |> classes |> List.rev_map(class_fn);

  let signature = List.append(packages, modules);
  module_declaration(
    ~name=Located.mk(Some(t.name |> String.capitalize_ascii)),
    ~type_=pmty_signature(signature),
  );
};
let emit_type = (env, t) =>
  emit_package_type(
    class_id =>
      Java_Env.find(class_id, env)
      |> Java_Class_Emit.emit_module_type
      |> psig_module,
    t,
  );
let emit_alias_type = t =>
  emit_package_type(
    class_id => {
      let typ_t = Java_Type_Emit.Object_Type_Emit.emit_type(class_id);
      [%sigi: type t = [%t typ_t]];
    },
    t,
  );
