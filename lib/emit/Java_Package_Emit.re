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
      |> Java_Class.emit_module_type
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

// TODO: classes in default package

let emit_file_type = (env, t) => {
  let packages = packages(t);
  let modules = packages |> List.map(emit_type(env));
  [%sig: open JavaFFI; [%%s [psig_recmodule(modules)]]];
};

let emit_file_classes = (env, t) => {
  let rec all_classes = (acc, t) => {
    let classes =
      classes(t)
      |> List.map(id => {
           // TODO: exception
           let java_class = Java_Env.find(id, env);
           (id, Java_Class.emit_file(java_class));
         });
    let new_acc = List.append(classes, acc);
    switch (packages(t)) {
    | [] => new_acc
    | packages => packages |> List.concat_map(all_classes(new_acc))
    };
  };
  let packages = packages(t);
  packages |> List.concat_map(all_classes([]));
};
