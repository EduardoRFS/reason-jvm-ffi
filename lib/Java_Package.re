open Java_Type;

module StringMap = Map.Make(String);

type t = {
  name: string,
  packages: StringMap.t(t),
  classes: StringMap.t(Object_Type.t),
};

let make = (~packages=StringMap.empty, ~classes=StringMap.empty, name) => {
  name,
  packages,
  classes,
};

// TODO: class and package with same name
// com.github.eduardorfs.Something
// com.github.Eduardorfs

let add_to_package = ({Object_Type.package, _} as object_type, t) => {
  let rec add_internal = (t, parts) =>
    switch (parts) {
    | [] =>
      let classes = t.classes |> StringMap.add(object_type.name, object_type);
      {...t, classes};
    | [part, ...remaining] =>
      let package = {
        t.packages
        |> StringMap.find_opt(part)
        |> Option.value(~default=make(part));
      };
      let package = add_internal(package, remaining);
      let packages = t.packages |> StringMap.add(package.name, package);
      {...t, packages};
    };
  add_internal(t, String.split_on_char('.', package));
};

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
         |> Java_Class.emit_module_type
         |> psig_module
       );
  let signature = List.append(packages, modules);
  module_declaration(
    ~name=Located.mk(Some(t.name |> String.capitalize_ascii)),
    ~type_=pmty_signature(signature),
  );
};
