open Emit_Helper;
open Basic_types;
open Structures;
open Lid;

// TODO: keep same method order as in the bytecode

let emit_constructor = (class_name, env, constructor: java_method) =>
  env
  |> Env.constructor_lid(class_name, constructor.name)
  |> loc
  |> pexp_ident;

let emit_functor_env = (required_classes, t: java_class) => {
  let add_class = (env, clazz: java_class) => {
    let lid = concat_lid([class_lid(clazz.name), unsafe_module_lid]);
    env |> Env.add(clazz.name, lid);
  };
  let add_self = (self: java_class, env) => {
    env
    |> Env.add(
         self.name,
         unsafe_module_lid,
       );
  };
  required_classes |> List.fold_left(add_class, Env.empty) |> add_self(t);
};
let emit_functor_parameters_type = (required_classes, self: java_class) => {
  // TODO: duplicated because mutually recursive
  let emit_alias_type = {
    let rec emit_package_type = (class_fn, t) => {
      let packages =
        t
        |> Java_Package.packages
        |> List.rev_map(package =>
             emit_package_type(class_fn, package) |> psig_module
           );
      let modules =
        // TODO: handle exception
        t |> Java_Package.classes |> List.rev_map(class_fn);

      let signature = List.append(packages, modules);
      module_declaration(
        ~name=Located.mk(Some(t.name |> String.capitalize_ascii)),
        ~type_=pmty_signature(signature),
      );
    };
    emit_package_type(
      // TODO: hardcoded Javatype
      class_id => {
      // TODO: this is clearly hackish
      let class_lid = class_lid(class_id);
      let class_lid =
        self.java_name == class_id
          ? concat_lid([class_lid, unsafe_module_lid, Lident("Class")])
          : class_lid;
      // TODO: this kinda of module creationg should be centralized
      module_declaration(
        ~name=Located.mk(Some(class_id.name |> String.capitalize_ascii)),
        ~type_=pmty_typeof(pmod_ident(Located.mk(class_lid))),
      )
      |> psig_module;
    });
  };
  let required_classes = required_classes |> List.map(clazz => clazz.name);
  let modules = {
    required_classes
    // TODO: this is clearly hackish
    @ [self.java_name]
    |> Java_Package.of_classes(".")
    // TODO: classes on default package???
    |> Java_Package.packages
    |> List.rev_map(emit_alias_type)
    |> List.rev_map(psig_module);
  };
  // TODO: hardcoded Javatype
  let open_javatype = [%sigi:
    [@ocaml.warning "-33"]
    open Javatype
  ];
  pmty_signature([open_javatype, ...modules]);
};
let emit = (required_class, t) => {
  let env = emit_functor_env(required_class, t);
  // open Unsafe.Please.Stop
  let env = env |> Env.open_lid(unsafe_module_lid);
  let static_methods =
    t.functions
    |> List.map((method: java_method) => {
         pstr_value_alias(
           method.name,
           Env.function_lid(t.name, method.name, env)
           |> Located.mk
           |> pexp_ident,
         )
       });
  let constructors =
    t.constructors
    |> List.map((method: java_method) =>
         pstr_value_alias(method.name, emit_constructor(t.name, env, method))
       );
  let type_value =
    pstr_type_alias(
      "t",
      concat_lid([unsafe_module_lid, Lident("Class"), unsafe_lid("t")]),
    );
  let safe_values =
    List.concat([
      [type_value, [%stri type sub('a) = {.. ...t} as 'a]],
      constructors,
      static_methods,
    ]);

  let open_package = pstr_open_alias(package_lid(t.java_name.package));
  let env = Env.open_lid(package_lid(t.java_name.package), env);
  let content = [%str
    [@ocaml.warning "-33"]
    open Params;
    %s
    [open_package, unsafe_module(Unsafe.emit(env, t))];
    [@ocaml.warning "-33"]
    open Unsafe.Please.Stop;
    %s
    safe_values
  ];
  let content = {
    let lid = Java_Type.Object_Type.emit_module_lid(t.java_name);
    let lid = concat_lid([Lident("Javatype"), lid]);
    let mod_type = pmty_typeof(pmod_ident(Located.mk(lid)));
    pmod_constraint(pmod_structure(content), mod_type);
  };
  let parameter =
    Named(
      Located.mk(Some("Params")),
      emit_functor_parameters_type(required_class, t),
    );
  // useful to ensure the generated code complies with the type definition
  let mod_functor = pmod_functor(parameter, content);
  module_binding(~name=Located.mk(Some("Make")), ~expr=mod_functor)
  |> pstr_module;
};
