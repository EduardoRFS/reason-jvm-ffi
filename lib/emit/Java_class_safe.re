open Emit_Helper;
open Basic_types;
open Structures;
open Java_class;
// TODO: keep same method order as in the bytecode

let emit_functor_parameters_type = t => {
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
      let class_lid =
        Java_Type_Emit.Object_Type_Emit.emit_module_lid(class_id);
      // TODO: this kinda of module creationg should be centralized
      module_declaration(
        ~name=Located.mk(Some(class_id.name |> String.capitalize_ascii)),
        ~type_=pmty_typeof(pmod_ident(Located.mk(class_lid))),
      )
      |> psig_module;
    });
  };
  let required_classes = find_required_classes(t);
  let modules = {
    required_classes
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
let emit_functor = t => {
  let (functions, _methods) = get_methods_by_kind(t);
  let static_methods =
    functions
    |> List.map((method: java_method) =>
         pstr_value_alias(method.name, emit_curried_method(method))
       );

  let type_value =
    pstr_type_alias("t", concat_lid([unsafe_module_lid, unsafe_lid("t")]));
  let safe_values = [type_value, ...static_methods];

  let open_package = pstr_open_alias(package_lid(t.java_name.package));
  let content = [%str
    [@ocaml.warning "-33"]
    open Params;
    %s
    [open_package, unsafe_module(Java_class_unsafe.emit(t))];
    [@ocaml.warning "-33"]
    open Unsafe.Please.Stop;
    %s
    safe_values
  ];
  let content = {
    let mod_constraint = {
      let lid = Java_Type_Emit.Object_Type_Emit.emit_module_lid(t.java_name);
      let mod_type = pmty_typeof(pmod_ident(Located.mk(lid)));
      pmod_constraint(pmod_structure(content), mod_type);
    };
    let wrapper =
      module_binding(
        ~name=Located.mk(Some(t.name.name)),
        ~expr=mod_constraint,
      );
    pstr_recmodule([wrapper]);
  };
  // TODO: hardcoded Javatype
  let parameter =
    Named(Located.mk(Some("Params")), emit_functor_parameters_type(t));
  // useful to ensure the generated code complies with the type definition
  let mod_functor = pmod_functor(parameter, pmod_structure([content]));
  module_binding(~name=Located.mk(Some("Make")), ~expr=mod_functor)
  |> pstr_module;
};
let emit_file = t => [%str
  open JavaFFI;
  open Javatype;
  %s
  [emit_functor(t)]
];

// TODO: this is mostly duplicated code grr
let emit_method_type = (kind, method: java_method) => {
  let name = kind == `Unsafe ? unsafe_name(method.name) : method.name;
  value_description(
    ~name=Located.mk(name),
    ~type_=Java_Method.emit_type(kind, method),
    ~prim=[],
  )
  |> psig_value;
};
let emit_methods_type = (kind, methods) =>
  methods |> List.map(emit_method_type(kind));

let emit_module_type = t => {
  let static_methods =
    List.filter(({static, _}: java_method) => static, t.methods);
  let static_methods = emit_methods_type(`Method, static_methods);
  let type_declaration = {
    let alias =
      ptyp_constr(
        Located.mk(Java_Type_Emit.Object_Type_Emit.emit_unsafe_lid(t.name)),
        [],
      );
    type_declaration(
      ~name=Located.mk("t"),
      ~params=[],
      ~cstrs=[],
      ~kind=Ptype_abstract,
      ~private_=Public,
      ~manifest=Some(alias),
    );
  };
  let signature =
    List.concat([
      [Java_class_unsafe.emit_type(t)],
      static_methods,
      [psig_type(Recursive, [type_declaration])],
    ]);
  module_declaration(
    ~name=Located.mk(Some(t.name.name)),
    ~type_=pmty_signature(signature),
  );
};
