open Emit_Helper;
open Java_Type;
open Java_Type_Emit;
open Basic_types;

// TODO: keep same method order as in the bytecode

let (let.some) = Option.bind;

let find_required_classes = t => {
  let extends =
    switch (t.extends) {
    | Some(extends) => Java_Type.find_required_class(Object(extends))
    | None => []
    };
  let methods =
    t.methods |> List.concat_map(Java_Method.find_required_classes);
  let requireds = extends @ methods;
  requireds |> List.filter((!=)(t.name));
};

let get_methods_by_kind = t =>
  t.methods  // TODO: this clearly shouldn't be here
  |> List.map(method => Java_Method.relativize(t.name, method))
  |> List.partition(({static, _}) => static);

let jni_class_name = "unsafe_jni_class";
let object_id = "jni_jobj";

let emit_field = Java_Field_Emit.emit(jni_class_name);
let emit_fields = t =>
  t.fields
  |> List.map((field: java_field) => {
       let name = unsafe_name(field.name);
       [%stri let [%p pvar(name)] = [%e emit_field(field)]];
     });
let emit_method = Java_Method_Emit.emit(jni_class_name);
let emit_methods = methods =>
  methods
  |> List.map((method: java_method) => {
       let name = unsafe_name(method.name);
       [%stri let [%p pvar(name)] = [%e emit_method(method)]];
     });

let emit_unsafe_class = t => {
  let (_functions, methods) = get_methods_by_kind(t);

  let java_fields =
    t.fields
    |> List.map(({name, _}: java_field) =>
         pcf_method((
           Located.mk(name),
           Public,
           Cfk_concrete(
             Fresh,
             eapply(
               evar(~modules=["Fields"], unsafe_name(name)),
               [evar(object_id)],
             ),
           ),
         ))
       );
  let method_fields =
    methods
    |> List.map(({name, _}: java_method) =>
         pcf_method((
           Located.mk(name),
           Public,
           Cfk_concrete(
             Fresh,
             eapply(
               evar(~modules=["Methods"], unsafe_name(name)),
               [evar(object_id)],
             ),
           ),
         ))
       );
  let inheritance_field = {
    let.some extends_id = t.extends;
    let extends_lid =
      Object_Type_Emit.emit_unsafe_lid(extends_id) |> Located.mk;
    let extends = pcl_constr(extends_lid, []);
    let apply_class = pcl_apply(extends, [(Nolabel, evar(object_id))]);
    Some([pcf_inherit(Fresh, apply_class, None)]);
  };
  let class_fields =
    List.concat([
      Option.value(~default=[], inheritance_field),
      java_fields,
      method_fields,
    ]);
  let class_expr =
    class_structure(~self=ppat_any, ~fields=class_fields) |> pcl_structure;
  let class_fun = pcl_fun(Nolabel, None, pvar(object_id), class_expr);
  class_infos(
    ~virt=Concrete,
    ~params=[],
    ~name=Located.mk(unsafe_t),
    ~expr=class_fun // TODO: wait, class_infos without class_expr?
  );
};

let emit_unsafe = t => {
  let (functions, methods) = get_methods_by_kind(t);
  let declare_fields = [%stri
    module Fields = {
      %s
      emit_fields(t);
    }
  ];
  let declare_methods = [%stri
    module Methods = {
      %s
      emit_methods(methods);
    }
  ];
  let declare_functions = [%stri
    module Static = {
      %s
      emit_methods(functions);
    }
  ];

  let find_class = {
    let name = Object_Type.to_jvm_name(t.name) |> estring;
    [%stri let [%p pvar(jni_class_name)] = () => Jni.find_class([%e name])];
  };

  let class_declaration = pstr_class([emit_unsafe_class(t)]);
  [
    find_class,
    declare_fields,
    declare_methods,
    declare_functions,
    class_declaration,
  ];
};
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
    |> List.map(({name, _}: java_method) => {
         // TODO: please separate that
         let call =
           eapply(
             evar(~modules=["Static"], unsafe_name(name)),
             [evar(jni_class_name)],
           );
         [%stri let [%p pvar(name)] = [%e call]];
       });
  // TODO: please stop duplicating module name
  // TODO: please fix this shit code
  let type_value =
    type_declaration(
      ~name=Located.mk("t"),
      ~params=[],
      ~cstrs=[],
      ~kind=Ptype_abstract,
      ~private_=Public,
      ~manifest=
        Some(
          ptyp_constr(
            Ldot(
              Ldot(Ldot(Lident("Unsafe"), "Please"), "Stop"),
              "unsafe_t",
            )
            |> Located.mk,
            [],
          ),
        ),
    );
  let type_value = pstr_type(Nonrecursive, [type_value]);
  let safe_values = [type_value, ...static_methods];

  let content = [%str
    [@ocaml.warning "-33"]
    open Params;
    module Unsafe = {
      module Please = {
        module Stop = {
          %s
          emit_unsafe(t);
        };
      };
    };
    [@ocaml.warning "-33"]
    open Unsafe.Please.Stop;
    %s
    safe_values
  ];
  let content = {
    let mod_constraint = {
      let lid = Java_Type_Emit.Object_Type_Emit.emit_module_lid(t.name);
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
    ~type_=Java_Method_Emit.emit_type(kind, method),
    ~prim=[],
  )
  |> psig_value;
};
let emit_methods_type = (kind, methods) =>
  methods |> List.map(emit_method_type(kind));

let emit_fields_type = fields =>
  fields
  |> List.map((field: java_field) => {
       // TODO: duplicated code
       let name = field.static ? field.name : unsafe_name(field.name);
       value_description(
         ~name=Located.mk(name),
         ~type_=Java_Field_Emit.emit_type(`Unsafe, field),
         ~prim=[],
       )
       |> psig_value;
     });

let emit_unsafe_class_type = t => {
  let (_functions, methods) = get_methods_by_kind(t);

  let inheritance_field = {
    let.some extends_id = t.extends;
    let extends_lid =
      Object_Type_Emit.emit_unsafe_lid(extends_id) |> Located.mk;
    let extends = pcty_constr(extends_lid, []);
    Some([pctf_inherit(extends)]);
  };

  let java_fields =
    t.fields
    |> List.map(({name, _} as field: java_field) =>
         pctf_method((
           Located.mk(name),
           Public,
           Concrete,
           Java_Field_Emit.emit_type(`Field, field),
         ))
       );
  let method_fields =
    methods
    |> List.map(({name, _} as method: java_method) =>
         pctf_method((
           Located.mk(name),
           Public,
           Concrete,
           Java_Method_Emit.emit_type(`Method, method),
         ))
       );
  let class_fields =
    List.concat([
      Option.value(~default=[], inheritance_field),
      java_fields,
      method_fields,
    ]);
  let class_signature = class_signature(~self=ptyp_any, ~fields=class_fields);
  let class_fun =
    pcty_arrow(Nolabel, [%type: Jni.obj], pcty_signature(class_signature));

  class_infos(
    ~virt=Concrete,
    ~params=[],
    ~name=Located.mk(unsafe_t),
    ~expr=class_fun,
  );
};
let emit_unsafe_type = t => {
  let declare_jni_class =
    value_description(
      ~name=Located.mk(jni_class_name),
      ~type_=[%type: unit => Jni.clazz],
      ~prim=[],
    )
    |> psig_value;
  let declare_fields = [%sigi:
    module Fields: {[%%s emit_fields_type(t.fields)];}
  ];
  let (functions, methods) = get_methods_by_kind(t);
  let declare_methods = [%sigi:
    module Methods: {[%%s emit_methods_type(`Unsafe, methods)];}
  ];
  let declare_functions = [%sigi:
    module Static: {[%%s emit_methods_type(`Unsafe, functions)];}
  ];

  let class_declaration = psig_class([emit_unsafe_class_type(t)]);
  let content = [
    declare_jni_class,
    declare_fields,
    declare_methods,
    declare_functions,
    class_declaration,
  ];
  [%sig: module Unsafe: {module Please: {module Stop: {[%%s content];};};}];
};
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
      emit_unsafe_type(t),
      static_methods,
      [psig_type(Recursive, [type_declaration])],
    ]);
  module_declaration(
    ~name=Located.mk(Some(t.name.name)),
    ~type_=pmty_signature(signature),
  );
};
