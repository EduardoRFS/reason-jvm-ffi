open Emit_Helper;
open Java_Type;
open Java_Type_Emit;
open Java_Class;

// TODO: keep same method order as in the bytecode

let get_methods_by_kind = t =>
  t.methods |> List.partition((Java_Method.{static, _}) => static);

let jni_class_name = "unsafe_jni_class";
let object_id = "jni_jobj";

let emit_field = Java_Field_Emit.emit(jni_class_name);
let emit_fields = t =>
  t.fields
  |> List.map(field => {
       open Java_Field;
       let name = unsafe_name(field.name);
       [%stri let [%p pvar(name)] = [%e emit_field(field)]];
     });
let emit_method = Java_Method_Emit.emit(jni_class_name);
let emit_methods = methods =>
  methods
  |> List.map(method => {
       open Java_Method;
       let name = unsafe_name(method.name);
       [%stri let [%p pvar(name)] = [%e emit_method(method)]];
     });

let emit_unsafe_class = t => {
  let (_functions, methods) = get_methods_by_kind(t);

  let method_fields =
    methods
    |> List.map(({Java_Method.name, _}) =>
         pcf_method((
           Located.mk(name),
           Public,
           Cfk_concrete(
             Fresh,
             eapply(evar(unsafe_name(name)), [evar(object_id)]),
           ),
         ))
       );
  let inheritance_field = {
    let.some extends_id = t.extends;
    let extends_lid =
      Object_Type_Emit.emit_unsafe_lid(extends_id) |> Located.mk;
    let extends = pcl_constr(extends_lid, []);
    let apply_class = pcl_apply(extends, [(Nolabel, evar(object_id))]);
    Some(pcf_inherit(Fresh, apply_class, None));
  };
  let fields =
    List.concat([
      List.filter_map(Fun.id, [inheritance_field]),
      method_fields,
    ]);
  let class_expr = class_structure(~self=ppat_any, ~fields) |> pcl_structure;
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
    let name = Object_Type.to_jvm_name(t.id) |> estring;
    [%stri let [%p pvar(jni_class_name)] = Jni.find_class([%e name])];
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
  let static_methods =
    List.filter(({Java_Method.static, _}) => static, t.methods);
  let static_methods = emit_methods(static_methods);

  let content = [%str
    [@ocaml.warning "-33"]
    open Javatype;
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
    static_methods
  ];
  // TODO: hardcoded Javatype
  let parameter =
    Named(Located.mk(Some("Javatype")), emit_functor_parameters_type(t));
  let mod_functor = pmod_functor(parameter, pmod_structure(content));
  module_binding(~name=Located.mk(Some("Make")), ~expr=mod_functor)
  |> pstr_module;
};
let emit_file = t => [%str
  open JavaFFI;
  %s
  [emit_functor(t)]
];

// TODO: this is mostly duplicated code grr
let emit_method_type = (kind, method) => {
  open Java_Method;
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
  |> List.map(field => {
       open Java_Field;
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
    |> List.map(({Java_Field.name, _} as field) =>
         pctf_method((
           Located.mk(name),
           Public,
           Concrete,
           Java_Field_Emit.emit_type(`Field, field),
         ))
       );
  let method_fields =
    methods
    |> List.map(({Java_Method.name, _} as method) =>
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
      ~type_=[%type: Jni.clazz],
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
    List.filter(({Java_Method.static, _}) => static, t.methods);
  let static_methods = emit_methods_type(`Method, static_methods);
  let type_declaration = {
    let alias =
      ptyp_constr(
        Located.mk(Java_Type_Emit.Object_Type_Emit.emit_unsafe_lid(t.id)),
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
    ~name=Located.mk(Some(t.id.name)),
    ~type_=pmty_signature(signature),
  );
};
