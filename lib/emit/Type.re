open Emit_Helper;
open Basic_types;
open Java_class;
open Structures;
open Lid;

// TODO: duplicated
let emit_method = (kind, method: java_method) => {
  let name = kind == `Unsafe ? unsafe_name(method.name) : method.name;
  value_description(
    ~name=Located.mk(name),
    ~type_=method.signature,
    ~prim=[],
  )
  |> psig_value;
};
let emit_methods = (kind, methods) =>
  methods |> List.map(emit_method(kind));

let emit_fields = fields =>
  fields
  |> List.map((field: java_field) => {
       // TODO: duplicated code
       let name = field.static ? field.name : unsafe_name(field.name);
       value_description(
         ~name=Located.mk(name),
         ~type_=field.signature,
         ~prim=[],
       )
       |> psig_value;
     });
let emit_class_type = t => {
  let inheritance_field = {
    let.some extends_id = t.extends;
    let extends_lid = unsafe_class_lid(extends_id) |> Located.mk;
    let extends = pcty_constr(extends_lid, []);
    Some([pctf_inherit(extends)]);
  };

  let java_fields =
    t.fields
    |> List.map(({name, _} as field: java_field) =>
         pctf_method((Located.mk(name), Public, Concrete, field.signature))
       );
  let method_fields =
    t.methods
    |> List.map(({name, _} as method: java_method) =>
         pctf_method((Located.mk(name), Public, Concrete, method.signature))
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

let emit_unsafe = t => {
  let declare_jni_class =
    value_description(
      ~name=Located.mk(jni_class_name),
      ~type_=[%type: unit => Jni.clazz],
      ~prim=[],
    )
    |> psig_value;
  let declare_fields = [%sigi: module Fields: {[%%s emit_fields(t.fields)];}];
  let declare_constructors = [%sigi:
    module Constructors: {[%%s emit_methods(`Unsafe, t.constructors)];}
  ];
  let declare_methods = [%sigi:
    module Methods: {[%%s emit_methods(`Unsafe, t.methods)];}
  ];
  let declare_functions = [%sigi:
    module Functions: {[%%s emit_methods(`Unsafe, t.functions)];}
  ];
  let delcare_class = [%sigi:
    module Class: {[%%s [psig_class([emit_class_type(t)])]];}
  ];
  let content = [
    declare_jni_class,
    declare_fields,
    declare_constructors,
    declare_methods,
    declare_functions,
    delcare_class,
  ];
  unsafe_module_type(content);
};

// TODO: this is mostly duplicated code grr
let emit_method = (kind, method: java_method) => {
  let name = kind == `Unsafe ? unsafe_name(method.name) : method.name;
  value_description(
    ~name=Located.mk(name),
    ~type_=method.signature,
    ~prim=[],
  )
  |> psig_value;
};
let emit_methods = (kind, methods) =>
  methods |> List.map(emit_method(kind));

let emit = t => {
  let constructors = emit_methods(`Method, t.constructors);
  let functions = emit_methods(`Method, t.functions);
  let type_declaration = psig_type_alias("t", unsafe_class_lid(t.name));
  let signature =
    List.concat([
      [
        emit_unsafe(t),
        type_declaration,
        [%sigi: type sub('a) = {.. ...t} as 'a],
      ],
      constructors,
      functions,
    ]);
  module_declaration(
    ~name=Located.mk(Some(t.name.name)),
    ~type_=pmty_signature(signature),
  )
  |> psig_module;
};
