open Emit_Helper;
open Basic_types;
open Java_Type;
open Java_class;
open Structures;
open Lid;

let emit_fields = (env, t) =>
  t.fields
  |> List.map((field: java_field) => {
       let name = unsafe_name(field.name);
       pstr_value_alias(name, field.make_field(env));
     });

let emit_method = (env, method) =>
  method.call_jni(~clazz=evar(jni_class_name), env);
let emit_methods = (env, methods) =>
  methods
  |> List.map((method: java_method) => {
       let name = unsafe_name(method.name);
       pstr_value_alias(name, emit_method(env, method));
     });

let emit = (env, t) => {
  let declare_fields = [%stri
    module Fields = {
      %s
      emit_fields(env, t);
    }
  ];
  let declare_constructors = [%stri
    module Constructors = {
      %s
      emit_methods(env, t.constructors);
    }
  ];
  let declare_methods = [%stri
    module Methods = {
      %s
      emit_methods(env, t.methods);
    }
  ];
  let declare_functions = [%stri
    module Functions = {
      %s
      emit_methods(env, t.functions);
    }
  ];
  let include_class_declaration =
    loc(class_lid(t.java_name))
    |> pmod_ident
    |> include_infos
    |> pstr_include;
  let declare_class = [%stri
    module Class = {
      %s
      [include_class_declaration];
    }
  ];
  // TODO: this is a hackish solution, shouldn't be needed
  let declare_self_alias = {
    let alias = pmod_structure([unsafe_module([declare_class])]);
    module_binding(~name=loc(Some(t.name.name)), ~expr=alias) |> pstr_module;
  };

  let find_class = {
    let name = Object_Type.to_jvm_name(t.java_name) |> estring;
    pstr_value_alias(
      jni_class_name,
      [%expr () => Jni.find_class([%e name])],
    );
  };

  [
    declare_self_alias,
    find_class,
    declare_fields,
    declare_constructors,
    declare_methods,
    declare_functions,
    declare_class,
  ];
};
