open Emit_Helper;
open Basic_types;
open Java_Type;
open Java_class;
open Structures;
open Lid;

// TODO: duplicated
let emit_method_type = (kind, method: java_method) => {
  let name = kind == `Unsafe ? unsafe_name(method.name) : method.name;
  value_description(
    ~name=Located.mk(name),
    ~type_=method.signature,
    ~prim=[],
  )
  |> psig_value;
};
let emit_methods_type = (kind, methods) =>
  methods |> List.map(emit_method_type(kind));
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

let emit_class = (env, t) => {
  let java_fields =
    t.fields
    |> List.map(({name, _}: java_field) =>
         pcf_method((
           Located.mk(name),
           Public,
           Cfk_concrete(
             Fresh,
             eapply(
               Env.field_lid(t.name, name, env) |> loc |> pexp_ident,
               [evar(object_id)],
             ),
           ),
         ))
       );
  let method_fields =
    t.methods
    |> List.map(({name, _} as method: java_method) =>
         pcf_method((
           Located.mk(name),
           Public,
           Cfk_concrete(Fresh, emit_curried_method(t.name, env, method)),
         ))
       );
  let inheritance_field = {
    let.some extends_id = t.extends;
    let extends_lid = env |> Env.unsafe_t_lid(extends_id) |> loc;
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
  let class_declaration =
    class_infos(
      ~virt=Concrete,
      ~params=[],
      ~name=Located.mk(unsafe_t),
      ~expr=class_fun // TODO: wait, class_infos without class_expr?
    );
  pstr_class([class_declaration]);
};

let emit_class_functor_env = t => {
  let params_lid = Lident("Params");
  let env = Env.empty;
  let env =
    t.extends
    |> Option.map((extends: class_name) => {
         let lid = concat_lid([params_lid, Lident(extends.name)]);
         env |> Env.add_empty_class(extends, lid);
       })
    |> Option.value(~default=env);

  let fields_lid = concat_lid([params_lid, Lident("Fields")]);
  let methods_lid = concat_lid([params_lid, Lident("Methods")]);
  env
  |> Env.add_empty_class(t.name, Lident("Invalid"))
  |> Env.set_fields(t.fields, fields_lid, t.name)
  |> Env.set_methods(t.methods, methods_lid, t.name);
};
let emit_class_functor_parameters_type = t => {
  let parent = {
    let.some extends = t.extends;
    let parent_name = extends.name;
    let parent_type_module =
      concat_lid([class_lid(extends), unsafe_module_lid, Lident("Class")]);
    Some([psig_module_alias_module(parent_name, parent_type_module)]);
  };
  let fields =
    psig_module_alias_module(
      "Fields",
      concat_lid([unsafe_module_type_lid(t.java_name), Lident("Fields")]),
    );
  let methods =
    psig_module_alias_module(
      "Methods",
      concat_lid([unsafe_module_type_lid(t.java_name), Lident("Methods")]),
    );
  List.append(Option.value(~default=[], parent), [fields, methods])
  |> pmty_signature;
};
let emit_class_functor = t => {
  let env = emit_class_functor_env(t);
  let env = env |> Env.open_lid(Lident("Params"));

  let content = emit_class(env, t);
  let parameters =
    Named(
      Located.mk(Some("Params")),
      emit_class_functor_parameters_type(t),
    );
  let content = [[%stri open Params], content];
  // useful to ensure the generated code complies with the type definition
  let mod_functor = pmod_functor(parameters, pmod_structure(content));
  module_binding(~name=Located.mk(Some("Class")), ~expr=mod_functor)
  |> pstr_module;
};

let emit_fields_type = fields =>
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
let emit_type = t => {
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
  let declare_constructors = [%sigi:
    module Constructors: {[%%s emit_methods_type(`Unsafe, t.constructors)];}
  ];
  let declare_methods = [%sigi:
    module Methods: {[%%s emit_methods_type(`Unsafe, t.methods)];}
  ];
  let declare_functions = [%sigi:
    module Functions: {[%%s emit_methods_type(`Unsafe, t.functions)];}
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
