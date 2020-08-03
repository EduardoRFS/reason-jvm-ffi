open Emit_Helper;
open Basic_types;
open Java_class;
open Lid;

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
let emit = t => {
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
