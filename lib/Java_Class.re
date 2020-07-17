open Emit_Helper;
open Java_Type;

let (let.some) = Option.bind;

type id = Object_Type.t;

type t = {
  id,
  extends: option(id),
  // TODO: fields
  methods: list(Java_Method.t),
};

// TODO: class name to snake case
// TODO: escape override method name

let jni_class_name = "unsafe_jni_class";
let emit_method = Java_Method.emit(jni_class_name);
let emit_methods = methods =>
  Java_Method.(
    List.map(
      method => {
        let name = method.static ? method.name : unsafe_name(method.name);
        [%stri let [%p pvar(name)] = [%e emit_method(method)]];
      },
      methods,
    )
  );
let emit_unsafe = t => {
  let object_id = "jni_jobj";
  let methods =
    List.filter(({Java_Method.static, _}) => !static, t.methods);
  let declare_methods = emit_methods(methods);

  let method_fields =
    List.map(
      ({Java_Method.name, _}) =>
        pcf_method((
          Located.mk(name),
          Public,
          Cfk_concrete(
            Fresh,
            eapply(evar(unsafe_name(name)), [evar(object_id)]),
          ),
        )),
      methods,
    );
  let inheritance_field = {
    let.some extends_id = t.extends;
    let extends_lid = Object_Type.emit_unsafe_lid(extends_id) |> Located.mk;
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
  let class_declaration =
    class_infos(
      ~virt=Concrete,
      ~params=[],
      ~name=Located.mk(unsafe_t),
      ~expr=class_fun // TODO: wait, class_infos without class_expr?
    );
  let find_class = {
    let name = Object_Type.to_jvm_name(t.id) |> estring;
    [%stri let [%p pvar(jni_class_name)] = Jni.find_class([%e name])];
  };
  List.append(
    [find_class, ...declare_methods],
    [pstr_class([class_declaration])],
  );
};
let emit_module = t => {
  let static_methods =
    List.filter(({Java_Method.static, _}) => static, t.methods);
  let static_methods = emit_methods(static_methods);

  [%str
    module Unsafe = {
      module Please = {
        module Stop = {
          %s
          emit_unsafe(t);
        };
      };
    };
    open Unsafe.Please.Stop;
    %s
    static_methods
  ];
};

// TODO: this is mostly duplicated code grr
let emit_methods_type = methods =>
  Java_Method.(
    List.map(
      method => {
        let name = method.static ? method.name : unsafe_name(method.name);
        value_description(
          ~name=Located.mk(name),
          ~type_=Java_Method.emit_type(method),
          ~prim=[],
        )
        |> psig_value;
      },
      methods,
    )
  );
let emit_unsafe_type = t => {
  let jni_class_identifier =
    value_description(
      ~name=Located.mk(jni_class_name),
      ~type_=[%type: Jni.clazz],
      ~prim=[],
    )
    |> psig_value;
  let methods =
    List.filter(({Java_Method.static, _}) => !static, t.methods);
  let declare_methods = emit_methods_type(methods);
  let method_fields =
    List.map(
      ({Java_Method.name, _} as method) =>
        pctf_method((
          Located.mk(name),
          Public,
          Concrete,
          Java_Method.emit_type(~is_method=true, method),
        )),
      methods,
    );
  let inheritance_field = {
    let.some extends_id = t.extends;
    let extends_lid = Object_Type.emit_unsafe_lid(extends_id) |> Located.mk;
    let extends = pcty_constr(extends_lid, []);
    Some(pctf_inherit(extends));
  };
  let fields =
    List.concat([
      List.filter_map(Fun.id, [inheritance_field]),
      method_fields,
    ]);
  let class_signature = class_signature(~self=ptyp_any, ~fields);
  let class_fun =
    pcty_arrow(Nolabel, [%type: Jni.obj], pcty_signature(class_signature));

  let class_declaration =
    class_infos(
      ~virt=Concrete,
      ~params=[],
      ~name=Located.mk(unsafe_t),
      ~expr=class_fun,
    );
  List.append(
    [jni_class_identifier, ...declare_methods],
    [psig_class([class_declaration])],
  );
};
let emit_module_type = t => {
  let static_methods =
    List.filter(({Java_Method.static, _}) => static, t.methods);
  let static_methods = emit_methods_type(static_methods);
  [%sig:
    module Unsafe: {
      module Please: {module Stop: {[%%s emit_unsafe_type(t)];};};
    };
    [%%s static_methods]
  ];
};
