open Basic_types;
open Emit_Helper;
open Java_Type;
open Java_Type_Emit;
open Java_class;

// TODO: duplicated
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
let emit_field = Java_Field.emit(jni_class_name);
let emit_fields = t =>
  t.fields
  |> List.map((field: java_field) => {
       let name = unsafe_name(field.name);
       [%stri let [%p pvar(name)] = [%e emit_field(field)]];
     });
let emit_method = Java_Method.emit(jni_class_name);
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

let emit = t => {
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

let emit_fields_type = fields =>
  fields
  |> List.map((field: java_field) => {
       // TODO: duplicated code
       let name = field.static ? field.name : unsafe_name(field.name);
       value_description(
         ~name=Located.mk(name),
         ~type_=Java_Field.emit_type(`Unsafe, field),
         ~prim=[],
       )
       |> psig_value;
     });
let emit_class_type = t => {
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
           Java_Field.emit_type(`Field, field),
         ))
       );
  let method_fields =
    methods
    |> List.map(({name, _} as method: java_method) =>
         pctf_method((
           Located.mk(name),
           Public,
           Concrete,
           Java_Method.emit_type(`Method, method),
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
  let (functions, methods) = get_methods_by_kind(t);
  let declare_methods = [%sigi:
    module Methods: {[%%s emit_methods_type(`Unsafe, methods)];}
  ];
  let declare_functions = [%sigi:
    module Static: {[%%s emit_methods_type(`Unsafe, functions)];}
  ];

  let class_declaration = psig_class([emit_class_type(t)]);
  let content = [
    declare_jni_class,
    declare_fields,
    declare_methods,
    declare_functions,
    class_declaration,
  ];
  [%sig: module Unsafe: {module Please: {module Stop: {[%%s content];};};}];
};
