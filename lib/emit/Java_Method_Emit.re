open Fun;
open Java_Type;
open Emit_Helper;

open Java_Method;

// TODO: should static have unsafe version without jclass applied?
let emit_call = (clazz_id, object_id, method_id, args, t) => {
  let id_to_call = {
    let type_name =
      switch (t.return_type) {
      | Object(_)
      | Array(_) => "object"
      | java_type => Java_Type.to_code_name(java_type)
      };
    let function_name =
      t.static
        ? "call_static_" ++ type_name ++ "_method"
        : "call_" ++ type_name ++ "_method";
    evar(~modules=["Jni"], function_name);
  };
  eapply(id_to_call, [t.static ? clazz_id : object_id, method_id, args]);
};

let emit_argument = ((name, java_type)) => {
  let identifier = evar(name);
  let read_expr =
    switch (java_type) {
    | Object(_) => id([%expr [%e identifier]#get_jni_jobj])
    | Array(_) => failwith("TODO: ")
    // | Array(_) => [%expr Jni.array_to_jobj([%e identifier])]
    | _ => identifier
    };

  switch (java_type) {
  | Void => failwith("void cannot be an argument")
  | Boolean => id([%expr Boolean([%e read_expr])])
  | Byte => id([%expr Byte([%e read_expr])])
  | Char => id([%expr Char([%e read_expr])])
  | Short => id([%expr Short([%e read_expr])])
  | Int => id([%expr Int([%e read_expr])])
  | Long => id([%expr Long([%e read_expr])])
  | Float => id([%expr Float([%e read_expr])])
  | Double => id([%expr Double([%e read_expr])])
  | Object(_)
  | Array(_) => id([%expr Obj([%e read_expr])])
  };
};

let emit = (jni_class_name, t) => {
  // TODO: escape these names + jni_class_name
  let method_id = "jni_methodID";
  let object_id = "jni_jobj";

  let declare_method_id = {
    let name = estring(t.name);
    let signature = to_jvm_signature(t) |> estring;
    %expr
    Jni.get_methodID([%e evar(jni_class_name)], [%e name], [%e signature]);
  };
  let declare_function = {
    let arguments = t.parameters |> List.map(emit_argument) |> pexp_array;
    let call =
      emit_call(
        evar(jni_class_name),
        evar(object_id),
        evar(method_id),
        arguments,
        t,
      );

    let parameters =
      t.parameters
      |> List.map(((name, _)) => (Labelled(name), pvar(name)));
    let last_parameter = t.static ? punit : pvar(object_id);
    let parameters = [(Nolabel, last_parameter), ...parameters];

    List.fold_left(
      (acc, (label, parameter)) => pexp_fun(label, None, parameter, acc),
      call,
      parameters,
    );
  };

  %expr
  {
    let [%p pvar(method_id)] = [%e declare_method_id];
    %e
    declare_function;
  };
};

let emit_type = (~is_method=false, t) => {
  let parameters =
    List.rev_map(
      ((key, value)) => (Labelled(key), Java_Type_Emit.emit_type(value)),
      t.parameters,
    );

  let return_type = Java_Type_Emit.emit_type(t.return_type);
  let method_type =
    switch (parameters) {
    | [] => ptyp_arrow(Nolabel, typ_unit, return_type)
    | [(label, parameter), ...parameters] =>
      List.fold_left(
        (fn, (label, parameter)) => ptyp_arrow(label, parameter, fn),
        ptyp_arrow(label, parameter, return_type),
        parameters,
      )
    };
  let additional_parameter =
    ptyp_constr(lident(~modules=["Jni"], "obj") |> Located.mk, []);
  let method_type =
    is_method
      ? method_type : ptyp_arrow(Nolabel, additional_parameter, method_type);
  method_type;
};
