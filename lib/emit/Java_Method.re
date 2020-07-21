open Fun;
open Emit_Helper;
open Basic_types;
open Basic_structures;

let find_required_classes = t =>
  List.concat_map(
    ((_, java_type)) => Java_Type.find_required_class(java_type),
    t.parameters,
  )
  @ Java_Type.find_required_class(t.return_type);

let relativize = (clazz, t) => {
  let relativize = Java_Type.relativize(clazz);
  let parameters =
    t.parameters |> List.map(((name, value)) => (name, relativize(value)));
  let return_type = relativize(t.return_type);
  {...t, parameters, return_type};
};

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

let object_id = "this";
let emit = (jni_class_name, t) => {
  // TODO: escape these names + jni_class_name
  let method_id = "jni_methodID";

  let declare_method_id =
    eapply(
      [%expr Jni.get_methodID],
      [
        eapply(evar(jni_class_name), [eunit]),
        estring(t.java_name),
        t.java_signature |> estring,
      ],
    );
  let declare_function = {
    let arguments = t.parameters |> List.map(emit_argument) |> pexp_array;
    let call =
      emit_call(
        // TODO: proper solve the problem of recursion + jni class
        eapply(evar(jni_class_name), [eunit]),
        evar(object_id),
        evar(method_id),
        arguments,
        t,
      );
    // TODO: should we trust the Java return? I have a bad feeling on that
    let body =
      switch (t.return_type) {
      | Object(object_type) => new_unsafe_class(object_type, call)
      | Array(_) => failwith("TODO: too much work bro")
      | _ => call
      };

    // TODO: duplicated code between type and code
    let parameters =
      t.parameters
      |> List.rev_map(((name, _)) => (Labelled(name), pvar(name)));
    let parameters =
      switch (parameters) {
      | [] => [(Nolabel, punit)]
      | parameters => parameters
      };
    let additional_parameter =
      t.static
        ? [(Nolabel, pvar(jni_class_name))]
        : [(Nolabel, pvar(object_id))];
    let parameters = List.append(parameters, additional_parameter);

    List.fold_left(
      (acc, (label, parameter)) => pexp_fun(label, None, parameter, acc),
      body,
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

let emit_type = (kind, t) => {
  let parameters =
    t.parameters
    |> List.rev_map(((name, value)) =>
         (Labelled(name), Java_Type_Emit.emit_type(value))
       );
  let parameters =
    switch (parameters) {
    | [] => [(Nolabel, typ_unit)]
    | parameters => parameters
    };
  let additional_parameter =
    switch (kind, t.static) {
    | (`Method, _) => []
    | (`Unsafe, true) => [(Nolabel, [%type: unit => Jni.clazz])]
    | (`Unsafe, false) => [(Nolabel, [%type: Jni.obj])]
    };
  let parameters = List.append(parameters, additional_parameter);

  let return_type = Java_Type_Emit.emit_type(t.return_type);
  List.fold_left(
    (typ, (label, parameter)) => ptyp_arrow(label, parameter, typ),
    return_type,
    parameters,
  );
};
