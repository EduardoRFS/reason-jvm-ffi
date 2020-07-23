open Fun;
open Emit_Helper;
open Basic_types;
open Structures;

let is_static = t => t.kind == `Function;

let find_required_classes = t =>
  List.concat_map(
    ((_, java_type, _)) => Java_Type.find_required_class(java_type),
    t.parameters,
  )
  @ Java_Type.find_required_class(t.return_type);

let emit_argument = ((name, java_type, _)) => {
  let identifier = evar(name);
  let read_expr =
    switch (java_type) {
    | Object(_) => get_unsafe_jobj(identifier)
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

let emit_jni_method_name = t =>
  Java_Type_Emit.emit_camljava_jni_to_call(
    `Method,
    t.kind == `Function,
    t.kind == `Constructor ? Void : t.return_type,
  );
let emit_method_call =
    (env, clazz_id, object_id, method_id, args, t: java_method) => {
  let args = args |> List.map(emit_argument) |> pexp_array;
  let call_method =
    eapply(
      emit_jni_method_name(t),
      [is_static(t) ? clazz_id : evar(object_id), method_id, args],
    );
  let returned_value =
    switch (t.kind) {
    | `Constructor =>
      let call_return =
        pexp_let(
          Nonrecursive,
          [value_binding(~pat=punit, ~expr=call_method)],
          evar(object_id),
        );
      let allocate_object =
        pexp_let(
          Nonrecursive,
          [
            value_binding(
              ~pat=pvar(object_id),
              ~expr=[%expr Jni.alloc_object([%e clazz_id])],
            ),
          ],
          call_return,
        );
      allocate_object;
    | _ => call_method
    };

  unsafe_cast_returned_value(env, t.return_type, returned_value);
};

let object_id = "this";
// TODO: escape these names + jni_class_name
let method_id = "jni_methodID";

let emit_jni_get_methodID = (jni_class_name, t: java_method) =>
  eapply(
    is_static(t) ? [%expr Jni.get_static_methodID] : [%expr Jni.get_methodID],
    [
      eapply(evar(jni_class_name), [eunit]),
      estring(t.java_name),
      t.java_signature |> estring,
    ],
  );

let emit = (jni_class_name, env, t) => {
  // TODO: duplicated code between type and code
  let parameters = {
    let parameters =
      t.parameters
      |> List.rev_map(((name, _, _)) => (Labelled(name), pvar(name)));
    let parameters =
      switch (parameters) {
      | [] => [(Nolabel, punit)]
      | parameters => parameters
      };
    let additional_parameter =
      switch (t.kind) {
      | `Constructor => []
      | `Method => [(Nolabel, pvar(object_id))]
      | `Function => [(Nolabel, pvar(jni_class_name))]
      };
    List.append(additional_parameter, parameters);
  };

  let method_call =
    emit_method_call(
      env,
      eapply(evar(jni_class_name), [eunit]),
      object_id,
      evar(method_id),
      t.parameters,
      t,
    );

  // parameters => method_call
  let declare_function = pexp_fun_helper(parameters, method_call);

  pexp_let_alias(
    method_id,
    emit_jni_get_methodID(jni_class_name, t),
    declare_function,
  );
};

let emit_type = (kind, t) => {
  let parameters = {
    let parameters =
      t.parameters
      |> List.mapi((index, (name, value, _)) =>
           (
             Labelled(name),
             Java_Type_Emit.emit_type(~kind=`Parameter(index), value),
           )
         )
      |> List.rev;

    let parameters =
      switch (parameters) {
      | [] => [(Nolabel, typ_unit)]
      | parameters => parameters
      };
    let additional_parameter =
      switch (kind, t.kind, is_static(t)) {
      | (_, `Constructor, _)
      | (`Method, _, _) => []
      | (`Unsafe, _, true) => [(Nolabel, [%type: unit => Jni.clazz])]
      | (`Unsafe, _, false) => [(Nolabel, [%type: Jni.obj])]
      };
    List.append(additional_parameter, parameters);
  };
  let return_type = Java_Type_Emit.emit_type(t.return_type);
  ptyp_arrow_helper(parameters, return_type);
};
