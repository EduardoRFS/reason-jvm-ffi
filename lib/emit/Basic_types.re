[@deriving (eq, ord)]
type class_name = {
  package: list(string),
  name: string,
};

type java_type =
  // TODO: remove void
  | Void
  | Boolean
  | Byte
  | Char
  | Short
  | Int
  // TODO: maybe Camlint?
  | Long
  | Float
  | Double
  | Object(class_name)
  | Array(java_type);

// TODO: access, final and static
type java_field = {
  java_signature: string,
  name: string,
  static: bool,
  type_: java_type,
};

type parameter = {
  label: Asttypes.arg_label,
  pat: Parsetree.pattern,
  expr: Parsetree.expression,
  typ: Parsetree.core_type,
  jni_argument: option(Parsetree.expression),
  java_type // TODO: remove this in the future
};

// TODO: access
type java_method = {
  java_name: string,
  java_signature: string,
  name: string,
  kind: [ | `Constructor | `Method | `Function],
  this: option(parameter),
  parameters: list(parameter),
  return_type: java_type,
};

// TODO: access
type java_class = {
  java_name: class_name,
  name: class_name,
  extends: option(class_name),
  fields: list(java_field),
  functions: list(java_method),
  constructors: list(java_method),
  methods: list(java_method),
};

module Java_Env =
  Map.Make({
    type t = class_name;
    let compare = compare_class_name;
  });

module Lid = {
  open Emit_Helper;
  // TODO: do this properly because apply
  let concat_lid = lids =>
    lids |> List.map(Longident.name) |> String.concat(".") |> Longident.parse;

  // TODO: do this properly, what if package is empty?
  let package_lid = package =>
    package
    |> List.map(String.capitalize_ascii)
    |> String.concat(".")
    |> Longident.parse;
  let class_lid = ({name, package}) =>
    concat_lid(
      package == []
        ? [Lident(name)] : [package_lid(package), Lident(name)],
    );
  let module_type_lid = name =>
    concat_lid([Lident("Javatype"), class_lid(name)]);

  let unsafe_name = name => "unsafe_" ++ name;
  let unsafe_lid = name => Lident(unsafe_name(name));
  let unsafe_module_lid = Longident.parse("Unsafe.Please.Stop");
  let unsafe_module_type_lid = name =>
    concat_lid([module_type_lid(name), unsafe_module_lid]);
  let unsafe_class_lid = class_name =>
    concat_lid([
      class_lid(class_name),
      unsafe_module_lid,
      Lident("Class"),
      unsafe_lid("t"),
    ]);
};
module Env = {
  module StringMap = Map.Make(String);
  let string_map_of_pairs = list => list |> List.to_seq |> StringMap.of_seq;
  include Map.Make({
    type t = class_name;
    let compare = compare_class_name;
  });

  type value = {
    env_unsafe_class: Longident.t,
    env_jni_class: Longident.t,
    env_fields: StringMap.t(Longident.t),
    env_constructors: StringMap.t(Longident.t),
    env_methods: StringMap.t(Longident.t),
    env_functions: StringMap.t(Longident.t),
  };

  open Lid;
  let update_name = (value, f, name, t) =>
    // TODO: what if doesn't exists
    t |> add(name, f(value, find(name, t)));
  let set_fields = (fields, lid) => {
    let env_fields =
      fields
      |> List.map((field: java_field) =>
           (field.name, concat_lid([lid, unsafe_lid(field.name)]))
         )
      |> string_map_of_pairs;
    update_name(env_fields, (env_fields, t) => {...t, env_fields});
  };
  let update_methods = (map, methods, lid) => {
    let env_methods =
      methods
      |> List.map((method: java_method) =>
           (method.name, concat_lid([lid, unsafe_lid(method.name)]))
         )
      |> string_map_of_pairs;
    update_name(env_methods, map);
  };
  let set_constructors =
    update_methods((env_constructors, t) => {...t, env_constructors});
  let set_methods = update_methods((env_methods, t) => {...t, env_methods});
  let set_functions =
    update_methods((env_functions, t) => {...t, env_functions});

  let add_empty_class = (class_name, lid) => {
    let unsafe_class = concat_lid([lid, unsafe_lid("t")]);
    let unsafe_jni_clazz = concat_lid([lid, unsafe_lid("jni_clazz")]);
    let value = {
      env_unsafe_class: unsafe_class,
      env_jni_class: unsafe_jni_clazz,
      env_fields: StringMap.empty,
      env_constructors: StringMap.empty,
      env_methods: StringMap.empty,
      env_functions: StringMap.empty,
    };
    add(class_name, value);
  };
  let add_class = (~class_lid=None, clazz: java_class, lid, t) => {
    let name = clazz.name;
    let lid = s => concat_lid([lid, Lident(s)]);
    let class_lid =
      switch (class_lid) {
      | Some(class_lid) => class_lid
      | None => lid("Classs")
      };
    t
    |> add_empty_class(name, class_lid)
    |> set_fields(clazz.fields, lid("Fields"), name)
    |> set_constructors(clazz.constructors, lid("Constructors"), name)
    |> set_methods(clazz.methods, lid("Methods"), name)
    |> set_functions(clazz.functions, lid("Functions"), name);
  };

  let sub_lid = (a: Longident.t, b: Longident.t) => {
    // TODO: this code is terrible
    let rec starts_with = (a, b) =>
      switch (a, b) {
      | (_, []) => None
      | ([], rest) => Some(rest)
      | ([a, ...a_rest], [b, ...b_rest]) when a == b =>
        starts_with(a_rest, b_rest)
      | _ => None
      };
    let a = Longident.flatten(a);
    let b = Longident.flatten(b);
    let match = a |> starts_with(b) |> Option.value(~default=a);
    Longident.unflatten(match) |> Option.get;
  };

  let open_lid_value = (to_open, value) => {
    let sub_lid = a => sub_lid(a, to_open);
    let sub_lid_map = str_map => str_map |> StringMap.map(sub_lid);
    {
      env_unsafe_class: sub_lid(value.env_unsafe_class),
      env_jni_class: sub_lid(value.env_jni_class),
      env_fields: sub_lid_map(value.env_fields),
      env_constructors: sub_lid_map(value.env_constructors),
      env_methods: sub_lid_map(value.env_methods),
      env_functions: sub_lid_map(value.env_functions),
    };
  };
  let open_lid = (to_open, t) => t |> map(open_lid_value(to_open));

  let unsafe_class_lid = (class_name, t) => {
    let value = t |> find(class_name);
    value.env_unsafe_class;
  };
  let jni_class_lid = (class_name, t) => {
    let value = t |> find(class_name);
    value.env_jni_class;
  };
  let find_lid = (f, class_name, name, t) => {
    let value = t |> find(class_name);
    f(value) |> StringMap.find(name);
  };
  let field_lid = find_lid(value => value.env_fields);
  let constructor_lid = find_lid(value => value.env_constructors);
  let method_lid = find_lid(value => value.env_methods);
  let function_lid = find_lid(value => value.env_functions);
};

module Structures = {
  open Env;
  open Emit_Helper;

  let unsafe_module = content => [%stri
    module Unsafe = {
      module Please = {
        module Stop = {
          %s
          content;
        };
      };
    }
  ];
  let unsafe_module_type = content => [%sigi:
    module Unsafe: {module Please: {module Stop: {[%%s content];};};}
  ];

  let get_unsafe_jobj = id => pexp_send(id, loc("get_jni_jobj"));

  let unsafe_class_cast = (env, class_name, jobj) => {
    let lid = unsafe_class_lid(class_name, env) |> loc;
    eapply(pexp_new(lid), [jobj]);
  };

  // TODO: should we trust the Java return? I have a bad feeling on that
  let unsafe_cast_returned_value = (env, return_type, returned_value) => {
    switch (return_type) {
    | Object(class_name) =>
      unsafe_class_cast(env, class_name, returned_value)
    | Array(_) => failwith("TODO: too much work bro")
    | _ => returned_value
    };
  };
};
