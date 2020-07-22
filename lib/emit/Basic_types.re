[@deriving (show, eq, ord)]
type class_name = {
  package: list(string),
  name: string,
};

[@deriving show]
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
[@deriving show]
type java_field = {
  java_signature: string,
  name: string,
  static: bool,
  type_: java_type,
};

// TODO: access
[@deriving show]
type java_method = {
  java_name: string,
  java_signature: string,
  name: string,
  kind: [ | `Constructor | `Method | `Function],
  parameters: list((string, java_type)),
  return_type: java_type,
};

// TODO: access
[@deriving show]
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

module Structures = {
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

  let unsafe_class_cast = (class_name, jobj) => {
    let new_fn = pexp_new(unsafe_class_lid(class_name) |> loc);
    eapply(new_fn, [jobj]);
  };

  // TODO: should we trust the Java return? I have a bad feeling on that
  let unsafe_cast_returned_value = (return_type, returned_value) => {
    switch (return_type) {
    | Object(class_name) => unsafe_class_cast(class_name, returned_value)
    | Array(_) => failwith("TODO: too much work bro")
    | _ => returned_value
    };
  };
};

module Env = {
  module StringMap = Map.Make(String);
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

  let add_class =
      (
        ~fields=StringMap.empty,
        ~constructors=StringMap.empty,
        ~methods=StringMap.empty,
        ~functions=StringMap.empty,
        class_name,
        lid,
      ) => {
    open Structures;
    let unsafe_class = concat_lid([lid, unsafe_lid("unsafe_t")]);
    let unsafe_jni_clazz = concat_lid([lid, unsafe_lid("unsafe_jni_clazz")]);
    let value = {
      env_unsafe_class: unsafe_class,
      env_jni_class: unsafe_jni_clazz,
      env_fields: fields,
      env_constructors: constructors,
      env_methods: methods,
      env_functions: functions,
    };
    add(class_name, value);
  };
  let rec sub_lid = (a, b) =>
    Longident.(
      switch (a, b) {
      | (Ldot(content, name), Lident(b_name)) when name == b_name => content
      | (Ldot(content, name), Ldot(b_content, b_name)) when name == b_name =>
        sub_lid(content, b_content)
      | (a, _) => a
      }
    );

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
};
