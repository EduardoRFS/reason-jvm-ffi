[@deriving (eq, ord)]
type class_name = {
  package: list(string),
  name: string,
};

module EnvMap = {
  module StringMap = Map.Make(String);
  include Map.Make({
    type t = class_name;
    let compare = compare_class_name;
  });
  let string_map_of_pairs = list => list |> List.to_seq |> StringMap.of_seq;
  type value = Longident.t;
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
  signature: Parsetree.core_type,
  make_field: EnvMap.t(EnvMap.value) => Parsetree.expression,
  name: string,
  static: bool,
};

type parameter = {
  label: Asttypes.arg_label,
  pat: Parsetree.pattern,
  expr: Parsetree.expression,
  typ: Parsetree.core_type,
  jni_argument: option(Parsetree.expression),
};

// TODO: access
type java_method = {
  required_classes: list(class_name),
  signature: Parsetree.core_type,
  call_jni:
    (~clazz: Parsetree.expression, EnvMap.t(EnvMap.value)) =>
    Parsetree.expression,
  name: string,
  kind: [ | `Constructor | `Method | `Function],
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

  module Name = {
    let unsafe_jni_clazz = concat_lid([unsafe_module_lid, Lident("unsafe_jni_clazz")]);
    let unsafe_t = concat_lid([unsafe_module_lid, Lident("unsafe_t")]);
    let field = concat_lid([unsafe_module_lid, Lident("Field")]);
    let constructor = concat_lid([unsafe_module_lid, Lident("Constructor")]);
    let method_ = concat_lid([unsafe_module_lid, Lident("Method")]);
    let function_ = concat_lid([unsafe_module_lid, Lident("Function")]);
    let t = Lident("t");
    let sub = Lident("sub");
  };
};

module Env = {
  include EnvMap;
  
  let update_name = (value, f, name, t) =>
    // TODO: what if doesn't exists
    t |> add(name, f(value, find(name, t)));

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

  let open_lid = (to_open, t) => t |> map(lid => sub_lid(lid, to_open));

  let access = (lid, class_name, t) => Lid.concat_lid([t |> find(class_name), lid]);


  let clazz_lid = access(Lid.Name.unsafe_jni_clazz);
  let unsafe_t_lid = access(Lid.Name.unsafe_t);
  let t_lid = access(Lid.Name.t);
  let sub_lid = access(Lid.Name.sub);

  let find_lid = (base, class_name, name, t) => {
    let module_lid = t |> find(class_name);
    Lid.concat_lid([module_lid, base, Lident(name)]);
  };
  let field_lid = find_lid(Lid.Name.field);
  let constructor_lid = find_lid(Lid.Name.constructor);
  let method_lid = find_lid(Lid.Name.method_);
  let function_lid = find_lid(Lid.Name.function_);
};

module Structures = {
  open Env;
  open Emit_Helper;

  let unsafe_module = content => {
    let stop = pstr_module_alias("Stop", content);
    let please = pstr_module_alias("Please", pmod_structure([stop]));
    pstr_module_alias("Unsafe", pmod_structure([please]));
  };
  let unsafe_module_type = content => [%sigi:
    module Unsafe: {module Please: {module Stop: {[%%s content];};};}
  ];

  let get_unsafe_jobj = id => pexp_send(id, loc("get_jni_jobj"));

  let unsafe_class_cast = (env, class_name, jobj) => {
    let lid = unsafe_t_lid(class_name, env) |> loc;
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
