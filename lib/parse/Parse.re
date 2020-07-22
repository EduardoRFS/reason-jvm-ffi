open Reason_java_ffi_lib_emit;
open Basic_types;

open Javalib_pack;
open JBasics;
open Javalib;

let (let.some) = Option.bind;

let class_path = name => {
  let class_path = class_path(name);
  Gc.finalise(close_class_path, class_path);
  class_path;
};

let class_name_to_object_type = (class_name): Basic_types.class_name => {
  package: cn_package(class_name),
  name: cn_simple_name(class_name),
};
let rec value_type_to_java_type =
  fun
  | TBasic(`Bool) => Boolean
  | TBasic(`Byte) => Byte
  | TBasic(`Char) => Char
  | TBasic(`Short) => Short
  | TBasic(`Int) => Int
  | TBasic(`Long) => Long
  | TBasic(`Float) => Float
  | TBasic(`Double) => Double
  | TObject(TClass(name)) => Object(class_name_to_object_type(name))
  | TObject(TArray(value_type)) =>
    Array(value_type_to_java_type(value_type));

let jmethod_to_java_method = jmethod =>
  switch (jmethod) {
  | AbstractMethod(_) => failwith("sorry man, not abstract here")
  | ConcreteMethod(concrete_method) =>
    let signature = concrete_method.cm_signature;

    let java_name = ms_name(signature);
    let java_signature =
      JPrint.method_signature(~jvm=true, signature)
      |> String.split_on_char(':')
      |> List.tl
      |> String.concat(":");
    let static = concrete_method.cm_static;
    let kind =
      switch (static, java_name) {
      | (true, _) => `Function
      | (false, "<init>") => `Constructor
      | (false, _) => `Method
      };
    let variable_table =
      switch (concrete_method.cm_implementation) {
      | Native => []
      | Java(jcode) =>
        open JCode;
        let jcode = Lazy.force(jcode);
        jcode.c_local_variable_table |> Option.value(~default=[]);
      };
    let java_parameters =
      ms_args(signature)
      |> List.mapi((index, value_type) => {
           // on non-static 0 is this, so that's why the offset
           let table_index = static ? index : index + 1;
           let name = {
             let.some (_, _, name, received_value_type, _) =
               List.nth_opt(variable_table, table_index);
             if (value_type != received_value_type) {
               failwith("that is weird, look at value_type here");
             };
             Some(name);
           };
           let name =
             Option.value(~default="param_" ++ string_of_int(index), name);
           let java_type = value_type_to_java_type(value_type);
           (name, java_type);
         });
    let java_return_type =
      ms_rtype(signature)
      |> Option.map(value_type_to_java_type)
      |> Option.value(~default=Void);
    Java_Method.{
      java_name,
      java_signature,
      name: java_name,
      kind,
      parameters: java_parameters,
      return_type: java_return_type,
    };
  };
// TODO: methods and functions should be separated
let escape_duplicated_names = (compare, transform, list) =>
  List.fold_left(
    (new_list, item) => {
      let has_duplicated = {
        let occurrences = list |> List.filter(compare(item)) |> List.length;
        occurrences >= 2;
      };
      let counter = new_list |> List.filter(compare(item)) |> List.length;
      let item = has_duplicated ? transform(counter, item) : item;
      [item, ...new_list];
    },
    [],
    list,
  );

let class_field_to_java_field = class_field => {
  let signature = class_field.cf_signature;
  // TODO: find a better alternative to this
  let java_signature =
    JPrint.field_signature(~jvm=true, signature)
    |> String.split_on_char(':')
    |> List.tl
    |> String.concat(":");
  let name = fs_name(signature);
  let static = class_field.cf_static;
  let java_type = fs_type(signature) |> value_type_to_java_type;
  {java_signature, name, static, type_: java_type};
};
let jclass_to_java_class = jclass => {
  let java_name = class_name_to_object_type(jclass.c_name);
  let extends = jclass.c_super_class |> Option.map(class_name_to_object_type);
  let fields =
    jclass.c_fields
    |> FieldMap.value_elements
    |> List.map(class_field_to_java_field);

  let methods =
    jclass.c_methods
    |> MethodMap.value_elements
    |> List.map(jmethod_to_java_method)
    |> escape_duplicated_names(
         (method: java_method, method') =>
           method.java_name == method'.java_name,
         (index, method) =>
           {
             ...method,
             name: method.java_name ++ "_" ++ string_of_int(index),
           },
       );
  let (functions, constructors, methods) = {
    let (functions, methods) =
      methods
      |> List.partition((method: java_method) => method.kind == `Function);
    let (constructors, methods) =
      methods
      |> List.partition((method: java_method) => method.kind == `Constructor);
    (functions, constructors, methods);
  };
  {
    java_name,
    name: java_name,
    extends,
    fields,
    functions,
    constructors,
    methods,
  };
};

let create_env_and_package = (folder, classes) => {
  let class_path = class_path(folder);
  let classes =
    classes
    |> List.map(make_cn)
    |> List.map(get_class(class_path))
    |> List.map(
         fun
         | JInterface(_) => failwith("currently not supported")
         | JClass(jclass) => {
             let java_class = jclass_to_java_class(jclass);
             (java_class.name, java_class);
           },
       );

  let env = classes |> List.to_seq |> Java_Env.of_seq;
  let package = Java_Package.of_env(".", env);
  (env, package);
};
