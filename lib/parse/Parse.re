open Reason_java_ffi_lib_emit;
open Java_Type;
open Object_Type;

open Javalib_pack;
open JBasics;
open Javalib;

let (let.some) = Option.bind;

let class_path = name => {
  let class_path = class_path(name);
  Gc.finalise(close_class_path, class_path);
  class_path;
};

let class_name_to_object_type = class_name => {
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
    let name = ms_name(signature);
    let static = concrete_method.cm_static;
    let variable_table =
      switch (concrete_method.cm_implementation) {
      | Native => []
      | Java(jcode) =>
        open JCode;
        let jcode = Lazy.force(jcode);
        jcode.c_local_variable_table |> Option.value(~default=[]);
      };
    let parameters =
      ms_args(signature)
      |> List.mapi((index, value_type) => {
           // TODO: is that right for static functions?
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
    let return_type =
      ms_rtype(signature)
      |> Option.map(value_type_to_java_type)
      |> Option.value(~default=Void);
    Java_Method.{name, static, parameters, return_type};
  };
let jclass_to_java_class = jclass => {
  let id = class_name_to_object_type(jclass.c_name);
  let extends = jclass.c_super_class |> Option.map(class_name_to_object_type);
  let methods =
    jclass.c_methods
    |> MethodMap.value_elements
    |> List.map(jmethod_to_java_method);
  Java_Class.{id, extends, methods};
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
             (java_class.Java_Class.id, java_class);
           },
       );

  let env = classes |> List.to_seq |> Java_Env.of_seq;
  let package = Java_Package.of_env(".", env);
  (env, package);
};
