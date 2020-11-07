open Reason_jvm_ffi_ir;
open Javalib_pack;
open JBasics;
open JClass;
open JFile;

// TODO: keep same order as Java for methods and fields

let (let.some) = Option.bind;

let parse_class_name = name =>
  cn_package(name) @ [cn_simple_name(name)] |> String.concat("/");
let rec parse_value_type =
  fun
  | TBasic(`Bool) => Boolean
  | TBasic(`Byte) => Byte
  | TBasic(`Char) => Char
  | TBasic(`Short) => Short
  | TBasic(`Int) => Int
  | TBasic(`Long) => Long
  | TBasic(`Float) => Float
  | TBasic(`Double) => Double
  | TObject(TClass(name)) => Object(parse_class_name(name))
  | TObject(TArray(value_type)) => Array(parse_value_type(value_type));

let parse_class_field = (classpath, field) => {
  let signature = field.cf_signature;
  let jf_name = fs_name(signature);
  let jf_type = fs_type(signature) |> parse_value_type;
  let jf_final =
    switch (field.cf_kind) {
    | NotFinal => false
    | Final => true
    | Volatile => failwith("Not implemented volatile")
    };
  let jf_static = field.cf_static;
  {jf_classpath: classpath, jf_name, jf_type, jf_final, jf_static};
};
let parse_jmethod = (jm_classpath, jmethod) =>
  switch (jmethod) {
  | AbstractMethod(abstract_method) =>
    let signature = abstract_method.am_signature;

    let jm_name = ms_name(signature);
    let jm_parameters =
      // TODO: use source to get naming for abstract signatures
      ms_args(signature)
      |> List.map(parse_value_type)
      |> List.map(jvm_type => (None, jvm_type));
    let jm_return = ms_rtype(signature) |> Option.map(parse_value_type);
    let jm_abstract = true;
    let jm_kind =
      switch (jm_name) {
      | "<init>" => `Constructor
      | _ => `Method
      };
    {jm_classpath, jm_name, jm_parameters, jm_return, jm_abstract, jm_kind};
  | ConcreteMethod(concrete_method) =>
    let signature = concrete_method.cm_signature;

    let jm_name = ms_name(signature);
    let jm_static = concrete_method.cm_static;
    let jm_kind =
      switch (jm_static, jm_name) {
      | (true, _) => `Function
      | (false, "<init>") => `Constructor
      | (false, _) => `Method
      };
    let jm_return = ms_rtype(signature) |> Option.map(parse_value_type);
    let (variable_table, variable_type_table) =
      switch (concrete_method.cm_implementation) {
      | Native => ([], [])
      | Java(jcode) =>
        open JCode;
        let jcode = Lazy.force(jcode);
        let variable_table =
          jcode.c_local_variable_table |> Option.value(~default=[]);
        let variable_type_table =
          jcode.c_local_variable_type_table |> Option.value(~default=[]);
        (variable_table, variable_type_table);
      };
    let jm_parameters =
      ms_args(signature)
      |> List.mapi((index, value_type) => {
           // TODO: maybe we should use the index? Read the spec and also double is a problem
           // on non-static 0 is this, so that's why the offset
           let table_index = jm_static ? index : index + 1;
           let name = {
             let.some (_, _, name, received_value_type, index) =
               List.nth_opt(variable_table, table_index);
             if (value_type != received_value_type) {
               failwith("that is weird, look at value_type here");
             };
             Some(name);
           };
           let jvm_type = parse_value_type(value_type);
           // TODO: test generic with static and non static
           let _is_generic =
             variable_type_table
             |> List.exists(((_, _, _, _, table_index)) =>
                  index == table_index
                );

           (name, jvm_type);
         });
    let jm_abstract = false;
    {jm_classpath, jm_name, jm_parameters, jm_return, jm_abstract, jm_kind};
  };

let parse_jclass = jclass => {
  let jc_classpath = parse_class_name(jclass.c_name);
  let jc_fields =
    jclass.c_fields
    |> FieldMap.value_elements
    |> List.map(parse_class_field(jc_classpath));
  let jc_methods =
    jclass.c_methods
    |> MethodMap.value_elements
    |> List.map(parse_jmethod(jc_classpath))
    // TODO: probably this shouldn't be here
    |> List.filter(method => method.jm_name != "<clinit>");
  {jc_classpath, jc_fields, jc_methods};
};
let in_channel_to_jclass = ic => {
  let ch = JLib.IO.input_channel(ic);
  JParse.parse_class_low_level(ch) |> JLow2High.low2high_class;
};

let parse_bytecode = ic => {
  switch (in_channel_to_jclass(ic)) {
  | JInterface(_) => failwith("currently not supported")
  | JClass(jclass) => parse_jclass(jclass)
  };
};
