open Reason_jvm_ffi_ir;
open Utils;

let escape_duplicated_names = clazz => {
  let escape_method_name =
    fun
    | "<init>" => "make"
    | str => str;
  let fields_and_methods =
    (clazz.jc_fields |> List.map(field => (field.jf_name, `Field(field))))
    @ (
      clazz.jc_methods
      |> List.map(method =>
           (escape_method_name(method.jm_name), `Method(method))
         )
    );
  let all_names =
    fields_and_methods
    |> List.fold_left(
         (env, (name, _)) => {
           let occurrences =
             env |> StringMap.find_opt(name) |> Option.value(~default=0);
           let occurrences = occurrences + 1;
           env |> StringMap.add(name, occurrences);
         },
         StringMap.empty,
       );
  fields_and_methods
  |> List.fold_left(
       ((names, values), (original_name, value)) => {
         let is_duplicated = all_names |> StringMap.find(original_name) > 1;
         let occurrences =
           names |> List.filter((==)(original_name)) |> List.length;
         let name =
           is_duplicated
             ? original_name ++ "_" ++ string_of_int(occurrences + 1)
             : original_name;

         ([original_name, ...names], values @ [(name, value)]);
       },
       ([], []),
     )
  |> snd;
};
let gen_class = clazz => {
  let fields_and_methods = escape_duplicated_names(clazz);

  fields_and_methods
  |> List.map(
       fun
       | (name, `Field(field)) => (name, Field.gen_field(field))
       | (name, `Method(method)) => (name, Method.gen_method(method)),
     )
  |> List.map(((name, body)) => {
       let binding = value_binding(~pat=pvar(name), ~expr=body);
       pstr_value(Nonrecursive, [binding]);
     });
};
