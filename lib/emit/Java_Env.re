module Java_Env =
  Map.Make({
    type t = Basic_types.class_name;
    let compare = Basic_types.compare_class_name;
  });
include Java_Env;
