open Reason_jvm_ffi_ir;
open Parsetree;

let gen_method: jvm_method => expression = Method.gen_method;
let gen_field: jvm_field => expression = Field.gen_field;
