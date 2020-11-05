open Reason_jvm_ffi_ir;
open Parsetree;

let gen_field: jvm_field => expression = Field.gen_field;
let gen_method: jvm_method => expression = Method.gen_method;
let gen_class: jvm_class => structure = Class.gen_class;
