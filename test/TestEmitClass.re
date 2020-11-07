open Migrate_parsetree;
open Setup;
open Reason_jvm_ffi_ir;

describe("transform jvm_class to a structure", ({test, _}) => {
  let test = (name, input, output) => {
    let run_test = suite => {
      let input = Reason_jvm_ffi_bindgen.gen_class(input);
      Helpers.compare_str(input, output, suite);
    };
    test(name, run_test);
  };

  let constructor = {
    jm_classpath: "com/github/eduardorfs/RandomClass",
    jm_name: "<init>",
    jm_parameters: [],
    jm_return: None,
    jm_abstract: false,
    jm_kind: `Constructor,
  };
  let width_field = {
    jf_classpath: "com/github/eduardorfs/RandomClass",
    jf_name: "width",
    jf_type: Int,
    jf_final: false,
    jf_static: false,
  };
  let get_width_method = {
    jm_classpath: "com/github/eduardorfs/RandomClass",
    jm_name: "getWidth",
    jm_parameters: [],
    jm_return: Some(Int),
    jm_abstract: false,
    jm_kind: `Method,
  };

  test(
    "empty class",
    {
      jc_classpath: "com/github/eduardorfs/RandomClass",
      jc_fields: [],
      jc_methods: [],
    },
    [%str],
  );
  test(
    "single field",
    {
      jc_classpath: "com/github/eduardorfs/RandomClass",
      jc_fields: [width_field],
      jc_methods: [],
    },
    [%str
      let width = this =>
        Jvm_ffi_runtime.make_field(
          ~name="width",
          ~signature="I",
          ~getter=Jvm_ffi_runtime.get_int_field,
          ~setter=Jvm_ffi_runtime.set_int_field,
          this,
        )
    ],
  );
  test(
    "single method",
    {
      jc_classpath: "com/github/eduardorfs/RandomClass",
      jc_fields: [],
      jc_methods: [get_width_method],
    },
    [%str
      let getWidth = (this, ()) =>
        Jvm_ffi_runtime.call_method(
          ~name="getWidth",
          ~signature="()I",
          Jvm_ffi_runtime.call_int_method,
          this,
          [||],
        )
    ],
  );
  test(
    "constructor name mangling",
    {
      jc_classpath: "com/github/eduardorfs/RandomClass",
      jc_fields: [],
      jc_methods: [constructor],
    },
    [%str
      let make = () =>
        Jvm_ffi_runtime.call_constructor(
          ~signature="()V",
          // TODO: avoid duplicated class instance
          Jvm_ffi_runtime.find_class("com/github/eduardorfs/RandomClass"),
          [||],
        )
    ],
  );
  test(
    "name colision",
    {
      jc_classpath: "com/github/eduardorfs/RandomClass",
      jc_fields: [width_field],
      jc_methods: [
        {...get_width_method, jm_name: "width"},
        constructor,
        {...get_width_method, jm_name: "make"},
      ],
    },
    // TODO: maybe width_field_1 and width_method_1?
    [%str
      let width_1 = this =>
        Jvm_ffi_runtime.make_field(
          ~name="width",
          ~signature="I",
          ~getter=Jvm_ffi_runtime.get_int_field,
          ~setter=Jvm_ffi_runtime.set_int_field,
          this,
        );
      let width_2 = (this, ()) =>
        Jvm_ffi_runtime.call_method(
          ~name="width",
          ~signature="()I",
          Jvm_ffi_runtime.call_int_method,
          this,
          [||],
        );
      let make_1 = () =>
        Jvm_ffi_runtime.call_constructor(
          ~signature="()V",
          // TODO: avoid duplicated class instance
          Jvm_ffi_runtime.find_class("com/github/eduardorfs/RandomClass"),
          [||],
        );
      let make_2 = (this, ()) =>
        Jvm_ffi_runtime.call_method(
          ~name="make",
          ~signature="()I",
          Jvm_ffi_runtime.call_int_method,
          this,
          [||],
        )
    ],
  );
  test(
    "keep order",
    {
      jc_classpath: "com/github/eduardorfs/RandomClass",
      jc_fields: [width_field, {...width_field, jf_name: "height"}],
      jc_methods: [],
    },
    // TODO: maybe width_field_1 and width_method_1?
    [%str
      let width = this =>
        Jvm_ffi_runtime.make_field(
          ~name="width",
          ~signature="I",
          ~getter=Jvm_ffi_runtime.get_int_field,
          ~setter=Jvm_ffi_runtime.set_int_field,
          this,
        );
      let height = this =>
        Jvm_ffi_runtime.make_field(
          ~name="height",
          ~signature="I",
          ~getter=Jvm_ffi_runtime.get_int_field,
          ~setter=Jvm_ffi_runtime.set_int_field,
          this,
        )
    ],
  );
});
