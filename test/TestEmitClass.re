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
    "name colision",
    {
      jc_classpath: "com/github/eduardorfs/RandomClass",
      jc_fields: [width_field],
      jc_methods: [{...get_width_method, jm_name: "width"}],
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
