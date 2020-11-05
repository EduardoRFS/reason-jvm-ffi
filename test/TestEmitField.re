open Migrate_parsetree;
open Setup;
open Reason_jvm_ffi_ir;

describe("transform jvm_field to an expression", ({test, _}) => {
  let test = (name, input, output) => {
    let run_test = suite => {
      let input = Reason_jvm_ffi_bindgen.gen_field(input);
      Helpers.compare_expr(input, output, suite);
    };
    test(name, run_test);
  };

  test(
    "int width",
    {
      jf_classpath: "com/github/eduardorfs/RandomClass",
      jf_name: "width",
      jf_type: Int,
      jf_final: false,
      jf_static: false,
    },
    [%expr
      this =>
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
    "float weight",
    {
      jf_classpath: "com/github/eduardorfs/RandomClass",
      jf_name: "weight",
      jf_type: Float,
      jf_final: false,
      jf_static: false,
    },
    [%expr
      this =>
        Jvm_ffi_runtime.make_field(
          ~name="weight",
          ~signature="F",
          ~getter=Jvm_ffi_runtime.get_float_field,
          ~setter=Jvm_ffi_runtime.set_float_field,
          this,
        )
    ],
  );
  test(
    "final int height",
    {
      jf_classpath: "com/github/eduardorfs/RandomClass",
      jf_name: "height",
      jf_type: Int,
      // TODO: should the low level API consider the final?
      jf_final: true,
      jf_static: false,
    },
    [%expr
      this =>
        Jvm_ffi_runtime.make_field(
          ~name="height",
          ~signature="I",
          ~getter=Jvm_ffi_runtime.get_int_field,
          ~setter=Jvm_ffi_runtime.set_int_field,
          this,
        )
    ],
  );
  test(
    "static Object manager",
    {
      jf_classpath: "com/github/eduardorfs/RandomClass",
      jf_name: "manager",
      jf_type: Object("com/github/eduardorfs/Anotherlass"),
      jf_final: false,
      jf_static: true,
    },
    [%expr
      // TODO: maybe look in depth at this because the boot of modules may take a long time
      Jvm_ffi_runtime.make_global_variable(
        ~name="manager",
        ~signature="Lcom/github/eduardorfs/Anotherlass;",
        ~getter=Jvm_ffi_runtime.get_static_object_field,
        ~setter=Jvm_ffi_runtime.set_static_object_field,
        Jvm_ffi_runtime.find_class("com/github/eduardorfs/RandomClass"),
      )
    ],
  );
  test(
    "final static Double unixEpoch",
    {
      jf_classpath: "com/github/eduardorfs/RandomClass",
      jf_name: "unixEpoch",
      jf_type: Double,
      jf_final: true,
      jf_static: true,
    },
    [%expr
      // TODO: also should be only getter?
      Jvm_ffi_runtime.make_global_variable(
        ~name="unixEpoch",
        ~signature="D",
        ~getter=Jvm_ffi_runtime.get_static_double_field,
        ~setter=Jvm_ffi_runtime.set_static_double_field,
        Jvm_ffi_runtime.find_class("com/github/eduardorfs/RandomClass"),
      )
    ],
  );
});
