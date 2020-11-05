open Setup;
open Reason_jvm_ffi_ir;

describe("parse .class to jvm_class", ({test, _}) => {
  let test = (name, file, output) => {
    let run_test = suite => {
      let ic = open_in_bin(file);
      let input = Reason_jvm_ffi_parse.Parse.parse_bytecode(ic);
      close_in(ic);
      Helpers.compare_jvm_class(input, output, suite);
    };
    test(name, run_test);
  };

  test(
    "constructor, width, height, getter and setter",
    "./Object2D.class",
    {
      jc_classpath: "com/github/eduardorfs/Object2D",
      jc_fields: [
        {
          // TODO: maybe avoid this duplication of classpath?
          jf_classpath: "com/github/eduardorfs/Object2D",
          jf_name: "width",
          jf_type: Int,
          jf_final: false,
          jf_static: false,
        },
        {
          jf_classpath: "com/github/eduardorfs/Object2D",
          jf_name: "height",
          jf_type: Int,
          jf_final: false,
          jf_static: false,
        },
      ],
      jc_methods: [
        {
          jm_classpath: "com/github/eduardorfs/Object2D",
          jm_name: "getWidth",
          jm_parameters: [],
          jm_return: Some(Int),
          jm_kind: `Method,
        },
        // TODO: probably this terrible order is because of javalib fixes it
        {
          jm_classpath: "com/github/eduardorfs/Object2D",
          jm_name: "<init>",
          jm_parameters: [],
          jm_return: None,
          jm_kind: `Constructor,
        },
        {
          jm_classpath: "com/github/eduardorfs/Object2D",
          jm_name: "getHeight",
          jm_parameters: [],
          jm_return: Some(Int),
          jm_kind: `Method,
        },
      ],
    },
  );
  test(
    "extends, private, overload, parameters",
    "./Potato.class",
    {
      jc_classpath: "com/github/eduardorfs/Potato",
      jc_fields: [
        {
          jf_classpath: "com/github/eduardorfs/Potato",
          jf_name: "temperature",
          jf_type: Int,
          jf_final: false,
          jf_static: false,
        },
      ],
      jc_methods: [
        {
          jm_classpath: "com/github/eduardorfs/Potato",
          jm_name: "getTemperature",
          jm_parameters: [],
          jm_return: Some(Int),
          jm_kind: `Method,
        },
        {
          jm_classpath: "com/github/eduardorfs/Potato",
          jm_name: "setTemperature",
          jm_parameters: [(Some("temperature"), Int)],
          jm_return: None,
          jm_kind: `Method,
        },
        {
          jm_classpath: "com/github/eduardorfs/Potato",
          jm_name: "<init>",
          jm_parameters: [],
          jm_return: None,
          jm_kind: `Constructor,
        },
        {
          jm_classpath: "com/github/eduardorfs/Potato",
          jm_name: "getTemperature",
          jm_parameters: [(Some("offset"), Int)],
          jm_return: Some(Int),
          jm_kind: `Method,
        },
      ],
    },
  );
  test(
    "extends, generics",
    "./Square.class",
    {
      jc_classpath: "com/github/eduardorfs/Square",
      jc_fields: [],
      jc_methods: [
        {
          jm_classpath: "com/github/eduardorfs/Square",
          jm_name: "double_object",
          jm_parameters: [
            (Some("i"), Object("com/github/eduardorfs/Object2D")),
            (Some("a"), Object("java/lang/Object")),
            (Some("b"), Object("java/lang/Object")),
          ],
          jm_return: Some(Object("java/lang/Object")),
          jm_kind: `Function,
        },
        {
          jm_classpath: "com/github/eduardorfs/Square",
          jm_name: "<init>",
          jm_parameters: [],
          jm_return: None,
          jm_kind: `Constructor,
        },
      ],
    },
  );
  test(
    "static method, static analyzer, constructor",
    "./MLManager.class",
    {
      jc_classpath: "com/github/eduardorfs/MLManager",
      jc_fields: [
        {
          jf_classpath: "com/github/eduardorfs/MLManager",
          jf_name: "callback",
          jf_type: Object("fr/inria/caml/camljava/Callback"),
          jf_final: false,
          jf_static: true,
        },
      ],
      jc_methods: [
        // TODO: I think clinit should be removed at the emitter
        // {
        //   jm_classpath: "com/github/eduardorfs/MLManager",
        //   jm_name: "<clinit>",
        //   jm_parameters: [],
        //   jm_return: None,
        //   jm_kind: `Function,
        // },
        // TODO: should bootOcaml be here?
        {
          jm_classpath: "com/github/eduardorfs/MLManager",
          jm_name: "bootOcaml",
          jm_parameters: [],
          jm_return: None,
          jm_kind: `Function,
        },
        {
          jm_classpath: "com/github/eduardorfs/MLManager",
          jm_name: "getCallback",
          jm_parameters: [],
          jm_return: Some(Object("fr/inria/caml/camljava/Callback")),
          jm_kind: `Function,
        },
        {
          jm_classpath: "com/github/eduardorfs/MLManager",
          jm_name: "<init>",
          jm_parameters: [],
          jm_return: None,
          jm_kind: `Constructor,
        },
      ],
    },
  );
});
