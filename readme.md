# Sireum Utility Tools

[![Actions Status](https://github.com/sireum/tools/workflows/CI/badge.svg)](https://github.com/sireum/tools/actions) [![Run Status](https://api.shippable.com/projects/5a968ae4d03865070013954a/badge?branch=master)](https://app.shippable.com/projects/5a968ae4d03865070013954a) 

This repository holds various Sireum utility tools:

* **Sergen**: JSON and MessagePack de/serializer generator from Slang datatype definitions

  As an example, for Slang [AST](https://github.com/sireum/slang/blob/master/ast/shared/src/main/scala/org/sireum/lang/ast/AST.scala) 
  and [symbol information](https://github.com/sireum/slang/blob/master/tipe/shared/src/main/scala/org/sireum/lang/symbol/Info.scala):
  
  * Generated [JSON](https://github.com/sireum/slang/blob/master/tipe/shared/src/main/scala/org/sireum/lang/tipe/JSON.scala)
  
  * Generated [MessagePack](https://github.com/sireum/slang/blob/master/tipe/shared/src/main/scala/org/sireum/lang/tipe/MsgPack.scala)
  
    * ... and a [custom (hand-written) extension](https://github.com/sireum/slang/blob/master/tipe/shared/src/main/scala/org/sireum/lang/tipe/CustomMessagePack.scala) 
      that adds compression/pooling for commonly occurring objects

* **Transgen**: Transformer (visitor and rewriter) generator from Slang datatype definitions

  As an example, for Slang [AST](https://github.com/sireum/slang/blob/master/ast/shared/src/main/scala/org/sireum/lang/ast/AST.scala):
  
  * Generated [Tranformer](https://github.com/sireum/slang/blob/master/ast/shared/src/main/scala/org/sireum/lang/ast/Transformer.scala) (immutable)
  
  * Generated [MTransformer](https://github.com/sireum/slang/blob/master/ast/shared/src/main/scala/org/sireum/lang/ast/MTransformer.scala) (mutable)
  
* **Cligen**: Command-line option parser generator from Slang datatype objects

  As an example, Sireum [CLI config](https://github.com/sireum/v3/blob/master/cli/jvm/src/main/scala/org/sireum/cli/CliConfig.sc) datatype structure:
  
  * Generated [CLI](https://github.com/sireum/v3/blob/master/cli/jvm/src/main/scala/org/sireum/cli/Cli.scala)

Note that the generators and the generated code are [Slang](https://github.com/sireum/slang) programs.


## Testing


* **macOS/Linux**

  ```bash
  bin/build.cmd test
  ```
  
* **Windows**

  ```cmd
  bin\build.cmd test
  ```

The test run checks generated code such as the ones mentioned above do not change
when re-generated.