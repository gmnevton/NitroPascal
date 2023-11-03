# NewPascal
NewPascal is concept of new programming language based on Pascal. Maybe in future there will be a compiler and debugger.

---

Project files (extension: '.npp'):

```
project 'Project_name';

// program-type defines what extension will be given to output of compiler, eg.:
//   if +Windows is defined than extension will be '.exe'
{$program-type GUI+Windows} or {$program-type CONSOLE+Windows} 

{$resources 'Name for resurces'
  {@import 'resurces-file-name'}
  {@define 'resource-name' type:string:mapping-type:'resurce text'}
  {@define 'resource-name' type:base64:mapping-type:'base64data'}
}

import
  list-of-source-code-files;

//if it is a dynamic loaded library, specify which source files declare exported code
export
  list-of-source-code-files;

initialization
  // initialization code

finalization
  // finalization code

begin
  // program initialization code
end.
```

Source code files (extension: '.npc'):

```
code 'Source_code_name';

{$defines 'Name for defines'
  {@condition 'Name for condition':'Value for condition'}
  {@if 'Compiler_version' == '1.0'
    {@define 'define-name' type:string:'define value'}
  } else {
    {@define 'define-name' type:string:'define value'}
  }
}

import
  list-of-source-code-files;

type
  TSomeType = predefined-type:global; 
  TSomeType = class('Ancestor class name'):global;
  TSomeType = predefined-type; // if there is no :global defined, than the scope of this type is local for the source code file;

const
  // consts are protected (read only) and can not be modified at any time
  SomeConst:type:global = value;
  SomeConst = value;

var
  // variables are read/write and can be modified at any time
  SomeVariable:type:global = value;
  SomeVariable = value;

initialization
  // initialization code

finalization
  // finalization code

end.
```

Rules:

1. All source code files are local scope oriented, it means that everything that is defined in source code file is containd in its scope.
2. Any type, const, var or procedure/function defined can be set to be included in global scope, which means that it can be accessed by other source code files.
3. There is no need for existance of interface and implementation sections as in traditional Pascal.
4. Source code file is implicitly threated as implementation section, that defines what can be interfaced by declaring specifier ':global'.
5. Type definition can be extended.
6. 'begin' and 'end' key-words for method body declaration are history, for speed of typing we use brackets '{', '}' with ';' for declaration termination sign.
7. 'begin' is only allowed in project body definition, 'end.' is required as project body or source code file termination.
8. Body of methods defined in type declaration can be:
   a) declared inline with method declaration, eg.:
     type
       TExampleType = class {
       public
         MyExample(const A, B: Int32): Int32 {
           Result := A + B;
         };
       };
   b) declared later in the body of source code file, eg.:
     type
       TExampleType = class {
       public
         MyExample(const A, B: Int32): Int32;
       };
   
       TSecondExampleType = class(TExampleType) {
       public
         MyOtherExample: String;
       };
       ...
   
       TExampleType.MyExample(const A, B: Int32): Int32; {
         Result := A + B;
       };

       TSecondExampleType.MyOtherExample: String; {
         Result := 'output value';
       };
9. Class or record methods can be defined as class methods by using prefix 'class:', eg.:
       TExampleType = class {
       public
         class:MyExample(const A, B: Int32): Int32;
       };
10. Class or record methods can be defined as inline methods by using suffix ':inline', eg.:
       TExampleType = class {
       public
         MyExample(const A, B: Int32): Int32:inline;
       };
11. Const and variable declarations inside method body can be inlined.
12. Acces to globaly declared types, consts, variables require using prefix 'global:'.
13. If type, const, variable section defines many globaly accessible definitions, suffix ':global' can be added to such section definition, eg: type:global. But any other section key-word use without such suffix sets the scope to local.
14. All project and source code files are case-sensitive, meaning that variable 'a' and 'A' are two different declarations.
15. Some names can have spaces it them, to declare that use quotes ''.
16. Numbers can be declared with '_' for better readability, eg: 1_000_000.
