# NewPascal
NewPascal is concept of new programming language based on Pascal. Maybe in future there will be a compiler and debugger.

---

Project files (extension: '.npp'):

```
Project 'Project_name';

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
  TSomeType = class('Ancestor class name'):gloabl;
  TSomeType = predefined-type; // if there is no :global defined, than the scope of this type is local for the source code file;

const
  // consts are protected (read only) and can not be modified at any time
  SomeConst:type = value;
  SomeConst = value;

var
  // variables are read/write and can be modified at any time
  SomeVariable:type = value;
  SomeVariable = value;

initialization
  // initialization code

finalization
  // finalization code

end.
```






