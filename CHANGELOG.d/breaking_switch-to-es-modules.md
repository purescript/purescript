* Switch from Common JS to ES modules
  
  Previously, Purescript used Common JS for FFI declarations.

  Before, FFI was declared like this...

  ```javascript
  const mymodule = require('mymodule')

  exports.myvar = mymodule.myvar
  ```
  
  ...and will be changed to this...

  ```javascript
  import * as M from 'mymodule';
  export const myvar = M.myvar
  ```
  ...or using the short version...

  ```javascript
  export { myvar } from 'mymodule';
  ```

* FFI is annotated with `/* #__PURE__ */` so that bundlers can perform DCE
* If CJS is detected a `Warning` is emitted
* The current LTS Node.js version `12` is now the required minimum version
* `purs bundle` has been rudimentarily updated but will be removed in a subsequent PR
