export const runtimeImportImpl = nothing => just => moduleName => body => () =>
  import(`../${moduleName}/index.js`).then(() => body(nothing)(), err => body(just(err.toString()))());
