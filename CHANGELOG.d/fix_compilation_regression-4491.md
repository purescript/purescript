* Fix a compilation memory regression for very large files

When compiling a a very large file (>12K lines)
the CSE pass could baloon memory and result in increased
compilation times.

This fix uses a strict Map instead of a lazy Map to avoid
building up unnecessary thunks during the optimization pass.
