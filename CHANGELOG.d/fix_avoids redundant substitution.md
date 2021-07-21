* Avoids unnecessary substitution checks when running `unifyTypes`.

By only checking substitutions when necessary (where "necessary" is defined in a comment above `unifyTypes`), this leads to a 1.5-2x speedup for heavily constrained files.
