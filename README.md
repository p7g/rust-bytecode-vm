# rbcvm

## Next steps

- [ ] compound assignment (like `+=`)
- [ ] atoms like `:this_is_an_atom`
- [ ] [erlang style][1] records
- [ ] augment modules from rust
- [ ] maybe some documentation or specification?
- [ ] module header should be optional (unless imported)
- [ ] varargs
- [x] native functions should return `Result<Value, Error>`
- [ ] change all the `Result<_, String>` to some proper error type
- [ ] switch case: when cases are all suitable, should be a jump table kinda thing

[1]: http://erlang.org/doc/reference_manual/records.html
