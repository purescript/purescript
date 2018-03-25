ghcid:
	ghcid --command "stack ghci purescript:lib purescript:test:tests"

ghcid-test:
	ghcid --command "stack ghci purescript:lib purescript:test:tests" \
	    --test "Main.main"

.PHONY: ghcid ghcid-test
