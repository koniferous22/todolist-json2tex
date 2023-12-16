PROJECT_NAME := todolist-json2tex

ensure-bin-dir-exists:
	test -d "$(CURDIR)/bin" || mkdir "$(CURDIR)/bin"
build: ensure-bin-dir-exists
	stack build --exec "cp \"$(CURDIR)/$$(stack path --dist-dir)/build/$(PROJECT_NAME)-exe/$(PROJECT_NAME)-exe\" \"$(CURDIR)/bin/$(PROJECT_NAME)\""
build-watch: ensure-bin-dir-exists
	stack build --file-watch --fast --exec "cp \"$(CURDIR)/$$(stack path --dist-dir)/build/$(PROJECT_NAME)-exe/$(PROJECT_NAME)-exe\" \"$(CURDIR)/bin/$(PROJECT_NAME)\""
runtests: ensure-bin-dir-exists
	stack test --fast
runtests-watch: ensure-bin-dir-exists
	stack test --fast --file-watch
fmt:
	find ./app ./src ./test -type f | xargs stack exec hindent -- 
