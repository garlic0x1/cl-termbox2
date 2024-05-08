LISP ?= ${shell which sbcl}

example:
	$(LISP) --eval "(asdf:make :termbox2/example)" \
		--eval "(quit)"
