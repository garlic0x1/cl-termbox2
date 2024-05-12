LISP ?= ${shell which sbcl}

snake:
	$(LISP) --eval "(asdf:make :termbox2/snake)" \
		--eval "(quit)"

example:
	$(LISP) --eval "(asdf:make :termbox2/example)" \
		--eval "(quit)"
