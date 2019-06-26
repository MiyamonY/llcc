PROGRAM := llcc

.PHONY: test build
.DEFAULT: build

build:
	raco -o $(PROGRAM) llcc.rkt

test:
	./test.sh

clean:
	$(RM) $(PROGRAM) tmp*
