QUICKLISP_PATH=$(HOME)/quicklisp
SBCL=sbcl --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP=$(SBCL) --load $(QUICKLISP_PATH)/setup.lisp

all: quicklisp-github-stats

deps.txt: *.asd
		$(QUICKLISP) --eval "(push '*default-pathname-defaults* asdf:*central-registry*)" --eval '(ql:quickload "quicklisp-github-stats")' 
		touch deps.txt

manifest.txt: deps.txt
		$(QUICKLISP) --eval '(ql:write-asdf-manifest-file "manifest.txt")'

quicklisp-github-stats: *.lisp *.asd manifest.txt
		buildapp --manifest-file manifest.txt  	 \
				--asdf-path . 											 \
				--load-system quicklisp-github-stats \
				--entry quicklisp-github-stats::main \
				--output lisp-api.lisp

clean:
		rm -f quicklisp-github-stats manifest.txt deps.txt
