QUICKLISP_PATH=$(HOME)/quicklisp
SBCL=sbcl --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP=$(SBCL) --load $(QUICKLISP_PATH)/setup.lisp

all: lisp-api

# deps.txt: *.asd
# 		$(QUICKLISP) --eval "(push '*default-pathname-defaults* asdf:*central-registry*)" --eval '(ql:quickload "lisp-api")' 
# 		touch deps.txt

# manifest.txt: deps.txt
# 		$(QUICKLISP) --eval '(ql:write-asdf-manifest-file "manifest.txt")'

lisp-api: *.lisp *.asd manifest.txt
		buildapp \
				--asdf-path . 									  \
				--load-system lisp-api 						\
				--entry main 										 	\
				--output lisp-api

			  # --manifest-file manifest.txt \
		rm -f lisp-api manifest.txt deps.txt

clean:
		rm -f lisp-api manifest.txt
