OCAMLC = ocamlopt
OCAML_FLAGS = -I build
CMO = cmx
CMI = cmi
CMA = cmxa
MLI = types

LIBS = str.$(CMA) #unix.$(CMA)

LABS =

default: all


###############
## Utilities ##
###############

# testing.$(CMO):	testing.ml
# 	$(OCAMLC) -c testing.ml


###########
## Lab 0 ##
###########

LABS += lab0
lab0: build/lab0.$(CMO)
	$(OCAMLC) $(OCAML_FLAGS) $< -o $@

##################
## Lab Template ##
##################

# Template for single-file labs
define LAB_TEMPLATE =
build/lab$(1)_main.$(CMO): build/util.$(CMO) build/testing.$(CMO) build/lab$(1).$(CMO)

build/lab$(1).$(CMO): build/testing.$(CMO) build/util.$(CMO)

lab$(1): build/lab$(1).$(CMO) build/lab$(1)_main.$(CMO) build/testing.$(CMO) build/util.$(CMO)
	$(OCAMLC) $(OCAML_FLAGS) -o lab$(1) build/util.$(CMO) build/testing.$(CMO) build/lab$(1).$(CMO) build/lab$(1)_main.$(CMO)

lab$(1)_test: lab$(1)
	./lab$(1)

ifneq (,$(wildcard ./src/lab$(1)_main.ml))
LABS += lab$(1)
endif

endef

###########
## Labs  ##
###########

$(eval $(call LAB_TEMPLATE,1))
$(eval $(call LAB_TEMPLATE,2))

# Lab 3
build/lab3_main.$(CMO): build/util.$(CMO) build/testing.$(CMO) build/lab3_exp.$(CMO) build/lab3_rope.$(CMO) build/lab3_rbtree.$(CMO)
build/lab3_exp.$(CMO): build/testing.$(CMO) build/util.$(CMO)
build/lab3_rope.$(CMO): build/testing.$(CMO) build/util.$(CMO)
build/lab3_rbtree.$(CMO): build/testing.$(CMO) build/util.$(CMO)

lab3: build/lab3_main.$(CMO)
	$(OCAMLC) $(OCAML_FLAGS) -o lab3 build/util.$(CMO) build/testing.$(CMO) build/lab3_exp.$(CMO) build/lab3_rope.$(CMO) build/lab3_rbtree.$(CMO) build/lab3_main.$(CMO)

lab3_test: lab3
	./lab3

ifneq (,$(wildcard ./src/lab3_main.ml))
LABS += lab3
endif


# Lab 4
LAB4_O = build/util.$(CMO) build/testing.$(CMO) build/lab4_lazy.$(CMO) build/lab4_stream.$(CMO) build/lab4_queue.$(CMO)
build/lab4_main.$(CMO): $(LAB4_O)
build/lab4_lazy.$(CMO): build/testing.$(CMO) build/util.$(CMO)
build/lab4_stream.$(CMO): build/testing.$(CMO) build/util.$(CMO) build/lab4_lazy.$(CMO)
build/lab4_queue.$(CMO): build/testing.$(CMO) build/util.$(CMO) build/lab4_stream.$(CMO)

lab4: build/lab4_main.$(CMO)
	$(OCAMLC) $(OCAML_FLAGS) -o lab4 $(LAB4_O) build/lab4_main.$(CMO)

lab4_test: lab4
	./lab4

ifneq (,$(wildcard ./src/lab4_main.ml))
LABS += lab4
endif



###########
## Lab 5 ##
###########

build/javascript_lexer.ml: src/javascript_lexer.mll
	@mkdir -p build
	ocamllex -o $@ $<

# TODO: add -v and --strict flags
build/javascript_parser.ml: src/javascript_parser.mly
	@mkdir -p build
	cd build && ocamlyacc -bjavascript_parser ../src/javascript_parser.mly

build/javascript_parser.$(CMI): build/javascript_parser.ml build/javascript_ast.$(CMO)
	$(OCAMLC) $(OCAML_FLAGS) -c build/javascript_parser.mli

build/javascript_ast.$(CMO): build/util.$(CMO)
build/javascript_parser.$(CMO): build/javascript_ast.$(CMO) build/javascript_parser.$(CMI)
build/javascript_lexer.$(CMO): build/javascript_parser.$(CMO)
build/javascript_main.$(CMO): build/javascript_lexer.$(CMO)
build/lab5_tests.$(CMO): build/javascript_main.$(CMO)

JAVASCRIPT_O = $(addprefix build/,util.$(CMO) javascript_ast.$(CMO) javascript_parser.$(CMO) javascript_lexer.$(CMO) testing.$(CMO) javascript_main.$(CMO))

build/lab5_main.$(CMO): $(JAVASCRIPT_O) build/lab5_tests.$(CMO)
build/lab5_tests.$(CMO): $(JAVASCRIPT_O)

lab5: build/lab5_main.$(CMO) build/lab5_tests.$(CMO)
	$(OCAMLC) $(OCAML_FLAGS) -o $@ $(LIBS) $(JAVASCRIPT_O) build/lab5_tests.$(CMO) build/lab5_main.$(CMO)

lab5_test: lab5
	./lab5 --test

ifneq (,$(wildcard ./src/lab5_main.ml))
LABS += lab5
endif


####################
## Evaluator Labs ##
####################

build/javascript_heap.$(CMO): build/util.$(CMO) build/javascript_ast.$(CMO)

# Template for evaluator-file labs
define EVAL_TEMPLATE =
build/lab$(1)_main.$(CMO): $(JAVASCRIPT_O) $(addprefix build/,$(2)) build/lab$(1).$(CMO)
build/lab$(1).$(CMO): $(JAVASCRIPT_O) $(addprefix build/,$(2))


lab$(1): build/lab$(1)_main.$(CMO) build/lab$(1).$(CMO) $(JAVASCRIPT_EVAL_O)
	$(OCAMLC) $(OCAML_FLAGS) -o lab$(1) $(LIBS) $(JAVASCRIPT_O) $(addprefix build/,$(2)) build/lab$(1).$(CMO) build/lab$(1)_main.$(CMO)

lab$(1)_test: lab$(1)
	./lab$(1) --test

ifneq (,$(wildcard ./src/lab$(1)_main.ml))
LABS += lab$(1)
endif

endef

$(eval $(call EVAL_TEMPLATE,6))
$(eval $(call EVAL_TEMPLATE,7))
$(eval $(call EVAL_TEMPLATE,8,javascript_heap.$(CMO)))


# $(eval $(call EVAL_TEMPLATE,9))
# $(eval $(call EVAL_TEMPLATE,10))
# $(eval $(call EVAL_TEMPLATE,11))



#####################
## Utility Targets ##
#####################

build/%.$(CMO): src/%.ml
	@mkdir -p build
	$(OCAMLC) $(OCAML_FLAGS) -c $< -o $@

build/%.$(CMO): build/%.ml
	$(OCAMLC) $(OCAML_FLAGS) -c $< -o $@

all: $(LABS)

clean:
	rm -vf $(LABS)
	rm -vrf build
