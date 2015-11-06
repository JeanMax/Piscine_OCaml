#******************************************************************************#
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: mcanal <mcanal@student.42.fr>              +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2014/11/29 13:16:03 by mcanal            #+#    #+#              #
#    Updated: 2015/11/06 03:47:34 by mcanal           ###   ########.fr        #
#                                                                              #
#******************************************************************************#

NAME = dot_slash
SRC = gardening.ml
O_DIR = tmp
S_DIR = src
VPATH =	src

SRCC = $(addprefix $(S_DIR)/,$(SRC))
OBJS = $(SRC:%.ml=$(O_DIR)/%.cmo)
OPTOBJS = $(SRC:%.ml=$(O_DIR)/%.cmx)
DEPS = $(O_DIR)/.depend

LIBS = $(WITHGRAPHICS)
WITHGRAPHICS = graphics.cma -cclib -lgraphics
WITHUNIX = unix.cma -cclib -lunix
WITHSTR = str.cma -cclib -lstr
WITHNUMS = nums.cma -cclib -lnums
WITHTHREADS = threads.cma -cclib -lthreads
WITHDBM = dbm.cma -cclib -lmldbm -cclib -lndbm

RM = rm -rf
MV = mv -f
MKDIR = mkdir -p

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CFLAGS = -w +a-4-6-7-9-27-29-32..39-41..42-44-45 -warn-error -a

ifeq ($(shell uname), Linux)
else
endif

WHITE = \033[37;01m
RED = \033[31;01m
GREEN =  \033[32;01m
BLUE =  \033[34;01m
BASIC = \033[0m

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.PHONY: all me_cry opt clean fclean depend re

all: depend $(NAME)

me_cry: CFLAGS = -w @A
me_cry: depend $(NAME)

-include $(DEPS)

$(NAME): $(OBJS)
	@$(CAMLC) $(CFLAGS) $(LIBS) $(OBJS) -o $(NAME)
	@echo "$(BLUE)$(OBJS) $(WHITE)->$(RED) $@ $(BASIC)"
	@echo "$(WHITE)flags:$(BASIC) $(CFLAGS)"
	@echo "$(WHITE)compi:$(BASIC) $(CAMLC)"

opt: $(OPTOBJS)
	@$(CAMLOPT) $(CFLAGS) $(LIBS:.cma=.cmxa) $(OPTOBJS) -o $(NAME)
	@echo "$(BLUE)$(OPTOBJS) $(WHITE)->$(RED) $@ $(BASIC)"
	@echo "$(WHITE)flags:$(BASIC) $(CFLAGS)"
	@echo "$(WHITE)compi:$(BASIC) $(CAMLOPT)"

$(O_DIR)/%.cmo: %.ml
	@$(CAMLC) $(CFLAGS) -c $< -o $@
	@$(MV) $(@:$(O_DIR)%=$(S_DIR)%) $(@:$(O_DIR)%.cmo=$(S_DIR)%.cmi) $(O_DIR)
	@echo "$(WHITE)$<\t->$(BLUE) $@ $(BASIC)"

$(O_DIR)/%.cmx: %.ml
	@$(CAMLOPT) $(CFLAGS) -c $< -o $@
	@$(MV) $(@:$(O_DIR)%=$(S_DIR)%) $(@:$(O_DIR)%.cmx=$(S_DIR)%.cmi) $(@:$(O_DIR)%.cmx=$(S_DIR)%.o) $(O_DIR)
	@echo "$(WHITE)$<\t->$(BLUE) $@ $(BASIC)"

clean:
	@$(RM) $(O_DIR)

fclean: clean
	@$(RM) $(NAME)

depend: $(DEPS)
$(DEPS):
	@$(MKDIR) $(O_DIR)
	@$(CAMLDEP) $(SRCC) > $(DEPS)

re: fclean
	@$(MAKE) all
