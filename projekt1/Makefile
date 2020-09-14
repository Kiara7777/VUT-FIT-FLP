#
# Makefile pro funkcionalni projekt do predmetu FLP
# Sara Skutova, xskuto00@stud.fit.vutbr.cz
# 23.02.2018
#

NAME = rka-2-dka
#NAME = test
ARNAME = flp-fun-xskuto00
TESTS = test_opora.in test_sti.in test_opora-i.out test_sti-i.out test_opora-t.out test_sti-t.out
TESTSOUT = test_opora-i.out test_sti-i.out test_opora-t.out test_sti-t.out
HS = ghc
FLAGS = --make

.PHONY: all

all: $(NAME)

$(NAME): $(NAME).hs
		$(HS) $(FLAGS) $(NAME)

clean:
		rm -f $(NAME) $(NAME).o $(NAME).hi $(ARNAME).zip $(TESTSOUT)
		
zip:
		zip $(ARNAME).zip $(NAME).hs README Makefile $(TESTS)