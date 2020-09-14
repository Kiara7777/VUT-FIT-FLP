#
# Makefile pro logicky projekt do predmetu FLP
# Sara Skutova, xskuto00@stud.fit.vutbr.cz
#

FILE = kostka
NAME = flp18-log
#NAME = test
ARNAME = flp-log-xskuto00

.PHONY: all

all: $(FILE)

$(FILE): $(FILE).pl
		swipl -q -g start -o $(NAME) -c $(FILE).pl
		
clean:
		rm -f $(NAME) $(ARNAME).zip
		
zip:
		zip $(ARNAME).zip $(FILE).pl README Makefile kostka_zadani.in kostka1.in kostka2.in
 
