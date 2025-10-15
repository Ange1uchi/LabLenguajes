# Agrego este makefile para facilitar la compilación y ejecución de Tests.hs
#
# make compile -> compila usando GHC colocando artefactos en output/ para no encochinar el src/
# make run     -> ejecuta ./output/Tests.exe (compila antes si hace falta)
# make clean   -> limpia el src/output/

GHC      = ghc
SRC      = Test.hs
OUTDIR   = output
BIN      = $(OUTDIR)/Tests.exe

.PHONY: compile run clean

compile: $(BIN)

$(BIN): $(SRC) | $(OUTDIR)
	$(GHC) --make $(SRC) -odir $(OUTDIR) -hidir $(OUTDIR) -outputdir $(OUTDIR) -o $(BIN)

$(OUTDIR):
	@mkdir $(OUTDIR) 2>NUL || exit 0

run: compile
	.\output\Tests.exe

clean:
	@del /q $(OUTDIR)\*.o 2>NUL || exit 0
	@del /q $(OUTDIR)\*.hi 2>NUL || exit 0
	@del /q $(BIN) 2>NUL || exit 0
	@del /q $(OUTDIR)\*.exe 2>NUL || exit 0
