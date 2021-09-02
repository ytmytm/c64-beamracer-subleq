CA65   = ca65
CL65   = cl65
LD65   = ld65

BINDIR = bin

DEMOS = $(BINDIR)/subleq.prg

all: $(DEMOS)

VLIB = vlib/vasyl.s vlib/vlib.s

$(BINDIR)/subleq.prg: br-subleq.s $(VLIB)
	$(CL65) -t c64 -C c64-asm.cfg -u __EXEHDR__ $< -o $@ -Ln labels.lab -m map.map -v -vm -T -l listing.lst

clean:
	rm -f *.o 
