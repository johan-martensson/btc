GHC = ghc
GHCFLAGS = -O2

all: btcprice btcgui

btcprice: btcprice.hs
	$(GHC) $(GHCFLAGS) -o $@ $<

btcgui: btcgui.hs
	$(GHC) $(GHCFLAGS) -o $@ $<

clean:
	rm -f btcprice btcgui *.o *.hi

.PHONY: all clean
