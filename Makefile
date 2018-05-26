all:
	meson builddir && cd builddir && ninja
clean:
	rm -fr builddir *.mod src/*.o
