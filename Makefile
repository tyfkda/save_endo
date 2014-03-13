all:
	make -C src
	make -C tools

clean:
	make -C src clean
	make -C tools clean

diff:
	bin/calc_risk resources/target.png image.png
