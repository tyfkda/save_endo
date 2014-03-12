all:
	make -C src
	make -C tools

clean:
	make -C src clean

diff:
	bin/calc_risk results/target.png image.png
