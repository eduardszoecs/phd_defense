#!/bin/bash
for var in "$@"
do
	s=${var##*/}
	base=${s%.svg}
	echo 'converting' $var

	LD_LIBRARY_PATH=$HOME/cairo-git/lib inkscape -f $var -A $base.pdf
	echo 'done!'
done
