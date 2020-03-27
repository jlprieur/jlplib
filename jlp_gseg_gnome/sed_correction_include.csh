#!/bin/csh
#########################################################
#
#sed --expression="s/include \"mongo.h\"/include \"jlp_splot_def.h\"/g" $i > $i.0 
#sed --expression="s/include <fftw.h>/include "\""fftw.h"\""/g" $i > $i.0 
#########################################################
set list=`find . -name \*.c -print`

foreach i ($list)
sed --expression="s/include "\""jlp_gsegraf_gnome.h"\""/include "\""jlp_gnome.h"\""/g" $i > $i.0 
mv "$i.0" $i
end
goto end1

end1:
