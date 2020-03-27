#!/bin/csh
#########################################################
#
#sed --expression="s/include \"mongo.h\"/include \"jlp_splot_def.h\"/g" $i > $i.0 
#sed --expression="s/include <fftw.h>/include "\""fftw.h"\""/g" $i > $i.0 
#########################################################
set list=`find . -name jlp_Gseg3d_\*.cpp -print`

foreach i ($list)
sed --expression="s/include "\""jlp_gsegraf.h"\""/include "\""jlp_gseg_3d.h"\""/g" $i > $i.0 
mv "$i.0" $i
end
goto end1

end1:
