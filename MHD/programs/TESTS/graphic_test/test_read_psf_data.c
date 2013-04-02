
#include <stdio.h>
#include <stdlib.h>

#include "m_psf_data_4_viewer_c.h"
#include "read_psf_data_viewer_c.h"
#include "take_normal_psf_c.h"

struct psf_data *tako;
struct psf_data takomesh;

int main(int argc,char *argv[]){
	int i, j, k, num, kst, ked;
	tako = &takomesh;
	
	if(argc < 3){
		printf("set psf file header for first argument and step on the second argument\n");
		return 1;
	}
	
	
	check_gzip_kemoview_ucd_first(argv[1], argv[2], tako);
	
	take_normal_psf(tako);
	take_minmax_psf(tako);
	check_psf_data_c(tako);
	return 0;
};

