
#include <stdio.h>
#include <stdlib.h>

#include "m_psf_data_4_viewer_c.h"
#include "read_psf_data_viewer_c.h"
#include "take_normal_psf_c.h"
#include "check_psf_data_viewer_c.h"
#include "select_read_psf_viewer_c.h"
#include "set_kemoviewer_ucd_data.h"

struct psf_data *tako;
struct psf_data takomesh;

int main(int argc,char *argv[]){
	int length = 511;
	int istep;
    double time = 0.0;
	tako = &takomesh;
	
	if(argc < 3){
		printf("set psf file header for first argument and step on the second argument\n");
		return 1;
	}
	
	char *file_head_w_step = (char *)calloc(length+1, sizeof(char));
	char *ucd_header = (char *)calloc(length+1, sizeof(char));
	char *file_ext = (char *)calloc(length+1, sizeof(char));
    if(file_head_w_step == NULL){
        printf("malloc error for string\n");
        exit(0);
    };
    if(ucd_header == NULL){
        printf("malloc error for string\n");
        exit(0);
    };
    if(file_ext == NULL){
        printf("malloc error for string\n");
        exit(0);
    };
	int iflag_fileformat = set_data_format_flag(argv[1], file_head_w_step, file_ext);
	istep = get_index_from_file_head(file_head_w_step, ucd_header);

	check_gzip_kemoview_ucd_first(iflag_fileformat, istep, &time, ucd_header, tako);
    alloc_psf_data_s(tako);

	take_normal_psf(tako);
	take_minmax_psf(tako);
	check_psf_data_c(tako);
	return 0;
};

