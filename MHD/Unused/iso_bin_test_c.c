
#include <stdlib.h>
#include "m_psf_data_4_viewer_c.h"
#include "read_psf_bin_data_c.h"
#include "read_psf_bin_data_gz_c.h"

int main(){
	char bin_name[255] = "iso_temp2.800001.inb";
	char gzip_name[255] = "iso_temp3.800001.inb.gz";
	
	struct psf_data *psf_b = (struct psf_data *) malloc(sizeof(struct psf_data));
	read_alloc_iso_bin(bin_name, psf_b);
    alloc_psf_color_data_c(psf_b);
    alloc_psf_data_s(psf_b);
	check_psf_read(psf_b);
	
	struct psf_data *psf_z = (struct psf_data *) malloc(sizeof(struct psf_data));
	read_alloc_iso_bin_gz(gzip_name, psf_z);
    alloc_psf_color_data_c(psf_z);
    alloc_psf_data_s(psf_z);
	check_psf_read(psf_z);
	
	compare_psf_data(psf_b, psf_z);
	return 0;
}
