#include <stdio.h>

extern void c_read_control_sph_SGS_MHD();
extern void c_write_control_sph_SGS_MHD();

int main(int argc,char *argv[])
{
	
	c_read_control_sph_SGS_MHD();
	c_write_control_sph_SGS_MHD();
	
	return 0;
}
