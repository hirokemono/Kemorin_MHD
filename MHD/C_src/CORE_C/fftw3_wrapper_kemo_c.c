
/* fftw3_wrapper_kemo_c.c  */

#include "fftw3_wrapper_kemo_c.h"


void kemo_fftw_plan_dft_r2c_1d(fftw_plan *plan, int *n_size,
			double *dble_in, fftw_complex *cplx_out, unsigned *flags){
	*plan = fftw_plan_dft_r2c_1d(*n_size, dble_in, cplx_out, *flags);
	return;
}
void kemo_fftw_plan_dft_c2r_1d(fftw_plan *plan, int *n_size, 
			fftw_complex *cplx_in, double *dble_out, unsigned *flags){
	*plan = fftw_plan_dft_c2r_1d(*n_size, cplx_in, dble_out, *flags);
	return;
}

void kemo_fftw_destroy_plan(fftw_plan *plan){
	fftw_destroy_plan(*plan);
	return;
}



void kemo_fftw_execute_dft_r2c(fftw_plan *plan, double *dble_in, fftw_complex *cplx_out){
	fftw_execute_dft_r2c(*plan, dble_in, cplx_out);
	return;
}

void kemo_fftw_execute_dft_c2r(fftw_plan *plan, fftw_complex *cplx_in, double *dble_out){
	fftw_execute_dft_c2r(*plan, cplx_in, dble_out);
	return;
}
