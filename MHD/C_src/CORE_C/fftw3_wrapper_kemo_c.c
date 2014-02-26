
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

void kemo_fftw_plan_many_dft_r2c(fftw_plan *plan, int rank,
                                 int *n_size, int howmany,
                                 double *dble_in, const int *inembed,
                                 int istride, int idist,
                                 fftw_complex *cplx_out, int *onembed,
                                 int ostride, int odist,
                                 unsigned *flags){
	*plan = fftw_plan_many_dft_r2c(rank, n_size, howmany,
                                   dble_in, inembed, istride, idist, 
                                   cplx_out, onembed, ostride, odist, *flags);
	return;
}

void kemo_fftw_plan_many_dft_c2r(fftw_plan *plan, int rank,
                                 int *n_size, int howmany,
                                 fftw_complex *cplx_in, const int *inembed,
                                 int istride, int idist,
                                 double *dble_out, int *onembed,
                                 int ostride, int odist,
                                 unsigned *flags){
	*plan = fftw_plan_many_dft_c2r(rank, n_size, howmany,
                                   cplx_in, inembed, istride, idist, 
                                   dble_out, onembed, ostride, odist, *flags);
	return;
}

void kemo_fftw_destroy_plan(fftw_plan *plan){
	fftw_destroy_plan(*plan);
	return;
}


void kemo_fftw_execute(fftw_plan *plan){
	fftw_execute(*plan);
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
