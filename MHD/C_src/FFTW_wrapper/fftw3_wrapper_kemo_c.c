
/* fftw3_wrapper_kemo_c.c  */

#include "fftw3_wrapper_kemo_c.h"


void kemo_fftw_plan_dft_r2c_1d(long *plan, int *n_size,
			void *dble_in, void *cplx_out, int *iflag){
	*plan = (long) fftw_plan_dft_r2c_1d(*n_size, (double *) dble_in, (fftw_complex *) cplx_out,
                                        (unsigned) *iflag);
	return;
}
void kemo_fftw_plan_dft_c2r_1d(long *plan, int *n_size, 
			void *cplx_in, void *dble_out, int *iflag){
	*plan = (long) fftw_plan_dft_c2r_1d(*n_size, (fftw_complex *) cplx_in, (double *) dble_out, 
                                        (unsigned) *iflag);
	return;
}

void kemo_fftw_plan_many_dft_r2c(long *plan, int *irank,
                                 int *n_size, int *howmany,
                                 void *dble_in, const int *inembed, int *istride, int *idist,
                                 void *cplx_out, int *onembed, int *ostride, int *odist,
                                 int *iflag){
	*plan = (long) fftw_plan_many_dft_r2c(*irank, n_size, *howmany,
                                   (double *) dble_in, inembed, *istride, *idist, 
                                   (fftw_complex *) cplx_out, onembed, *ostride, *odist, 
                              		     (unsigned) *iflag);
	return;
}

void kemo_fftw_plan_many_dft_c2r(long *plan, int *irank,
                                 int *n_size, int *howmany,
                                 void *cplx_in, const int *inembed, int *istride, int *idist,
                                 void *dble_out, int *onembed, int *ostride, int *odist,
                                 int *iflag){
	*plan = (long) fftw_plan_many_dft_c2r(*irank, n_size, *howmany,
                                   (fftw_complex *) cplx_in, inembed, *istride, *idist, 
                                   (double *) dble_out, onembed, *ostride, *odist,
                                          (unsigned) *iflag);
	return;
}

void kemo_fftw_destroy_plan(long *plan){
	fftw_destroy_plan((fftw_plan) *plan);
	return;
}

void kemo_fftw_cleanup(){
	fftw_cleanup();
	return;
}

void kemo_fftw_execute(long *plan){
	fftw_execute((fftw_plan) *plan);
	return;
}

void kemo_fftw_execute_dft_r2c(long *plan, void *dble_in, void *cplx_out){
	fftw_execute_dft_r2c((fftw_plan) *plan, (double *) dble_in, (fftw_complex *) cplx_out);
	return;
}

void kemo_fftw_execute_dft_c2r(long *plan, void *cplx_in, void *dble_out){
	fftw_execute_dft_c2r((fftw_plan) *plan, (fftw_complex *) cplx_in, (double *) dble_out);
	return;
}
