
/* fftw3_wrapper_kemo_c.h  */

#ifndef _FFTW3_WRAPPER_KEMO_C_
#define _FFTW3_WRAPPER_KEMO_C_

#include "fftw3.h"
#include "kemosrc_param_c.h"

/* prototypes */

void kemo_fftw_plan_dft_r2c_1d(fftw_plan *plan, int *n_size,
			double *dble_in, fftw_complex *cplx_out, unsigned *flags);
void kemo_fftw_plan_dft_c2r_1d(fftw_plan *plan, int *n_size, 
			fftw_complex *cplx_in, double *dble_out, unsigned *flags);

void kemo_fftw_plan_many_dft_r2c(fftw_plan *plan, int rank,
                                 int *n_size, int howmany,
                                 double *dble_in, const int *inembed,
                                 int istride, int idist,
                                 fftw_complex *cplx_out, int *onembed,
                                 int ostride, int odist,
                                 unsigned *flags);
void kemo_fftw_plan_many_dft_c2r(fftw_plan *plan, int rank, 
                                 int *n_size, int howmany,
                                 fftw_complex *cplx_in, const int *inembed,
                                 int istride, int idist,
                                 double *dble_out, int *onembed,
                                 int ostride, int odist,
                                 unsigned *flags);


void kemo_fftw_destroy_plan(fftw_plan *plan);
void kemo_fftw_cleanup();

void kemo_fftw_execute(fftw_plan *plan);
void kemo_fftw_execute_dft_r2c(fftw_plan *plan, double *dble_in, fftw_complex *cplx_out);
void kemo_fftw_execute_dft_c2r(fftw_plan *plan, fftw_complex *cplx_in, double *dble_out);

#endif
