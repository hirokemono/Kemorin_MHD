
/* fftw3_wrapper_kemo_c.h  */

#ifndef _FFTW3_WRAPPER_KEMO_C_
#define _FFTW3_WRAPPER_KEMO_C_

#ifndef DEPENDENCY_CHECK
  #include "fftw3.h"
#endif

#include "calypso_param_c.h"

/* prototypes */

void kemo_fftw_plan_dft_r2c_1d(long *plan, int *n_size,
			void *dble_in, void *cplx_out, int *iflag);
void kemo_fftw_plan_dft_c2r_1d(long *plan, int *n_size, 
			void *cplx_in, void *dble_out, int *iflag);

void kemo_fftw_plan_many_dft_r2c(long *plan, int *irank,
                                 int *n_size, int *howmany,
                                 void *dble_in, const int *inembed, int *istride, int *idist,
                                 void *cplx_out, int *onembed, int ostride, int odist,
                                 int *iflag);
void kemo_fftw_plan_many_dft_c2r(long *plan, int *irank, 
                                 int *n_size, int *howmany,
                                 void *cplx_in, const int *inembed, int *istride, int *idist,
                                 void *dble_out, int *onembed, int *ostride, int *odist,
                                 int *iflag);


void kemo_fftw_destroy_plan(long *plan);
void kemo_fftw_cleanup();

void kemo_fftw_execute(long *plan);
void kemo_fftw_execute_dft_r2c(long *plan, void *dble_in, void *cplx_out);
void kemo_fftw_execute_dft_c2r(long *plan, void *cplx_in, void *dble_out);

#endif
