/*
// Copyright AMD 2024-2025, MIT License, contact Bob.Robey@amd.com
*/

#include <rocblas/rocblas.h>
extern "C"{
    void omp_rocBLAS_dgemm(void *ptr, int &modeA, int &modeB, int &m, int &n, 
                   int &k, double &alpha, double *A, int &lda,
                   double *B, int &ldb, double &beta, double *C, int &ldc)
    {
        rocblas_handle *handle = (rocblas_handle *) ptr;
        rocblas_status status =  rocblas_dgemm(*handle,
                                               (rocblas_operation) modeA,
                                               (rocblas_operation) modeB, 
                                               (rocblas_int) m, 
                                               (rocblas_int) n,
                                               (rocblas_int) k,
                                               &alpha,
                                               A, (rocblas_int) lda, 
                                               B, (rocblas_int) ldb,
                                               &beta,
                                               C, (rocblas_int) ldc);

        // Check for errors
        if (status != rocblas_status_success){
            fprintf(stderr, "rocblas_dgemm failed with status %d\n", status);
        };
    }

    void init_rocBLAS(void *ptr)
    {
        rocblas_handle *handle = (rocblas_handle *) ptr;
        rocblas_create_handle(handle);
    }
    
    void finalize_rocBLAS(int ptr)
    {
        rocblas_handle handle = (rocblas_handle) ptr;
        rocblas_destroy_handle(handle);
    }
}
