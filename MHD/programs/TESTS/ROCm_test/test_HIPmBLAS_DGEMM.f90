!
! Copyright AMD 2024-2025, MIT License, contact Bob.Robey@amd.com
      program test_HIPmBLAS_DGEMM
!
!
      use iso_c_binding
      use omp_lib
!
      use hipfort
      use hipfort_check
      use hipfort_rocblas
!
      use m_precision
      use m_DGEMM_test_size
      use calypso_DGEMM
      use calypso_ROCmBLAS_DGEMM
      use calypso_ROCmBLAS_interface
      use DGEMM_test_openmp
      use DGEMM_test_omp_target
!
      use, intrinsic :: iso_fortran_env, only: real64
      implicit none
!
!
!$OMP requires unified_shared_memory
      real(real64) :: elapsed(nloop)
!
      integer(kind = kint), parameter                                   &
     &                     :: size_a = num_column_C * num_row_A
      integer(kind = kint), parameter                                   &
     &                     :: size_b = num_row_A *    num_row_C
      integer(kind = kint), parameter                                   &
     &                     :: size_c = num_column_C * num_row_C
!
      integer(c_int) :: m =   num_column_C
      integer(c_int) :: n =   num_row_C
      integer(c_int) :: k =   num_row_A
!
      integer(c_int) :: lda = num_column_C
      integer(c_int) :: ldb = num_row_A
      integer(c_int) :: ldc = num_column_C
!
      integer(c_size_t) :: Nabytes = size_a * kreal
      integer(c_size_t) :: Nbbytes = size_b * kreal
      integer(c_size_t) :: Ncbytes = size_c * kreal
!
      real(real64) :: start, finish
!
!specify whether the matrix is to be transposed or not.
!     rocblas_operation_none =      111
!     rocblas_operation_transpose = 112
!
      integer(c_int) :: transa = rocblas_operation_none
      integer(c_int) :: transb = rocblas_operation_none

      type(c_ptr) :: rocblas_handle = c_null_ptr
      type(c_ptr) :: da = c_null_ptr, db = c_null_ptr, dc = c_null_ptr
!
      real(kind = kreal), allocatable, target :: A(:,:), B(:,:), C(:,:)
      real(kind = kreal), allocatable :: A_org(:,:), B_org(:,:)
      real(kind = kreal), allocatable :: C_org(:,:), C_ref(:,:)
!
!
      integer(kind = kint) :: i, j, ij, iloop
      real(real64) :: sum_check
!
      write(*,"(a)")                                                    &
     &   "----- Running test 'DGEMM' (Fortran 2003 interfaces) ----- "
!
      allocate(A(lda,k), A_org(lda,k))
      allocate(B(ldb,n), B_org(ldb,n))
      allocate(C(ldc,n), C_org(ldc,n), C_ref(ldc,n))
!
!
      call RANDOM_NUMBER(A_org)    ! Initialize matrices
      call RANDOM_NUMBER(B_org)    ! Initialize matrices
      call RANDOM_NUMBER(C_org)    ! Initialize matrices
!
      call norm_dgemm_matrix_omp_target(lda, k, A_org)
      call norm_dgemm_matrix_omp_target(ldb, n, A_org)
      call norm_dgemm_matrix_omp_target(ldc, n, A_org)
!
! ----  Original mat multi   --------------
      write(*,"(a)",advance="no")  "--- DGEMM by hand writing -- "
!
!   Copy matrices from original
      call copy_dgemm_matrices_omp_target                               &
     &   (m, n, k, A_org, lda, B_org, ldb, C_org, ldc, A, B, C)
!
      start = OMP_GET_WTIME()
      call calypso_simple_dgemm                                         &
     &   (m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
      elapsed(1) = OMP_GET_WTIME() - start
      write(*,*)
      write(*, '("Time for non-parallelized: ",1pE16.6e3)') elapsed(1)
!
      call copy_dgemm_matrix_omp_target(ldc, n, C, C_ref)
!
! ----  CPU OpenMP --------------
      do iloop = 1, nloop
        write(*,"(a)",advance="no")                                     &
     &                           "--- DGEMM with OpenMP 2-collpase -- "
!
!   Copy matrices from original
        call copy_dgemm_matrices_omp_target                             &
     &     (m, n, k, A_org, lda, B_org, ldb, C_org, ldc, A, B, C)
!
        start = OMP_GET_WTIME()
      call calypso_dgemm_openmp                                         &
     &   (m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
        elapsed(iloop) = OMP_GET_WTIME() - start
!
        call check_matmul_error(sum_matmul_error_omp_target(n, ldc,     &
     &                                                      C_ref, C))
      end do
      do iloop = 1, nloop
        write(*,'(a, i3, a, 1pE16.6e3)')   "  Time of ", iloop,         &
     &       "-th CPU OpenMP: ", elapsed(iloop)
      end do
!
! ----  explicit kernel --------------
      do iloop = 1, nloop
        write(*,"(a)",advance="no")  "--- DGEMM with explicit mode -- "
!
!   Copy matrices from original
        call copy_dgemm_matrices_omp_target                             &
     &     (m, n, k, A_org, lda, B_org, ldb, C_org, ldc, A, B, C)
!
        start = OMP_GET_WTIME()
        call calypso_init_ROCmBLAS(rocblas_handle)
!$OMP target enter data map(to:A,B,C)
!$OMP target data use_device_addr(A,B,C)
        call omp_dgemm(rocblas_handle, transa, transb, m, n, k, alpha,  &
            c_loc(A),lda,c_loc(B),ldb,beta,c_loc(C),ldc)
!$OMP end target data
!$OMP target update from(C)
!$OMP target exit data map(delete:A,B,C)
        call calypso_fin_ROCmBLAS(rocblas_handle)
        elapsed(iloop) = OMP_GET_WTIME() - start
!
        call check_matmul_error(sum_matmul_error_omp_target(n, ldc,     &
     &                                                      C_ref, C))
      end do
      do iloop = 1, nloop
        write(*,'(a, i3, a, 1pE16.6e3)')   "  Time of ", iloop,         &
     &       "-th explicit kernel: ", elapsed(iloop)
      end do
!
! ----  usm kernel --------------
      do iloop = 1, nloop
        write(*,"(a)",advance="no")  "--- DGEMM with usm mode -- "
!
!   Copy matrices from original
        call copy_dgemm_matrices_omp_target                             &
     &     (m, n, k, A_org, lda, B_org, ldb, C_org, ldc, A, B, C)
!
        start = OMP_GET_WTIME()
        call calypso_init_ROCmBLAS(rocblas_handle)

        call omp_dgemm(rocblas_handle, transa, transb, m, n, k, alpha,  &
                       c_loc(A), lda, c_loc(B), ldb,beta, c_loc(C), ldc)
!
        call calypso_fin_ROCmBLAS(rocblas_handle)
        elapsed(iloop) = OMP_GET_WTIME() - start
!
        call check_matmul_error(sum_matmul_error_omp_target(n, ldc,     &
     &                                                      C_ref, C))
      end do
      do iloop = 1, nloop
        write(*,'(a, i3, a, 1pE16.6e3)')   "  Time of ", iloop,         &
     &       "-th usm kernel: ", elapsed(iloop)
      end do
!
      end program test_HIPmBLAS_DGEMM
!
