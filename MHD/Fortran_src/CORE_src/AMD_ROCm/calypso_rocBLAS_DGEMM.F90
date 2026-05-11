!>@file   calypso_rocBLAS_DGEMM.F90
!!@brief  module calypso_rocBLAS_DGEMM
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief Array and loop sizes for DGEMM tests
!!
!!@verbatim
!!      subroutine calypso_OpenMP_target_DGEMM                          &
!!     &         (m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
!!        integer(c_int), intent(in) :: m, n, k
!!        integer(c_int), intent(in) :: lda, ldb, ldc
!!        real(kind = kreal), intent(in) :: alpha, beta
!!        real(kind = kreal), intent(in) :: A(lda,k)
!!        real(kind = kreal), intent(in) :: B(ldb,n)
!!        real(kind = kreal), intent(inout) :: C(ldc,n)
!!
!!      subroutine calypso_init_rocBLAS(rocblas_handle)
!!      subroutine calypso_fin_rocBLAS(rocblas_handle)
!!        type(c_ptr), intent(inout) :: rocblas_handle
!!
!!      subroutine alloc_rocBLAS_dgemm_work(Nabytes, Nbbytes, Ncbytes,  &
!!     &                                    A_cptr, B_cptr, C_cptr)
!!      subroutine dealloc_rocBLAS_dgemm_work(A_cptr, B_cptr, C_cptr)
!!        integer(c_size_t), intent(in) :: Nabytes, Nbbytes, Ncbytes
!!        type(c_ptr), intent(inout) :: A_cptr, B_cptr, C_cptr
!!
!!      subroutine calypso_OpenMP_rocBLAS_dgemm                         &
!!     &         (rocblas_handle, transa, transb, m, n, k,              &
!!     &          alpha, A_mat, lda, B_mat, ldb, beta, C_mat, ldc)
!!        type(c_ptr), intent(in) :: rocblas_handle
!!        integer(c_int), intent(in) :: transa, transb
!!        integer(c_int), intent(in) :: m, n, k
!!        integer(c_int), intent(in) :: lda, ldb, ldc
!!        real(c_double), intent(in) :: alpha, beta
!!        real(kind = kreal), intent(in), target :: A_mat(lda,k)
!!        real(kind = kreal), intent(in), target :: B_mat(ldb,n)
!!        real(kind = kreal), intent(inout), target :: C_mat(ldc,n)
!!      subroutine calypso_hip_rocBLAS_dgemm(rocblas_handle,            &
!!     &          Nabytes, Nbbytes, Ncbytes, transa, transb, m, n, k,   &
!!     &          alpha, A_mat, lda, B_mat, ldb, beta, C_mat, ldc,      &
!!     &          A_cptr, B_cptr, C_cptr)
!!        type(c_ptr), intent(in) :: rocblas_handle
!!        integer(c_int), intent(in) :: transa, transb
!!        integer(c_size_t), intent(in) :: Nabytes, Nbbytes, Ncbytes
!!        integer(c_int), intent(in) :: m, n, k
!!        integer(c_int), intent(in) :: lda, ldb, ldc
!!        real(c_double), intent(in) :: alpha, beta
!!        real(kind = kreal), intent(in), target :: A_mat(lda,k)
!!        real(kind = kreal), intent(in), target :: B_mat(ldb,n)
!!        real(kind = kreal), intent(inout), target :: C_mat(ldc,n)
!!        type(c_ptr), intent(inout) :: A_cptr, B_cptr, C_cptr
!!@endverbatim
      module calypso_rocBLAS_DGEMM
!
      use iso_c_binding
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_OpenMP_target_DGEMM                            &
     &         (m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
!
      integer(c_int), intent(in) :: m, n, k
      integer(c_int), intent(in) :: lda, ldb, ldc
      real(kind = kreal), intent(in) :: alpha, beta
      real(kind = kreal), intent(in) :: A(lda,k)
      real(kind = kreal), intent(in) :: B(ldb,n)
!
      real(kind = kreal), intent(inout) :: C(ldc,n)
!
      integer(kind = kint) :: i, j, ij
!
!
!$OMP target teams distribute parallel do collapse(2)
      do j = 1, n
        do i = 1, m
          C(i,j) = beta * C(i,j)
!
          do ij = 1, k
            C(i,j) = C(i,j) + alpha * A(i,ij) * B(ij,j)
          end do
        end do
      end do
!$OMP end target teams distribute parallel do
!
      end subroutine calypso_OpenMP_target_DGEMM
!
!  ---------------------------------------------------------------------
#ifdef _AMD_ROCM_
!  ---------------------------------------------------------------------
!
      subroutine calypso_init_rocBLAS(rocblas_handle)
!
      use hipfort_check
      use hipfort_rocblas
!
      type(c_ptr), intent(inout) :: rocblas_handle
!
      call rocblasCheck(rocblas_create_handle(rocblas_handle))
!
      end subroutine calypso_init_rocBLAS
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_fin_rocBLAS(rocblas_handle)
!
      use hipfort_check
      use hipfort_rocblas
!
      type(c_ptr), intent(inout) :: rocblas_handle
!
      call rocblasCheck(rocblas_destroy_handle(rocblas_handle))
!
      end subroutine calypso_fin_rocBLAS
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_rocBLAS_dgemm_work(Nabytes, Nbbytes, Ncbytes,    &
     &                                    A_cptr, B_cptr, C_cptr)
!
      use hipfort
      use hipfort_check
!
      integer(c_size_t), intent(in) :: Nabytes, Nbbytes, Ncbytes
!
      type(c_ptr), intent(inout) :: A_cptr, B_cptr, C_cptr
!
!
      call hipCheck(hipMalloc(A_cptr,Nabytes))
      call hipCheck(hipMalloc(B_cptr,Nbbytes))
      call hipCheck(hipMalloc(C_cptr,Ncbytes))
!
      end subroutine alloc_rocBLAS_dgemm_work
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_rocBLAS_dgemm_work(A_cptr, B_cptr, C_cptr)
!
      use hipfort
      use hipfort_check
!
      type(c_ptr), intent(inout) :: A_cptr, B_cptr, C_cptr
!
!
      call hipCheck(hipFree(A_cptr))
      call hipCheck(hipFree(B_cptr))
      call hipCheck(hipFree(C_cptr))
!
      end subroutine dealloc_rocBLAS_dgemm_work
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_OpenMP_rocBLAS_dgemm                           &
     &         (rocblas_handle, transa, transb, m, n, k,                &
     &          alpha, A_mat, lda, B_mat, ldb, beta, C_mat, ldc)
!
      use hipfort
      use hipfort_check
      use hipfort_rocblas
!
      type(c_ptr), intent(in) :: rocblas_handle
      integer(c_int), intent(in) :: transa, transb
!
      integer(c_int), intent(in) :: m, n, k
      integer(c_int), intent(in) :: lda, ldb, ldc
      real(c_double), intent(in) :: alpha, beta
      real(kind = kreal), intent(in), target :: A_mat(lda,k)
      real(kind = kreal), intent(in), target :: B_mat(ldb,n)
!
      real(kind = kreal), intent(inout), target :: C_mat(ldc,n)
!
!$OMP target enter data map(to:A_mat,B_mat,C_mat)
!$OMP target data use_device_addr(A_mat,B_mat,C_mat)
!      call rocblasCheck(rocblas_set_pointer_mode(rocblas_handle, 0))
      call rocblasCheck(rocblas_dgemm(rocblas_handle, transa, transb,   &
     &    m, n, k, alpha, c_loc(A_mat), lda, c_loc(B_mat), ldb,         &
     &    beta, c_loc(C_mat), ldc))
!$OMP end target data
!$OMP target update from(C_mat)
!$OMP target exit data map(delete:A_mat,B_mat,C_mat)
!
      end subroutine calypso_OpenMP_rocBLAS_dgemm
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_hip_rocBLAS_dgemm(rocblas_handle,              &
     &          Nabytes, Nbbytes, Ncbytes, transa, transb, m, n, k,     &
     &          alpha, A_mat, lda, B_mat, ldb, beta, C_mat, ldc,        &
     &          A_cptr, B_cptr, C_cptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocblas
!
      type(c_ptr), intent(in) :: rocblas_handle
      integer(c_int), intent(in) :: transa, transb
      integer(c_size_t), intent(in) :: Nabytes, Nbbytes, Ncbytes
!
      integer(c_int), intent(in) :: m, n, k
      integer(c_int), intent(in) :: lda, ldb, ldc
      real(c_double), intent(in) :: alpha, beta
      real(kind = kreal), intent(in), target :: A_mat(lda,k)
      real(kind = kreal), intent(in), target :: B_mat(ldb,n)
!
      real(kind = kreal), intent(inout), target :: C_mat(ldc,n)
      type(c_ptr), intent(inout) :: A_cptr, B_cptr, C_cptr
!
!Transfer from host to device
      call hipCheck(hipMemcpy(A_cptr, c_loc(A_mat(1,1)),                &
     &              Nabytes, hipMemcpyHostToDevice))
      call hipCheck(hipMemcpy(B_cptr, c_loc(B_mat(1,1)),                &
     &              Nbbytes, hipMemcpyHostToDevice))
      call hipCheck(hipMemcpy(C_cptr, c_loc(C_mat(1,1)),                &
     &              Ncbytes, hipMemcpyHostToDevice))
!
!      call rocblasCheck(rocblas_set_pointer_mode(rocblas_handle, 0))
      call rocblasCheck(rocblas_dgemm(rocblas_handle, transa, transb,   &
     &    m, n, k, alpha, A_cptr, lda, B_cptr, ldb, beta, C_cptr, ldc))

      call hipCheck(hipDeviceSynchronize())

! Transfer data back to host memory
      call hipCheck(hipMemcpy(c_loc(C_mat(1,1)), C_cptr,                &
     &                        Ncbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_hip_rocBLAS_dgemm
!
!  ---------------------------------------------------------------------
#endif
!  ---------------------------------------------------------------------
!
      end module calypso_rocBLAS_DGEMM
