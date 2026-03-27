!>@file   calypso_rocblas_matmuls.f90
!!@brief  module calypso_rocblas_matmuls
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in March, 2026
!!
!>@brief Matrix-Matrix-product using AMD ROCm GPGPU environment
!!
!!@verbatim
!!      subroutine calypso_omp_offload_matprod(np1, np2, nab, alpha,    &
!!     &                                       Amat, Bmat, beta, Prod)
!!        integer(kind = kint), intent(in) :: nab, np2, np1
!!        real(kind = kreal), intent(in) :: alpha, beta
!!        real(kind = kreal), intent(in) :: Amat(np1,nab)
!!        real(kind = kreal), intent(in) :: Bmat(nab,np2)
!!        real(kind = kreal), intent(inout) :: Prod(np1,np2)
!!
!!      subroutine calypso_rocblas_dgemm_init(rocblas_handle)
!!      subroutine calypso_rocblas_dgemm_fin(rocblas_handle)
!!        type(c_ptr), intent(inout) :: rocblas_handle
!!      subroutine calypso_alloc_rocblas_dgemm                          &
!!     &         (Nabytes, Nbbytes, Ncbytes, A_cptr, B_cptr, C_cptr)
!!      subroutine calypso_dealloc_rocblas_dgemm(A_cptr, B_cptr, C_cptr)
!!        integer(c_size_t), intent(in) :: Nabytes, Nbbytes, Ncbytes
!!        type(c_ptr), intent(inout) :: A_cptr, B_cptr, C_cptr
!!
!!      subroutine calypso_rocblas_dgemm(rocblas_handle,                &
!!     &          Nabytes, Nbbytes, Ncbytes, transa, transb, m, n, k,   &
!!     &          alpha, A_mat, lda, B_mat, ldb, beta, C_mat, ldc,      &
!!     &          A_cptr, B_cptr, C_cptr)
!!      subroutine calypso_omp_rocblas_dgemm                            &
!!     &         (rocblas_handle, transa, transb, m, n, k,              &
!!     &          alpha, A_mat, lda, B_mat, ldb, beta, C_mat, ldc)
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
!
      module calypso_rocblas_matmuls
!
      use m_precision
      use iso_c_binding
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine calypso_omp_offload_matprod(np1, np2, nab, alpha,      &
     &                                       Amat, Bmat, beta, Prod)
!
      integer(kind = kint), intent(in) :: nab, np2, np1
      real(kind = kreal), intent(in) :: alpha, beta
      real(kind = kreal), intent(in) :: Amat(np1,nab)
      real(kind = kreal), intent(in) :: Bmat(nab,np2)
!
      real(kind = kreal), intent(inout) :: Prod(np1,np2)
!
      integer(kind = kint) :: jj, kk, ll
!
!
!$OMP target teams distribute parallel do collapse(2)
      do kk = 1, np2
        do ll = 1, np1
          Prod(ll,kk) = beta * Prod(ll,kk)
          do jj = 1, nab
            Prod(ll,kk) = Prod(ll,kk)                                   &
     &                   + alpha * Amat(ll,jj) * Bmat(jj,kk)
          end do
        end do
      end do
!$OMP end target teams distribute parallel do
!
      end subroutine calypso_omp_offload_matprod
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine calypso_rocblas_dgemm_init(rocblas_handle)
!
      use hipfort_check
      use hipfort_rocblas
!
      type(c_ptr), intent(inout) :: rocblas_handle
!
!
      call rocblasCheck(rocblas_create_handle(rocblas_handle))
!
      end subroutine calypso_rocblas_dgemm_init
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_rocblas_dgemm_fin(rocblas_handle)
!
      use hipfort_check
      use hipfort_rocblas
!
      type(c_ptr), intent(inout) :: rocblas_handle
!
      call rocblasCheck(rocblas_destroy_handle(rocblas_handle))
!
      end subroutine calypso_rocblas_dgemm_fin
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_alloc_rocblas_dgemm                            &
     &         (Nabytes, Nbbytes, Ncbytes, A_cptr, B_cptr, C_cptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocblas
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
      end subroutine calypso_alloc_rocblas_dgemm
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_dealloc_rocblas_dgemm(A_cptr, B_cptr, C_cptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocblas
!
      type(c_ptr), intent(inout) :: A_cptr, B_cptr, C_cptr
!
!
      call hipCheck(hipFree(A_cptr))
      call hipCheck(hipFree(B_cptr))
      call hipCheck(hipFree(C_cptr))
!
      end subroutine calypso_dealloc_rocblas_dgemm
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_rocblas_dgemm(rocblas_handle,                  &
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
      end subroutine calypso_rocblas_dgemm
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_omp_rocblas_dgemm                              &
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
      end subroutine calypso_omp_rocblas_dgemm
!
!  ---------------------------------------------------------------------
!
      end module calypso_rocblas_matmuls
