! Copyright AMD 2024-2025, MIT License, contact Bob.Robey@amd.com
      module calypso_rocBLAS_interface
!
      implicit none
!
!  ---------------------------------------------------------------------
!
        interface
!
!  ---------------------------------------------------------------------
!
          subroutine init_rocBLAS(handle) bind(C)
            use iso_c_binding, only : c_ptr
            implicit none
            type(c_ptr)        :: handle
          end subroutine init_rocBLAS
!
!  ---------------------------------------------------------------------
!
          subroutine finalize_rocBLAS(handle) bind(C)
            use iso_c_binding, only : c_ptr
            implicit none
            type(c_ptr), value :: handle
          end subroutine finalize_rocBLAS
!
!  ---------------------------------------------------------------------
!
          subroutine omp_rocBLAS_dgemm(handle, ma, mb, m, n, k, alpha,  &
     &              a, lda, b, ldb, beta, c, ldc) bind(C)
            use iso_c_binding, only : c_ptr, c_int, c_double
            implicit none
            type(c_ptr),value  :: a,b,c
            type(c_ptr)        :: handle
            integer(c_int)     :: ma,mb,m,n,k,lda,ldb,ldc
            real(c_double)     :: alpha,beta
          end subroutine omp_rocBLAS_dgemm
        end interface
!
!  ---------------------------------------------------------------------
!
      end module calypso_rocBLAS_interface
