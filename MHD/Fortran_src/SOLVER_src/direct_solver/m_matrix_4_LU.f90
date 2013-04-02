!
!      module m_matrix_4_LU
!
!      Written by H. Matsui
!
!       subroutine allocate_matrix_4_LU
!       subroutine deallocate_matrix_4_LU
!
      module m_matrix_4_LU
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable ::  a_nod(:,:)
      real(kind = kreal), allocatable ::  b_nod(:)
      real(kind = kreal) :: d_nod
!
      integer(kind = kint) :: ncomp_lu
      integer(kind = kint), allocatable ::  indx(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_matrix_4_LU
!
       allocate( a_nod(ncomp_lu, ncomp_lu) )
       allocate( b_nod(ncomp_lu) )
       allocate( indx(ncomp_lu) )
!
       a_nod = 0.0d0
       b_nod = 0.0d0
       indx = 0
!
      end subroutine allocate_matrix_4_LU
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_matrix_4_LU
!
        deallocate( a_nod )
        deallocate( b_nod )
        deallocate( indx )
!
      end subroutine deallocate_matrix_4_LU
!
!  ---------------------------------------------------------------------
!
      end module m_matrix_4_LU
