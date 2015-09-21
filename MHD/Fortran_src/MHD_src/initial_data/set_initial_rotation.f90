!
!     module set_initial_rotation
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine set_initial_velo_1                                   &
!!     &         (numnod, xx, ncomp_nod, i_velo, i_press, d_nod)
!!      subroutine set_initial_velo_2                                   &
!!     &         (numnod, xx, ncomp_nod, i_velo, i_press, d_nod)
!!      subroutine set_initial_velo_3                                   &
!!     &         (numnod, xx, ncomp_nod, i_velo, i_press, d_nod)
!
      module set_initial_rotation
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_velo_1                                     &
     &         (numnod, xx, ncomp_nod, i_velo, i_press, d_nod)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: ncomp_nod, i_velo, i_press
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod
!
!$omp parallel do
      do inod = 1, numnod
       d_nod(inod,i_press)  = 0.0d0
       d_nod(inod,i_velo  ) = 0.0d0
       d_nod(inod,i_velo+1) = -xx(inod,3)
       d_nod(inod,i_velo+2) =  xx(inod,2)
      end do
!$omp end parallel do
!
      end subroutine set_initial_velo_1
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_velo_2                                     &
     &         (numnod, xx, ncomp_nod, i_velo, i_press, d_nod)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: ncomp_nod, i_velo, i_press
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod
!
!$omp parallel do
      do inod = 1, numnod
       d_nod(inod,i_press)  = 0.0d0
       d_nod(inod,i_velo  ) =  xx(inod,3)
       d_nod(inod,i_velo+1) =  0.0d0
       d_nod(inod,i_velo+2) = -xx(inod,1)
      end do
!$omp end parallel do
!
      end subroutine set_initial_velo_2
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_velo_3                                     &
     &         (numnod, xx, ncomp_nod, i_velo, i_press, d_nod)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: ncomp_nod, i_velo, i_press
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod
!
!$omp parallel do
      do inod = 1, numnod
       d_nod(inod,i_press)  = 0.0d0
       d_nod(inod,i_velo  ) = -xx(inod,2)
       d_nod(inod,i_velo+1) =  xx(inod,1)
       d_nod(inod,i_velo+2) = 0.0d0
      end do
!$omp end parallel do
!
      end subroutine set_initial_velo_3
!
!-----------------------------------------------------------------------
!
      end module set_initial_rotation
