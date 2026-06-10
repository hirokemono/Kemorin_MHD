!>@file   set_neib_connect_z.f90
!!@brief  module set_neib_connect_z
!!
!!@author H. Matsui
!!@date Programmed in April, 2007
!
!>@brief  Set neibghoring information for vertical filter
!!
!!@verbatim
!!      subroutine s_set_neib_connect_z(totalele, nfilter2_1,           &
!!     &                                nneib_ele, jdx)
!!        integer(kind = kint), intent(in) :: nneib_ele(totalele,2)
!!        integer(kind = kint), intent(in) :: totalele
!!        integer(kind = kint), intent(in) :: nfilter2_1
!!        integer(kind = kint), intent(inout)                           &
!!     &                     :: jdx(totalele,nfilter2_1,3)
!!@endverbatim
!
      module set_neib_connect_z
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
      subroutine s_set_neib_connect_z(totalele, nfilter2_1,             &
     &                                nneib_ele, jdx)
!
      integer(kind = kint), intent(in) :: nneib_ele(totalele,2)
      integer(kind = kint), intent(in) :: totalele
      integer(kind = kint), intent(in) :: nfilter2_1
!
      integer(kind = kint), intent(inout)                               &
     &                     :: jdx(totalele,nfilter2_1,3)
!
      integer(kind = kint) :: i, j, j1
!
!
      do i = 1, totalele
        j1 = 1
        do j = nneib_ele(i,1), 1, -1
          jdx(i,j1,1) = j
          jdx(i,j1,2) = 1
          j1 = j1 + 1
        end do
        jdx(i,j1,1) = 0
        jdx(i,j1,2) = 2
        j1 = j1 + 1
        do j = 1, nneib_ele(i,2)
          jdx(i,j1,1) = j
          jdx(i,j1,2) = 2
          j1 = j1 + 1
        end do
      end do
!
      end subroutine s_set_neib_connect_z
!
!-----------------------------------------------------------------------
!
      end module set_neib_connect_z
