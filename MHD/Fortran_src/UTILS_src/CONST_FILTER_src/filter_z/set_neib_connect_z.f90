!
!      module set_neib_connect_z
!
      module set_neib_connect_z
!
!        programmed by H. Matsui on June, 2007
!
      use m_precision
!
      implicit none
!
!      subroutine s_set_neib_connect_z
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_neib_connect_z
!
!     Written by Kemorin
!
      use m_geometry_parameter
      use m_commute_filter_z
      use m_neibor_data_z
!
      integer(kind = kint) :: i, i1, j, j1
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
!
      end subroutine s_set_neib_connect_z
!
!-----------------------------------------------------------------------
!
      end module set_neib_connect_z
