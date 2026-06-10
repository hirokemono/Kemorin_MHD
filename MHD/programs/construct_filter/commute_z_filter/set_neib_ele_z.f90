!
!      module set_neib_ele_z
!
!        programmed by H. Matsui on June, 2007
!
!!      subroutine s_set_neib_ele_z(numele, nsize, numfilter,           &
!!     &                            nneib_ele, ineib_ele)
!!        integer(kind = kint), intent(in) :: numele, numfilter, nsize
!!        integer(kind = kint), intent(inout) :: nneib_ele(numele,2)
!!        integer(kind = kint), intent(inout)                           &
!!     &                       :: ineib_ele(numele,nsize,2)
!!
      module set_neib_ele_z
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
      subroutine s_set_neib_ele_z(numele, nsize, numfilter,             &
     &                            nneib_ele, ineib_ele)
!
      integer(kind = kint), intent(in) :: numele, numfilter, nsize
!
      integer(kind = kint), intent(inout) :: nneib_ele(numele,2)
        integer(kind = kint), intent(inout)                             &
     &                       :: ineib_ele(numele,nsize,2)
!
      integer(kind = kint) :: i, j
!
!
      do j = 1, numfilter
        do i = 1, numele
          ineib_ele(i,j,1) = i-j;
          ineib_ele(i,j,2) = i+j;
        end do
      end do
!
      do j = numfilter, 1, -1
        do i = 1, numele
          if (ineib_ele(i,j,1) .lt. 1) then
            nneib_ele(i,1) = nneib_ele(i,1) - 1
            nneib_ele(i,2) = nneib_ele(i,2) + 1
            ineib_ele(i,j,1) = -1
            ineib_ele(i,nneib_ele(i,2),2)                               &
     &            = ineib_ele(i,nneib_ele(i,2)-1,2) + 1
           end if
          if (ineib_ele(i,j,2) .gt. numele) then
            nneib_ele(i,2) = nneib_ele(i,2) - 1
            nneib_ele(i,1) = nneib_ele(i,1) + 1
            ineib_ele(i,j,2) = -1
            ineib_ele(i,nneib_ele(i,1),1)                               &
     &            = ineib_ele(i,nneib_ele(i,1)-1,1) - 1
           end if
         end do
       end do
!
!
      end subroutine s_set_neib_ele_z
!
!-----------------------------------------------------------------------
!
      end module set_neib_ele_z
