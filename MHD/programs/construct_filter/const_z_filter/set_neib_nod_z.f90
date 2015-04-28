!
!      module set_neib_nod_z
!
      module set_neib_nod_z
!
!        programmed by H. Matsui on June, 2007
!
      use m_precision
!
      implicit none
!
!      subroutine s_set_neib_nod_z(numnod, nsize, numfilter, nneib_nod, &
!     &          ineib_nod)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_neib_nod_z(numnod, nsize, numfilter, nneib_nod,  &
     &          ineib_nod)
!
      integer(kind = kint) :: numnod, nsize, numfilter
      integer(kind = kint), dimension(numnod,2) :: nneib_nod
      integer(kind = kint), dimension(numnod,nsize,2) ::ineib_nod
      integer(kind = kint) :: i, i1, j, inod
!
!
      do j = 1, numfilter
        do i = 1, numnod
          ineib_nod(i,j,1) = i-j;
          ineib_nod(i,j,2) = i+j;
        end do
      end do
!
      do j = numfilter, 1, -1
        do i = 1, numnod
          if (ineib_nod(i,j,1) .lt. 1) then
            nneib_nod(i,1) = nneib_nod(i,1) - 1
            nneib_nod(i,2) = nneib_nod(i,2) + 1
            ineib_nod(i,j,1) = -1
            ineib_nod(i,nneib_nod(i,2),2)                               &
     &            = ineib_nod(i,nneib_nod(i,2)-1,2) + 1
           end if
          if (ineib_nod(i,j,2) .gt. numnod) then
            nneib_nod(i,2) = nneib_nod(i,2) - 1
            nneib_nod(i,1) = nneib_nod(i,1) + 1
            ineib_nod(i,j,2) = -1
            ineib_nod(i,nneib_nod(i,1),1)                               &
     &            = ineib_nod(i,nneib_nod(i,1)-1,1) - 1
           end if
         end do
       end do
!
!
      end subroutine s_set_neib_nod_z
!
!-----------------------------------------------------------------------
!
      end module set_neib_nod_z
