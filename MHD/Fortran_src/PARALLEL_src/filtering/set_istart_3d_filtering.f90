!set_istart_3d_filtering.f90
!      module set_istart_3d_filtering
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on Nov., 2008
!
!
!      subroutine count_num_3d_filtering_sum(ngrp_fil, istack_fil,      &
!     &          ntot_fil, num_near_fil, min_sum_fil, max_sum_fil,      &
!     &          istack_sum_fil, ntot_sum_fil)
!      subroutine set_start_id_4_3d_filtering(ngrp_fil, istack_fil,     &
!     &          ntot_fil, num_near_fil, istack_sum_fil, ntot_sum_fil,  &
!     &          ist_sum_fil, ied_sum_fil)
!
      module set_istart_3d_filtering
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_3d_filtering_sum(ngrp_fil, istack_fil,       &
     &          ntot_fil, num_near_fil, min_sum_fil, max_sum_fil)
!
      integer(kind = kint), intent(in) :: ngrp_fil
      integer(kind = kint), intent(in) :: istack_fil(0:ngrp_fil)
      integer(kind = kint), intent(in) :: ntot_fil
      integer(kind = kint), intent(in) :: num_near_fil(ntot_fil)
!
      integer(kind = kint), intent(inout) :: min_sum_fil(ngrp_fil)
      integer(kind = kint), intent(inout) :: max_sum_fil(ngrp_fil)
!
      integer(kind = kint) :: igrp, ist, ied, i
!
!
      do igrp = 1, ngrp_fil
        ist = istack_fil(igrp-1) + 1
        ied = istack_fil(igrp)
        min_sum_fil(igrp) = 0
        max_sum_fil(igrp) = 0
        do i = ist, ied
          min_sum_fil(igrp) = min(min_sum_fil(igrp),num_near_fil(i))
          max_sum_fil(igrp) = max(max_sum_fil(igrp),num_near_fil(i))
        end do
      end do
!
      end subroutine count_num_3d_filtering_sum
!
!  ---------------------------------------------------------------------
!
      subroutine set_start_id_4_3d_filtering(ngrp_fil, istack_fil,      &
     &          ntot_fil, num_near_fil, istack_sum_fil, ntot_sum_fil,   &
     &          ist_sum_fil, ied_sum_fil)
!
      integer(kind = kint), intent(in) :: ngrp_fil
      integer(kind = kint), intent(in) :: istack_fil(0:ngrp_fil)
      integer(kind = kint), intent(in) :: ntot_fil
      integer(kind = kint), intent(in) :: num_near_fil(ntot_fil)
!
      integer(kind = kint), intent(in) :: istack_sum_fil(0:ngrp_fil)
      integer(kind = kint), intent(in) :: ntot_sum_fil
!
      integer(kind = kint), intent(inout) :: ist_sum_fil(ntot_sum_fil)
      integer(kind = kint), intent(inout) :: ied_sum_fil(ntot_sum_fil)
!
      integer(kind = kint) :: igrp, ist, ied, inum, jst, jj, jnum
!
!
      do igrp = 1, ngrp_fil
        ist = istack_fil(igrp-1) + 1
        ied = istack_fil(igrp)
        if( ied .ge. ist) then
          jst = istack_sum_fil(igrp-1)
          do jnum = 1, num_near_fil(ied)
            jj = jst + jnum
            ied_sum_fil(jj) = ied
          end do
!
          do inum = ied, ist+1, -1
            if (num_near_fil(inum) .ne. num_near_fil(inum-1) )  then
              do jnum = 1, num_near_fil(inum)
                jj = jst + jnum
                ist_sum_fil(jj) = inum
              end do
            end if
          end do
          do jnum = 1, num_near_fil(ist)
            jj = jst + jnum
            ist_sum_fil(jj) = ist
          end do
        end if
!
      end do
!
      end subroutine set_start_id_4_3d_filtering
!
!  ---------------------------------------------------------------------
!
      end module set_istart_3d_filtering
