!set_iflag_for_used_ele
!      module set_iflag_for_used_ele
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine s_set_iflag_for_used_ele(numele, e_multi,             &
!     &          num_mat, num_mat_bc, mat_istack, mat_item,             &
!     &          ngrp_ele, id_ele_grp, iflag_used_ele)
!      subroutine set_iflag_for_used_ele_overlap(numele,                &
!     &          num_mat, num_mat_bc, mat_istack, mat_item,             &
!     &          ngrp_ele, id_ele_grp, iflag_used_ele)
!
      module set_iflag_for_used_ele
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_iflag_for_used_ele(numele, e_multi,              &
     &          num_mat, num_mat_bc, mat_istack, mat_item,              &
     &          ngrp_ele, id_ele_grp, iflag_used_ele)
!
      integer(kind=kint), intent(in) :: numele
      real(kind = kreal), intent(in) :: e_multi(numele)
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
!
      integer(kind = kint), intent(in) :: ngrp_ele
      integer(kind = kint), intent(in) :: id_ele_grp(ngrp_ele)
      integer(kind = kint), intent(inout) :: iflag_used_ele(numele)
!
      integer(kind = kint) :: jgrp, jnum, jele, jg, jst, jed
!
!
      iflag_used_ele(1:numele) = 0
      do jgrp = 1, ngrp_ele
        jg = id_ele_grp(jgrp)
        if(jg .le. 0) then
          do jele = 1, numele
            if(e_multi(jele) .gt. 0.0d0) iflag_used_ele(jele) = 1
          end do
        else
          jst = mat_istack(jg-1) + 1
          jed = mat_istack(jg)
          do jnum = jst, jed
            jele = mat_item(jnum)
            if(e_multi(jele) .gt. 0.0d0) iflag_used_ele(jele) = 1
          end do
        end if
      end do
!
      end subroutine s_set_iflag_for_used_ele
!
!  ---------------------------------------------------------------------
!
      subroutine set_iflag_for_used_ele_overlap(numele,                 &
     &          num_mat, num_mat_bc, mat_istack, mat_item,              &
     &          ngrp_ele, id_ele_grp, iflag_used_ele)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
!
      integer(kind = kint), intent(in) :: numele, ngrp_ele
      integer(kind = kint), intent(in) :: id_ele_grp(ngrp_ele)
      integer(kind = kint), intent(inout) :: iflag_used_ele(numele)
!
      integer(kind = kint) :: jgrp, jnum, jele, jg, jst, jed
!
!
      iflag_used_ele(1:numele) = 0
      do jgrp = 1, ngrp_ele
        jg = id_ele_grp(jgrp)
        if(jg .le. 0) then
          do jele = 1, numele
            iflag_used_ele(jele) = 1
          end do
        else
          jst = mat_istack(jg-1) + 1
          jed = mat_istack(jg)
          do jnum = jst, jed
            jele = mat_item(jnum)
            iflag_used_ele(jele) = 1
          end do
        end if
      end do
!
      end subroutine set_iflag_for_used_ele_overlap
!
!  ---------------------------------------------------------------------
!
      end module set_iflag_for_used_ele
