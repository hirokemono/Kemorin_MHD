!start_end_4_2d_ele_grping.f90
!      module start_end_4_2d_ele_grping
!
!      Written by Kemorin on Mar., 2008
!
!      subroutine set_start_end_egrping(ref_ele,                        &
!     &          dmin_grping, dmax_grping, max_prev, item_st, item_ed)
!      subroutine set_2nd_start_end_egrp(ref_ele,                       &
!     &          dmin_grping, dmax_grping, max_prev,                    &
!     &          item_st, item_ed, jtem_st, jtem_ed)
!
      module start_end_4_2d_ele_grping
!
      use m_precision
!
      implicit    none
!
      private :: find_start_end_sorted_grping
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_start_end_egrping(numele, ref_ele,                 &
     &          dmin_grping, dmax_grping, max_prev, item_st, item_ed)
!
      use m_constants
      use m_geometry_constants
      use m_work_4_add_egrp_sph
      use quicksort
!
      integer(kind = kint), intent(in) :: numele
      real(kind = kreal), intent(in) :: ref_ele(numele)
      real(kind = kreal), intent(in) :: dmin_grping
      real(kind = kreal), intent(in) :: dmax_grping
!
      integer(kind = kint), intent(inout) :: item_st, item_ed
      real(kind = kreal), intent(inout) :: max_prev
!
      integer(kind = kint) :: ist_ele, iele
!
!
      if(dmin_grping .ge. max_prev) then
        ist_ele = item_ed
      else
!
        ist_ele = ione
        do iele = 1, numele
          wk1_4_sort(iele) = ref_ele(iele)
          iele_4_sort(iele) = iele
        end do
        call quicksort_real_w_index(numele, wk1_4_sort, ione,           &
     &      numele, iele_4_sort)
      end if
!
      call find_start_end_sorted_grping(ist_ele, numele,                &
     &     wk1_4_sort, dmin_grping, dmax_grping, item_st, item_ed )
      max_prev = dmax_grping
!
      end subroutine set_start_end_egrping
!
!   --------------------------------------------------------------------
!
      subroutine set_2nd_start_end_egrp(numele, ref_ele,                &
     &          dmin_grping, dmax_grping, max_prev,                     &
     &          item_st, item_ed, jtem_st, jtem_ed)
!
      use m_work_4_add_egrp_sph
      use quicksort
!
      integer(kind = kint), intent(in) :: numele
      real(kind = kreal), intent(in) :: ref_ele(numele)
      real(kind = kreal), intent(in) :: dmin_grping
      real(kind = kreal), intent(in) :: dmax_grping
      integer(kind = kint), intent(inout) :: item_st, item_ed
      integer(kind = kint), intent(inout) :: jtem_st, jtem_ed
      real(kind = kreal), intent(inout) :: max_prev
!
      integer(kind = kint) :: jst_ele, inum, iele
!
!
      if(item_ed .lt. item_st) then
        jtem_st = item_st
        jtem_ed = item_ed
        return
      end if
!
      if(dmin_grping .ge. max_prev) then
        jst_ele = jtem_ed + 1
      else
!
        jst_ele = item_st
        do inum = item_st, item_ed
          iele = iele_4_sort(inum)
          wk2_4_sort(inum) = ref_ele(iele)
        end do
        call quicksort_real_w_index(item_ed, wk2_4_sort,                &
     &      item_st, item_ed, iele_4_sort)
!
      end if
!
      call find_start_end_sorted_grping(jst_ele, item_ed,               &
     &    wk2_4_sort(1), dmin_grping, dmax_grping, jtem_st, jtem_ed )
      max_prev = dmax_grping
!
      end subroutine set_2nd_start_end_egrp
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine find_start_end_sorted_grping(ist_ele, ied_ele,         &
     &          sorted_array, min_grping, max_grping,                   &
     &          item_start, item_end)
!
      integer(kind = kint), intent(in) :: ist_ele, ied_ele
      real(kind= kreal), intent(in) :: sorted_array(ied_ele)
      real(kind= kreal), intent(in) :: min_grping, max_grping
      integer(kind = kint), intent(inout) :: item_start, item_end
!
      integer(kind = kint) :: iele
!
      item_start = ist_ele
      do iele = ist_ele, ied_ele
        if ( sorted_array(iele) .lt. min_grping ) then
          item_start = iele + 1
          item_end =   iele
        else if ( sorted_array(iele) .ge. max_grping ) then
          item_end = iele - 1
          exit
        else if (iele .eq. ied_ele) then
          item_end = iele
          exit
        end if
      end do
!
      end subroutine find_start_end_sorted_grping
!
!   --------------------------------------------------------------------
!
      end module start_end_4_2d_ele_grping
