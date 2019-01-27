!sort_by_position_4_eb3d.f90
!     module sort_by_position_4_eb3d
!
!     written by H. Matsui on Aug., 2007
!
!!      subroutine s_sort_by_position_4_eb3d(nnod, ndiv, IGROUP,        &
!!     &          xx1, xx2, xx3, VAL, IS1)
!!      subroutine s_sort_by_position_with_ratio(num_domain,            &
!!     &          ndiv, part_tbl, nnod, IGROUP, xx1, xx2, xx3, VAL, IS1)
!!      subroutine s_sort_by_position_w_grp(nnod, ndiv, num_mat,        &
!!     &          mat_name, ntot_node_ele_grp, inod_stack_ele_grp,      &
!!     &          inod_ele_grp, num_egrp_sel, grp_sel_name, IGROUP,     &
!!     &          xx1, xx2, xx3, VAL, IS1)
!!
!!      subroutine s_sort_by_position_with_volume                       &
!!     &         (num_domain, ndiv, nnod, n_volume, tot_vol,            &
!!     &          IGROUP, xx1, xx2, xx3, VAL, IS1)
!!      subroutine s_sort_by_position_with_ratio_volume                 &
!!     &         (num_domain, ndiv, nnod,part_volume, n_volume, IGROUP, &
!!     &          xx1, xx2, xx3, VAL, IS1)
!
      module sort_by_position_4_eb3d
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_sort_by_position_4_eb3d(nnod, ndiv, IGROUP,         &
     &          xx1, xx2, xx3, VAL, IS1)
!
      use quicksort
      use sort_by_position_4_rcb
      use cal_minmax_and_stacks
      use sort_sphere_4_rcb
!
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndiv(3)
      real(kind = kreal), intent(in) :: xx1(nnod), xx2(nnod), xx3(nnod)
!
      real(kind = kreal), intent(inout) :: VAL(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
!
      integer(kind = kint) :: ncou, ip0
      integer(kind = kint) :: irest1, num1
!
!
      ip0 = ione
!
!      write(*,*) 'copy_position_sort_4_rcb'
      call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx1, ncou,       &
     &    VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
      call quicksort_real_w_index(nnod, VAL, ione, nnod, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
      call sorting_by_2nd_direction(nnod, ncou, xx1, xx2, VAL, IS1)
!
      call sorting_by_2nd_direction(nnod, ncou, xx2, xx3, VAL, IS1)
!
      call cal_divide_and_rest(num1, irest1, nnod, ndiv(1) )
!
!      write(*,*) 'set_domain_list_by_order'
      call set_domain_list_by_order(nnod, ione, ndiv(1), num1, irest1,  &
     &    IS1, IGROUP)
!
      call s_sort_by_position_4_2d(nnod, ndiv, IGROUP, xx1, xx2, xx3,   &
     &    VAL, IS1)
!
!
      end subroutine s_sort_by_position_4_eb3d
!
!   --------------------------------------------------------------------
!
      subroutine s_sort_by_position_with_ratio(num_domain,              &
     &          ndiv, part_tbl, nnod, IGROUP, xx1, xx2, xx3, VAL, IS1)
!
      use quicksort
      use sort_by_position_4_rcb
      use cal_minmax_and_stacks
      use sort_sphere_4_rcb
!
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndiv(3)
      real(kind = kreal), intent(in) :: xx1(nnod), xx2(nnod), xx3(nnod)
      integer(kind = kint), intent(in) :: num_domain
!
      real(kind = kreal), intent(inout) :: part_tbl(num_domain)
!
      real(kind = kreal), intent(inout) :: VAL(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
!
      integer(kind = kint) :: group_id(num_domain), domain_id(num_domain)
      integer(kind = kint) :: tbl_size, order, ncou, ip0, ndiv_2d, i
!
!
      ip0 = ione
      tbl_size = num_domain
      order = ione
      do i = 1, num_domain
        domain_id(i) = i
      end do
!
!      write(*,*) 'copy_position_sort_4_rcb'
      call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx1, ncou,       &
      &    VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
      call quicksort_real_w_index(nnod, VAL, ione, nnod, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
      call sorting_by_2nd_direction(nnod, ncou, xx1, xx2, VAL, IS1)
!
      call sorting_by_2nd_direction(nnod, ncou, xx2, xx3, VAL, IS1)
!
      call set_domain_list_with_part_tbl                                &
      &    (nnod, ncou, num_domain, ione, ndiv(1), part_tbl, IS1,       &
      &     IGROUP, tbl_size, order, group_id, domain_id)
      tbl_size = tbl_size / ndiv(1)
      do ip0 = 1, ndiv(1)
        call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx2, ncou,     &
        &      VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
        call quicksort_real_w_index(nnod, VAL, ione, ncou, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
        call sorting_by_2nd_direction(nnod, ncou, xx2, xx3, VAL, IS1)
!
        call sorting_by_2nd_direction(nnod, ncou, xx3, xx1, VAL, IS1)
!        write(*,*) 'set_domain_list_with_part_tbl size', tbl_size, ip0
        call set_domain_list_with_part_tbl                              &
        &    (nnod, ncou, num_domain, ndiv(1), ndiv(2), part_tbl, IS1,  &
        &     IGROUP, tbl_size, ip0, group_id, domain_id)
!
      end do
!
!
      ndiv_2d = ndiv(1)*ndiv(2)
      tbl_size = tbl_size / ndiv(2)
      do ip0 = 1, ndiv_2d
        call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx3, ncou,     &
        &      VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
        call quicksort_real_w_index(nnod, VAL, ione, ncou, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
        call sorting_by_2nd_direction(nnod, ncou, xx3, xx1, VAL, IS1)
!
        call sorting_by_2nd_direction(nnod, ncou, xx1, xx2, VAL, IS1)
!        write(*,*) 'set_domain_list_with_part_tbl size', tbl_size, ip0
        call set_domain_list_with_part_tbl                              &
        &    (nnod, ncou, num_domain, ndiv_2d, ndiv(3), part_tbl, IS1,  &
        &     IGROUP, tbl_size, ip0, group_id, domain_id)
!
      end do
!
!
      end subroutine s_sort_by_position_with_ratio
!
!   --------------------------------------------------------------------
!
      subroutine s_sort_by_position_w_grp(nnod, ndiv, num_mat,          &
     &          mat_name, ntot_node_ele_grp, inod_stack_ele_grp,        &
     &          inod_ele_grp, num_egrp_sel, egrp_sel_name, IGROUP,      &
     &          xx1, xx2, xx3, VAL, IS1)
!
      use quicksort
      use sort_by_position_4_rcb
      use cal_minmax_and_stacks
      use sort_sphere_4_rcb
!
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndiv(3)
      real(kind = kreal), intent(in) :: xx1(nnod), xx2(nnod), xx3(nnod)
!
      integer(kind = kint), intent(in) ::  num_egrp_sel
      character(len=kchara), intent(in) :: egrp_sel_name(num_egrp_sel)
!
      integer(kind = kint), intent(in) :: num_mat, ntot_node_ele_grp
      integer(kind = kint), intent(in) :: inod_stack_ele_grp(0:num_mat)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_ele_grp(ntot_node_ele_grp)
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      real(kind = kreal), intent(inout) :: VAL(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
!
      integer(kind = kint) :: ncou, ip0
      integer(kind = kint) :: irest1, num1
      integer(kind = kint) :: j
!
!
      ip0 = ione
      IGROUP(nnod) = 0
!
      do j = 1, num_egrp_sel + 1
!
        if (j .le. num_egrp_sel) then
          write(*,*) 'copy_position_sort_4_rcb'
          call copy_position_sort_by_ele_grp(egrp_sel_name(j), num_mat, &
     &        mat_name, ntot_node_ele_grp, inod_stack_ele_grp,          &
     &        inod_ele_grp, nnod, IGROUP, xx1, ncou, VAL, IS1)
        else
          call copy_position_sort_for_rest(nnod, IGROUP, xx1, ncou,     &
     &        VAL, IS1)
        end if
!
!        write(*,*) 'quicksort_real_w_index'
        call quicksort_real_w_index(nnod, VAL, ione, nnod, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
        call sorting_by_2nd_direction(nnod, ncou, xx1, xx2, VAL, IS1)
!
        call sorting_by_2nd_direction(nnod, ncou, xx2, xx3, VAL, IS1)
!
        call cal_divide_and_rest(num1, irest1, nnod, ndiv(1) )
!
!        write(*,*) 'set_domain_list_by_order'
        call set_domain_list_w_rev(nnod, j, ndiv(1), num1, irest1,      &
     &      IS1, IGROUP)
!
      end do
!
!
      call s_sort_by_position_4_2d(nnod, ndiv, IGROUP, xx1, xx2, xx3,   &
     &      VAL, IS1)
!
      end subroutine s_sort_by_position_w_grp
!
!   --------------------------------------------------------------------
!
      subroutine s_sort_by_position_4_2d(nnod, ndiv, IGROUP,            &
     &          xx1, xx2, xx3, VAL, IS1)
!
      use quicksort
      use sort_by_position_4_rcb
      use cal_minmax_and_stacks
      use sort_sphere_4_rcb
!
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndiv(3)
      real(kind = kreal), intent(in) :: xx1(nnod), xx2(nnod), xx3(nnod)
!
      real(kind = kreal), intent(inout) :: VAL(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
!
      integer(kind = kint) :: ncou, ip0, ndiv_2d
      integer(kind = kint) :: irest1, num1
!
!
      do ip0 = 1, ndiv(1)
        call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx2, ncou,     &
     &      VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
        call quicksort_real_w_index(nnod, VAL, ione, ncou, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
        call sorting_by_2nd_direction(nnod, ncou, xx2, xx3, VAL, IS1)
!
        call sorting_by_2nd_direction(nnod, ncou, xx3, xx1, VAL, IS1)
!
        call cal_divide_and_rest(num1, irest1, ncou, ndiv(2) )
!
!        write(*,*) 'set_domain_list_by_order', ndiv(1), ndiv(2)
        call set_domain_list_by_order(nnod, ndiv(1), ndiv(2), num1,     &
     &      irest1, IS1, IGROUP)
!
      end do
!
!
      ndiv_2d = ndiv(1)*ndiv(2)
      do ip0 = 1, ndiv_2d
        call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx3, ncou,     &
     &      VAL, IS1)
!
        write(*,*) 'quicksort_real_w_index'
        call quicksort_real_w_index(nnod, VAL, ione, ncou, IS1)
!
        write(*,*) 'sorting_by_2nd_direction'
        call sorting_by_2nd_direction(nnod, ncou, xx3, xx1, VAL, IS1)
!
        call sorting_by_2nd_direction(nnod, ncou, xx1, xx2, VAL, IS1)
!
        call cal_divide_and_rest(num1, irest1, ncou, ndiv(3) )
!
        write(*,*) 'set_domain_list_by_order'
        call set_domain_list_by_order(nnod, ndiv_2d, ndiv(3), num1,     &
     &      irest1, IS1, IGROUP)
!
      end do
!
      end subroutine s_sort_by_position_4_2d
!
!   --------------------------------------------------------------------
!
      subroutine s_sort_by_position_with_volume                         &
     &         (num_domain, ndiv, nnod, n_volume, tot_vol,              &
     &          IGROUP, xx1, xx2, xx3, VAL, IS1)
!
      use quicksort
      use sort_by_position_4_rcb
      use cal_minmax_and_stacks
      use sort_sphere_4_rcb
!
!
      integer(kind = kint), intent(in) :: num_domain
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndiv(3)
      real(kind = kreal), intent(in) :: xx1(nnod), xx2(nnod), xx3(nnod)
      real(kind = kreal), intent(in) :: n_volume(nnod), tot_vol
!
      real(kind = kreal), intent(inout) :: VAL(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
!
      integer(kind = kint) :: group_id(num_domain), domain_id(num_domain)
      integer(kind = kint) :: tbl_size, order, ncou, ip0, ndiv_2d, i
!
      real(kind = kreal) :: grp_volume
!
      ip0 = ione
      tbl_size = num_domain
      order = ione
      do i = 1, num_domain
        domain_id(i) = i
      end do
!
!      write(*,*) 'copy_position_sort_4_rcb'
      call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx1, ncou,       &
     &    VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
      call quicksort_real_w_index(nnod, VAL, ione, nnod, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
      call sorting_by_2nd_direction(nnod, ncou, xx1, xx2, VAL, IS1)
!
      call sorting_by_2nd_direction(nnod, ncou, xx2, xx3, VAL, IS1)
!
      grp_volume = tot_vol / ndiv(1)
      call set_domain_list_by_volume(nnod, ione, ndiv(1), ncou,         &
     &           grp_volume, IS1, n_volume, IGROUP)

      grp_volume = grp_volume / ndiv(2)
      do ip0 = 1, ndiv(1)
        call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx2, ncou,     &
     &      VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
        call quicksort_real_w_index(nnod, VAL, ione, ncou, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
        call sorting_by_2nd_direction(nnod, ncou, xx2, xx3, VAL, IS1)
!
        call sorting_by_2nd_direction(nnod, ncou, xx3, xx1, VAL, IS1)
!        write(*,*) 'set_domain_list_by_volume size', tbl_size, ip0
        call set_domain_list_by_volume(nnod, ndiv(1), ndiv(2), ncou,    &
     &      grp_volume, IS1, n_volume, IGROUP)
!
      end do
!
!
      grp_volume = grp_volume / ndiv(3)
      ndiv_2d = ndiv(1)*ndiv(2)
      do ip0 = 1, ndiv_2d
        call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx3, ncou,     &
     &      VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
        call quicksort_real_w_index(nnod, VAL, ione, ncou, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
        call sorting_by_2nd_direction(nnod, ncou, xx3, xx1, VAL, IS1)
!
        call sorting_by_2nd_direction(nnod, ncou, xx1, xx2, VAL, IS1)
!        write(*,*) 'set_domain_list_by_part_tbl size', tbl_size, ip0
        call set_domain_list_by_volume(nnod, ndiv_2d, ndiv(3), ncou,    &
     &      grp_volume, IS1, n_volume, IGROUP)
!
      end do
!
      end subroutine s_sort_by_position_with_volume
!
!   --------------------------------------------------------------------
!
      subroutine s_sort_by_position_with_ratio_volume                   &
     &         (num_domain, ndiv, nnod,part_volume, n_volume, IGROUP,   &
     &          xx1, xx2, xx3, VAL, IS1)
!
      use quicksort
      use sort_by_position_4_rcb
      use cal_minmax_and_stacks
      use sort_sphere_4_rcb
!
!
      integer(kind = kint), intent(in) :: num_domain
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndiv(3)
      real(kind = kreal), intent(in) :: xx1(nnod), xx2(nnod), xx3(nnod)
      real(kind = kreal), intent(inout) :: part_volume(num_domain)
      real(kind = kreal), intent(in) :: n_volume(nnod)
!
      real(kind = kreal), intent(inout) :: VAL(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
!
      integer(kind = kint) :: group_id(num_domain), domain_id(num_domain)
      integer(kind = kint) :: tbl_size, order, ncou, ip0, ndiv_2d, i
!
!
      ip0 = ione
      tbl_size = num_domain
      order = ione
      do i = 1, num_domain
        domain_id(i) = i
      end do
!
!      write(*,*) 'copy_position_sort_4_rcb'
      call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx1, ncou,       &
     &    VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
      call quicksort_real_w_index(nnod, VAL, ione, nnod, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
      call sorting_by_2nd_direction(nnod, ncou, xx1, xx2, VAL, IS1)
!
      call sorting_by_2nd_direction(nnod, ncou, xx2, xx3, VAL, IS1)
!
      call set_domain_list_with_part_volume                             &
     &   (nnod, ncou, num_domain, ione, ndiv(1), part_volume, n_volume, &
     &    IS1, IGROUP, tbl_size, order, group_id, domain_id)
!
      tbl_size = tbl_size / ndiv(1)
      do ip0 = 1, ndiv(1)
        call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx2, ncou,     &
     &      VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
        call quicksort_real_w_index(nnod, VAL, ione, ncou, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
        call sorting_by_2nd_direction(nnod, ncou, xx2, xx3, VAL, IS1)
!
        call sorting_by_2nd_direction(nnod, ncou, xx3, xx1, VAL, IS1)
!        write(*,*) 'set_domain_list_with_part_tbl size', tbl_size, ip0
        call set_domain_list_with_part_volume                           &
     &    (nnod, ncou, num_domain, ndiv(1), ndiv(2), part_volume,       &
     &     n_volume, IS1, IGROUP, tbl_size, ip0, group_id, domain_id)
!
      end do
!
!
      ndiv_2d = ndiv(1)*ndiv(2)
      tbl_size = tbl_size / ndiv(2)
      do ip0 = 1, ndiv_2d
        call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx3, ncou,     &
     &      VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
        call quicksort_real_w_index(nnod, VAL, ione, ncou, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
        call sorting_by_2nd_direction(nnod, ncou, xx3, xx1, VAL, IS1)
!
        call sorting_by_2nd_direction(nnod, ncou, xx1, xx2, VAL, IS1)
!        write(*,*) 'set_domain_list_by_part_tbl size', tbl_size, ip0
        call set_domain_list_with_part_volume                           &
     &    (nnod, ncou, num_domain, ndiv_2d, ndiv(3), part_volume,       &
     &     n_volume, IS1, IGROUP, tbl_size, ip0, group_id, domain_id)
      end do
!
!
      end subroutine s_sort_by_position_with_ratio_volume
!
!   --------------------------------------------------------------------
!
      end module sort_by_position_4_eb3d
