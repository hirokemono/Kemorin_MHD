!sort_by_position_4_rcb.f90
!     module sort_by_position_4_rcb
!
!     written by H. Matsui on Aug., 2007
!
!      subroutine sort_4_rcb(nnod, iter, ip0, IGROUP, idir,             &
!     &          xx1, x2, xx3, VAL, IS1)
!      subroutine copy_position_sort_by_ele_grp(tgt_grp_name, num_mat,  &
!     &          mat_name, ntot_node_ele_grp, inod_stack_ele_grp,       &
!     &          inod_ele_grp, nnod, IGROUP, rr, ncou, VAL, IS1)
!      subroutine copy_position_sort_for_rest(nnod, IGROUP, rr, ncou,   &
!     &          VAL, IS1)
!
!      subroutine copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx_1,     &
!     &          ncou, VAL, IS1)
!      subroutine sorting_by_2nd_direction(nnod, ncou, xx_1, xx_2,      &
!     &          VAL, IS1)
!
      module sort_by_position_4_rcb
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: s_sort_by_position_4_rcb
      private :: set_domain_4_rcb
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sort_4_rcb(nnod, iter, ip0, IGROUP, idir,              &
     &          xx1, xx2, xx3, VAL, IS1)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: xx1(nnod)
      real(kind = kreal), intent(in) :: xx2(nnod)
      real(kind = kreal), intent(in) :: xx3(nnod)
!
      integer(kind = kint), intent(in) :: iter, ip0, idir
!
      real(kind = kreal), intent(inout) :: VAL(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
!
!
      if (idir .eq. 1) then
        call s_sort_by_position_4_rcb(nnod, iter, ip0, IGROUP,          &
     &      xx1, xx2, xx3, VAL, IS1)
      else if (idir .eq. 2) then
        call s_sort_by_position_4_rcb(nnod, iter, ip0, IGROUP,          &
     &      xx2, xx3, xx1, VAL, IS1)
      else if (idir .eq. 3) then
        call s_sort_by_position_4_rcb(nnod, iter, ip0, IGROUP,          &
     &      xx3, xx1, xx2, VAL, IS1)
      end if
!
      end subroutine sort_4_rcb
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine s_sort_by_position_4_rcb(nnod, iter, ip0, IGROUP,      &
     &          xx1, xx2, xx3, VAL, IS1)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: iter, ip0
      real(kind = kreal), intent(in) :: xx1(nnod), xx2(nnod), xx3(nnod)
!
      real(kind = kreal), intent(inout) :: VAL(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
!
      integer(kind = kint) :: ncou
!
!
      call copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx1, ncou,       &
     &    VAL, IS1)
!
      call quicksort_real_w_index(nnod, VAL, ione, ncou, IS1)
!
      call sorting_by_2nd_direction(nnod, ncou, xx1, xx2, VAL, IS1)
!
      call sorting_by_2nd_direction(nnod, ncou, xx2, xx3, VAL, IS1)
!
!
      call set_domain_4_rcb(nnod, iter, ip0, IGROUP, ncou, IS1)
!
      end subroutine s_sort_by_position_4_rcb
!
!   --------------------------------------------------------------------
!
      subroutine copy_position_sort_by_ele_grp(tgt_grp_name, num_mat,   &
     &          mat_name, ntot_node_ele_grp, inod_stack_ele_grp,        &
     &          inod_ele_grp, nnod, IGROUP, rr, ncou, VAL, IS1)
!
      integer(kind = kint), intent(in) :: num_mat, ntot_node_ele_grp
      integer(kind = kint), intent(in) :: inod_stack_ele_grp(0:num_mat)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_ele_grp(ntot_node_ele_grp)
      character(len=kchara), intent(in) :: mat_name(num_mat)
      character(len=kchara), intent(in) :: tgt_grp_name
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: rr(nnod)
!
      integer(kind = kint), intent(inout) :: ncou
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
      real(kind = kreal), intent(inout) :: VAL(nnod)
!
      integer(kind = kint) :: i, ist, ied, inum, inod
      integer(kind = kint), parameter :: ione = 1
!
      ncou = 0
      do i = 1, num_mat
        if (mat_name(i) .eq. tgt_grp_name) then
          ist = inod_stack_ele_grp(i-1) + 1
          ied = inod_stack_ele_grp(i)
          do inum = ist, ied
            inod = inod_ele_grp(inum)
            if (IGROUP(inod) .eq. 0) then
              ncou = ncou + 1
              IS1(ncou)= i
              VAL(ncou)= rr(i)
              IGROUP(inod) = ione
            end if
          end do
        end if
      end do
!
      end subroutine copy_position_sort_by_ele_grp
!
!   --------------------------------------------------------------------
!
      subroutine copy_position_sort_for_rest(nnod, IGROUP, rr, ncou,    &
     &          VAL, IS1)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: rr(nnod)
!
      integer(kind = kint), intent(inout) :: ncou
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
      real(kind = kreal), intent(inout) :: VAL(nnod)
!
      integer(kind = kint) :: i, ist, ied, inum, inod
      integer(kind = kint), parameter :: ione = 1
!
      ncou = 0
      do i= 1, nnod
        if (IGROUP(inod) .eq. 0) then
          ncou = ncou + 1
          IS1(ncou)= i
          VAL(ncou)= rr(i)
          IGROUP(inod) = ione
        end if
      end do
!
      end subroutine copy_position_sort_for_rest
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine copy_position_sort_4_rcb(nnod, ip0, IGROUP, xx_1,      &
     &          ncou, VAL, IS1)
!
      integer(kind = kint), intent(in) :: nnod, ip0
      integer(kind = kint), intent(in) :: IGROUP(nnod)
      real(kind = kreal), intent(in) :: xx_1(nnod)
!
      integer(kind = kint), intent(inout) :: ncou
      integer(kind = kint), intent(inout) :: IS1(nnod)
      real(kind = kreal), intent(inout) :: VAL(nnod)
!
      integer(kind = kint) :: i
!
      ncou= 0
      do i= 1, nnod
        if (IGROUP(i) .eq. ip0) then
          ncou= ncou + 1
          IS1(ncou)= i
          VAL(ncou)= xx_1(i)
        end if
      end do
!
      end subroutine copy_position_sort_4_rcb
!
!   --------------------------------------------------------------------
!
      subroutine sorting_by_2nd_direction(nnod, ncou, xx_1, xx_2,       &
     &          VAL, IS1)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: nnod, ncou
      real(kind = kreal), intent(in) :: xx_1(nnod), xx_2(nnod)
!
      real(kind = kreal), intent(inout) :: VAL(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
!
      integer(kind = kint) :: ist, ied, i, inod, inod1, inod0
!
!
      do i = 1, ncou
        inod = IS1(i)
        VAL(i) = xx_2(inod)
      end do
!
      i = 1
      do
        ist = i
        ied = i
        do
          i = i + 1
          inod1 = IS1(i)
          inod0 = IS1(i-1)
          if (xx_1(inod1) .ne. xx_1(inod0)) exit
          ied = i
          if (i .ge. ncou) exit
        end do
        if(ied .gt. ist) then
          call quicksort_real_w_index(nnod, VAL, ist, ied, IS1)
        end if
        if (i.ge.ncou) exit
      end do
!
      end subroutine sorting_by_2nd_direction
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_domain_4_rcb(nnod, iter, ip0, IGROUP, ncou, IS1)
!
      integer(kind = kint), intent(in) :: nnod, iter, ip0, ncou
      integer(kind = kint), intent(in) :: IS1(nnod)
!
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
      integer(kind = kint) :: ic, in
!
      do ic= 1, ncou/2
        in= IS1(ic)
        IGROUP(in)= ip0 + 2**(iter-1)
      enddo
!
      end subroutine set_domain_4_rcb
!
!   --------------------------------------------------------------------
!
      end module sort_by_position_4_rcb
