!
!      module m_filter_coefs
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine allocate_filter_coefs
!      subroutine deallocate_filter_coefs
!
!      subroutine allocate_num_near_all_f
!      subroutine allocate_num_near_all_w(filter)
!      subroutine allocate_nod_ele_near_all_w
!      subroutine allocate_nod_ele_near_1nod(numnod, numele)
!
!      subroutine deallocate_num_near_all_w
!      subroutine deallocate_nod_ele_near_all_w
!      subroutine deallocate_nod_ele_near_1nod
!
!      subroutine check_num_near_all_f(id_rank)
!      subroutine check_near_nod_all_filter(id_rank)
!      subroutine check_filter_functions(id_rank, id_base)
!
      module m_filter_coefs
!
      use m_precision
      use t_filter_coefs
!
      implicit none
!
!
      real(kind = kreal), allocatable :: filter_func(:)
      real(kind = kreal), allocatable :: filter_weight(:)
!
      integer(kind = kint) :: ntot_nod_near_all_w
      integer(kind = kint) :: nmax_nod_near_all_w
      integer(kind = kint) :: nmin_nod_near_all_w
      integer(kind = kint) :: ntot_nod_fcoefs
      integer(kind = kint), allocatable :: inod_all_w(:)
      integer(kind = kint), allocatable :: nnod_near_nod_all_w(:)
      integer(kind = kint), allocatable :: inod_stack_nod_all_w(:)
      integer(kind = kint), allocatable :: inod_near_nod_all_w(:)
!
      integer(kind = kint) :: ntot_ele_near_all_w
      integer(kind = kint) :: nmax_ele_near_all_w
      integer(kind = kint) :: nmin_ele_near_all_w
      integer(kind = kint), allocatable :: nele_near_nod_all_w(:)
      integer(kind = kint), allocatable :: iele_stack_nod_all_w(:)
      integer(kind = kint), allocatable :: iele_near_nod_all_w(:)
!
!
      integer(kind = kint), allocatable :: nnod_near_nod_weight(:)
!
      integer(kind = kint), allocatable :: i_exp_level_whole_nod(:)
      integer(kind = kint), allocatable :: itbl_near_nod_whole(:)
      integer(kind = kint) :: num_failed_whole
!
      integer(kind = kint), allocatable :: i_exp_level_fluid_nod(:)
      integer(kind = kint), allocatable :: itbl_near_nod_fluid(:)
      integer(kind = kint) :: num_failed_fluid
!
      integer(kind = kint), allocatable :: iflag_make_whole_filter(:)
      integer(kind = kint), allocatable :: iflag_make_fluid_filter(:)
      integer(kind = kint), allocatable :: iflag_make_moment_ele(:)
      integer(kind = kint), allocatable :: iele_make_moment_again(:)
      integer(kind = kint) :: nele_make_moment_again
!
!
      type(each_filter_coef), save :: fil_coef1
      type(each_filter_coef), save :: tmp_coef1
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_filter_coefs
!
      allocate( filter_func(ntot_nod_near_all_w) )
      allocate( filter_weight(ntot_nod_near_all_w) )
!
      filter_func = 0.0d0
      filter_weight = 0.0d0
!
      end subroutine allocate_filter_coefs
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_filter_coefs
!
      deallocate( filter_func )
      deallocate( filter_weight )
!
      end subroutine deallocate_filter_coefs
!
! ----------------------------------------------------------------------
!
      subroutine allocate_num_near_all_w(filter)
!
      use t_filter_coefficients
!
      type(filter_coefficients_type), intent(inout) :: filter
!
!
      ntot_nod_fcoefs = filter%ntot_nod
      allocate(inod_all_w(ntot_nod_fcoefs))
      allocate(nnod_near_nod_all_w(ntot_nod_fcoefs))
      allocate(inod_stack_nod_all_w(0:ntot_nod_fcoefs))
!
      allocate(nele_near_nod_all_w(ntot_nod_fcoefs))
      allocate(iele_stack_nod_all_w(0:ntot_nod_fcoefs))
!
      inod_all_w = 0
      nnod_near_nod_all_w = 0
      inod_stack_nod_all_w = 0
      nele_near_nod_all_w = 0
      iele_stack_nod_all_w = 0
!
      end subroutine allocate_num_near_all_w
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_ele_near_all_w
!
      allocate(inod_near_nod_all_w(ntot_nod_near_all_w))
      allocate(iele_near_nod_all_w(ntot_ele_near_all_w))
      inod_near_nod_all_w = 0
      iele_near_nod_all_w = 0
!
      end subroutine allocate_nod_ele_near_all_w
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_ele_near_1nod(numnod, numele, fil_coef)
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      call alloc_each_filter_coef(numnod, numele, fil_coef)
!
      allocate(nnod_near_nod_weight(numnod))
      nnod_near_nod_weight = 0
!
      end subroutine allocate_nod_ele_near_1nod
!
! -----------------------------------------------------------------------
!
      subroutine allocate_filter_num_sort_IO
!
      use m_nod_filter_comm_table
!
      allocate( i_exp_level_whole_nod(inter_nod_3dfilter) )
      allocate( i_exp_level_fluid_nod(inter_nod_3dfilter) )
      allocate( itbl_near_nod_whole(inter_nod_3dfilter) )
      allocate( itbl_near_nod_fluid(inter_nod_3dfilter) )
      i_exp_level_whole_nod = 0
      i_exp_level_fluid_nod = 0
      itbl_near_nod_whole = 0
      itbl_near_nod_fluid = 0
!
      end subroutine allocate_filter_num_sort_IO
!
! -----------------------------------------------------------------------
!
      subroutine allocate_correct_filter_flag(node, ele)
!
      use t_geometry_data
!
      type(node_data),           intent(in) :: node
      type(element_data),        intent(in) :: ele
!
      allocate(iflag_make_whole_filter(node%numnod))
      allocate(iflag_make_fluid_filter(node%numnod))
      allocate(iflag_make_moment_ele(ele%numele))
      allocate(iele_make_moment_again(ele%numele))
!
      if(node%numnod .gt. 0) then
        iflag_make_whole_filter = 0
        iflag_make_fluid_filter = 0
      end if
      if(ele%numele .gt. 0) then
        iflag_make_moment_ele =  0
        iele_make_moment_again = 0
      end if
!
      end subroutine allocate_correct_filter_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_near_all_w
!
      deallocate(inod_all_w)
      deallocate(inod_stack_nod_all_w)
      deallocate(nnod_near_nod_all_w)
      deallocate(iele_stack_nod_all_w)
      deallocate(nele_near_nod_all_w)
!
      end subroutine deallocate_num_near_all_w
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_ele_near_all_w
!
      deallocate(inod_near_nod_all_w)
      deallocate(iele_near_nod_all_w)
!
      end subroutine deallocate_nod_ele_near_all_w
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_ele_near_1nod(fil_coef)
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
      deallocate(nnod_near_nod_weight)
!
      call dealloc_each_filter_coef(fil_coef)
!
      end subroutine deallocate_nod_ele_near_1nod
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_filter_num_sort_IO
!
      deallocate( i_exp_level_whole_nod )
      deallocate( i_exp_level_fluid_nod )
      deallocate( itbl_near_nod_whole )
      deallocate( itbl_near_nod_fluid )
!
      end subroutine deallocate_filter_num_sort_IO
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_correct_filter_flag
!
      deallocate(iflag_make_whole_filter)
      deallocate(iflag_make_fluid_filter)
      deallocate(iflag_make_moment_ele)
      deallocate(iele_make_moment_again)
!
      end subroutine deallocate_correct_filter_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_num_near_all_f(id_rank)
!
      integer, intent(in) :: id_rank
      integer(kind = kint) :: inum
!
      write(50+id_rank,*) 'near node ID for filter and entire'
      write(50+id_rank,*)                                               &
     &      'max., min.: ', nmax_nod_near_all_w, nmin_nod_near_all_w
      do inum = 1, ntot_nod_fcoefs
        write(50+id_rank,*)                                             &
     &     inum, nnod_near_nod_all_w(inum)
      end do
!
      end subroutine check_num_near_all_f
!
! -----------------------------------------------------------------------
!
      subroutine check_near_nod_all_filter(id_rank)
!
      integer, intent(in) :: id_rank
      integer(kind = kint) :: inum, ist, ied
!
      write(50+id_rank,*) 'max and min. of near node ID for node ',     &
     &                 nmax_nod_near_all_w, nmin_nod_near_all_w
      do inum = 1, ntot_nod_fcoefs
        ist = inod_stack_nod_all_w(inum-1) + 1
        ied = inod_stack_nod_all_w(inum)
        write(50+id_rank,*) 'near node ID inod_near_nod_all_w',         &
     &     inum, inod_all_w(inum), ist, ied, nnod_near_nod_all_w(inum)
        write(50+id_rank,'(8i16)') inod_near_nod_all_w(ist:ied)
      end do
!
      end subroutine check_near_nod_all_filter
!
! -----------------------------------------------------------------------
!
      subroutine check_filter_functions(id_rank, id_base)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: id_base
      integer(kind = kint) :: inum, ist, ied, i
!
      do inum = 1, ntot_nod_fcoefs
        ist = inod_stack_nod_all_w(inum-1)
        ied = inod_stack_nod_all_w(inum)
        write(id_base+id_rank,*) 'filter',                              &
     &        inum, inod_all_w(inum), nnod_near_nod_all_w(inum)
        do i = 1, (ied-ist)
          write(id_base+id_rank,*) i, inod_near_nod_all_w(ist+i),       &
     &         filter_func(ist+i), filter_weight(ist+i)
        end do
      end do
!
      end subroutine check_filter_functions
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      end module m_filter_coefs
