!
!      module m_filter_coefs
!
      module m_filter_coefs
!
!     Written by H. Matsui on Nov., 2006
!
      use m_precision
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
!
      integer(kind = kint) :: nnod_near_1nod_weight
      integer(kind = kint) :: nnod_near_1nod_filter
      integer(kind = kint), allocatable :: inod_near_1nod_weight(:)
      integer(kind = kint), allocatable :: idist_from_center_1nod(:)
      integer(kind = kint), allocatable :: iweight_1nod_weight(:)
      integer(kind = kint) :: i_exp_level_1nod_weight
!
      integer(kind = kint) :: nele_near_1nod_weight
      integer(kind = kint) :: nele_near_1nod_filter
      integer(kind = kint), allocatable :: iele_near_1nod_weight(:)
!
      integer(kind = kint), allocatable :: nnod_near_nod_weight(:)
!
      real(kind = kreal), allocatable :: weight_1nod(:)
      real(kind = kreal), allocatable :: filter_1nod(:)
!
      integer(kind = kint), allocatable :: i_exp_level_whole_nod(:)
      integer(kind = kint), allocatable :: i_exp_level_fluid_nod(:)
!
      integer(kind = kint), allocatable :: itbl_near_nod_whole(:)
      integer(kind = kint), allocatable :: itbl_near_nod_fluid(:)
!
      integer(kind = kint), allocatable :: iflag_make_whole_filter(:)
      integer(kind = kint), allocatable :: iflag_make_fluid_filter(:)
      integer(kind = kint), allocatable :: iflag_make_moment_ele(:)
      integer(kind = kint), allocatable :: iele_make_moment_again(:)
      integer(kind = kint) :: nele_make_moment_again
!
!
      integer(kind = kint) :: nnod_near_1nod_tmp
      integer(kind = kint) :: i_exp_level_1nod_tmp
      integer(kind = kint), allocatable :: inod_near_1nod_tmp(:)
      real(kind = kreal), allocatable :: weight_tmp(:)
      real(kind = kreal), allocatable :: filter_tmp(:)
!
      integer(kind = kint) :: nele_near_1nod_tmp
      integer(kind = kint), allocatable :: iele_near_1nod_tmp(:)
!
      integer(kind = kint) :: num_failed_whole
      integer(kind = kint) :: num_failed_fluid
!
!      subroutine allocate_filter_coefs(ntot_nod_near_nod)
!      subroutine deallocate_filter_coefs
!
!      subroutine allocate_inod_all_w
!      subroutine allocate_num_near_all_f
!      subroutine allocate_num_near_all_w
!      subroutine allocate_nod_ele_near_all_w
!      subroutine allocate_nod_ele_near_1nod(numnod, numele)
!      subroutine allocate_nod_ele_1nod_tmp(numnod, numele)
!
!      subroutine deallocate_num_near_all_w
!      subroutine deallocate_nod_ele_near_all_w
!      subroutine deallocate_nod_ele_near_1nod
!      subroutine deallocate_nod_ele_1nod_tmp
!
!      subroutine check_num_near_all_f(my_rank)
!      subroutine check_near_nod_all_filter(my_rank)
!      subroutine check_filter_functions(my_rank, id_base)
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
      subroutine allocate_inod_all_w
!
      use m_filter_coef_combained
!
      allocate(inod_all_w(ntot_nod_3d_filter))
      inod_all_w = 0
!
      end subroutine allocate_inod_all_w
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_near_all_w
!
      use m_filter_coef_combained
!
      allocate(nnod_near_nod_all_w(ntot_nod_3d_filter))
      allocate(inod_stack_nod_all_w(0:ntot_nod_3d_filter))
!
      allocate(nele_near_nod_all_w(ntot_nod_3d_filter))
      allocate(iele_stack_nod_all_w(0:ntot_nod_3d_filter))
!
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
      subroutine allocate_nod_ele_near_1nod(numnod, numele)
!
      integer(kind = kint), intent(in) :: numnod, numele
!
      allocate(inod_near_1nod_weight(numnod))
      allocate(idist_from_center_1nod(numnod))
      allocate(iweight_1nod_weight(numnod))
!
      allocate(iele_near_1nod_weight(numele))
!
      allocate(nnod_near_nod_weight(numnod))
!
      allocate( weight_1nod(numnod) )
      allocate( filter_1nod(numnod) )
!
      inod_near_1nod_weight =   0
      idist_from_center_1nod = -1
      iweight_1nod_weight =    -1
!
      iele_near_1nod_weight = 0
!
      nnod_near_nod_weight = 0
!
      weight_1nod =  0.0d0
      filter_1nod =  0.0d0
!
      end subroutine allocate_nod_ele_near_1nod
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_ele_1nod_tmp(numnod, numele)
!
      integer(kind = kint), intent(in) :: numnod, numele
!
      allocate(inod_near_1nod_tmp(numnod))
      allocate(iele_near_1nod_tmp(numele))
!
      allocate( weight_tmp(numnod) )
      allocate( filter_tmp(numnod) )
!
      inod_near_1nod_tmp =   0
      iele_near_1nod_tmp = 0
!
      weight_tmp =  0.0d0
      filter_tmp =  0.0d0
!
      end subroutine allocate_nod_ele_1nod_tmp
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
      subroutine allocate_correct_filter_flag(numnod, numele)
!
      integer(kind = kint), intent(in) :: numnod, numele
!
      allocate(iflag_make_whole_filter(numnod))
      allocate(iflag_make_fluid_filter(numnod))
      allocate(iflag_make_moment_ele(numele))
      allocate(iele_make_moment_again(numele))
      iflag_make_whole_filter = 0
      iflag_make_fluid_filter = 0
      iflag_make_moment_ele =  0
      iele_make_moment_again = 0
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
      subroutine deallocate_nod_ele_near_1nod
!
      deallocate(inod_near_1nod_weight)
      deallocate(idist_from_center_1nod)
      deallocate(iweight_1nod_weight)
!
      deallocate(iele_near_1nod_weight)
!
      deallocate(nnod_near_nod_weight)
!
      deallocate( filter_1nod )
      deallocate( weight_1nod )
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
!
      subroutine deallocate_nod_ele_1nod_tmp
!
      deallocate(inod_near_1nod_tmp)
      deallocate(iele_near_1nod_tmp)
!
      deallocate( weight_tmp )
      deallocate( filter_tmp )
!
      end subroutine deallocate_nod_ele_1nod_tmp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_num_near_all_f(my_rank)
!
      use m_filter_coef_combained
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: inum
!
      write(50+my_rank,*) 'near node ID for filter and entire'
      write(50+my_rank,*)                                               &
     &      'max., min.: ', nmax_nod_near_all_w, nmin_nod_near_all_w
      do inum = 1, ntot_nod_3d_filter
        write(50+my_rank,*)                                             &
     &     inum, nnod_near_nod_all_w(inum)
      end do
!
      end subroutine check_num_near_all_f
!
! -----------------------------------------------------------------------
!
      subroutine check_near_nod_all_filter(my_rank)
!
      use m_filter_coef_combained
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: inum, ist, ied
!
      write(50+my_rank,*) 'max and min. of near node ID for node ',     &
     &                 nmax_nod_near_all_w, nmin_nod_near_all_w
      do inum = 1, ntot_nod_3d_filter
        ist = inod_stack_nod_all_w(inum-1) + 1
        ied = inod_stack_nod_all_w(inum)
        write(50+my_rank,*) 'near node ID inod_near_nod_all_w',         &
     &     inum, inod_all_w(inum), ist, ied, nnod_near_nod_all_w(inum)
        write(50+my_rank,'(8i16)') inod_near_nod_all_w(ist:ied)
      end do
!
      end subroutine check_near_nod_all_filter
!
! -----------------------------------------------------------------------
!
      subroutine check_filter_functions(my_rank, id_base)
!
      use m_filter_coef_combained
!
      integer(kind = kint), intent(in) :: my_rank, id_base
      integer(kind = kint) :: inum, ist, ied, i
!
      do inum = 1, ntot_nod_3d_filter
        ist = inod_stack_nod_all_w(inum-1)
        ied = inod_stack_nod_all_w(inum)
        write(id_base+my_rank,*) 'filter',                              &
     &        inum, inod_all_w(inum), nnod_near_nod_all_w(inum)
        do i = 1, (ied-ist)
          write(id_base+my_rank,*) i, inod_near_nod_all_w(ist+i),       &
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
