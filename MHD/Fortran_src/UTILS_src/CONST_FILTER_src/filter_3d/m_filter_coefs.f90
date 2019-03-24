!
!      module m_filter_coefs
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine allocate_num_near_all_f
!      subroutine allocate_nod_ele_near_1nod(numnod, numele)
!
      module m_filter_coefs
!
      use m_precision
      use t_filter_coefs
      use t_filter_coefficients
!
      implicit none
!
!
      integer(kind = kint) :: nmax_nod_near_all_w
      integer(kind = kint) :: nmin_nod_near_all_w
!
      integer(kind = kint) :: nmax_ele_near_all_w
!
      integer(kind = kint), allocatable :: nnod_near_nod_weight(:)
!
      integer(kind = kint), allocatable :: iflag_make_whole_filter(:)
      integer(kind = kint), allocatable :: i_exp_level_whole_nod(:)
      integer(kind = kint), allocatable :: itbl_near_nod_whole(:)
      integer(kind = kint) :: num_failed_whole
!
      integer(kind = kint), allocatable :: iflag_make_fluid_filter(:)
      integer(kind = kint), allocatable :: i_exp_level_fluid_nod(:)
      integer(kind = kint), allocatable :: itbl_near_nod_fluid(:)
      integer(kind = kint) :: num_failed_fluid
!
      integer(kind = kint), allocatable :: iflag_make_moment_ele(:)
      integer(kind = kint), allocatable :: iele_make_moment_again(:)
      integer(kind = kint) :: nele_make_moment_again
!
!
      type(filter_coefficients_type), save :: filter_d1
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
!
      end module m_filter_coefs
