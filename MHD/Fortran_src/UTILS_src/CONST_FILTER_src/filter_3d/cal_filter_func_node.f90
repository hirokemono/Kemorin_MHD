!
!      module cal_filter_func_node
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine const_commute_filter_coefs(mom_nod)
!!      subroutine set_simple_fluid_filter(dxidxs, mom_nod)
!!        type(dxidx_data_type), intent(inout) :: dxidxs
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
!
      module cal_filter_func_node
!
      use m_precision
!
      use m_constants
!
      implicit none
!
      integer(kind = kint), parameter :: num_fixed_point = 0
      private :: num_fixed_point
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_commute_filter_coefs(mom_nod)
!
      use m_geometry_data
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use t_filter_moments
      use cal_filter_func_each_node
      use expand_filter_area_4_1node
      use set_simple_filters
!
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      integer(kind = kint) :: inod, ierr
!
!
      call init_4_cal_fileters(node1%numnod, node1%internal_node,       &
     &    ele1%numele, ele1%nnod_4_ele)
!
      write(70+my_rank,*) ' Best condition for filter'
!
      do inod = inod_start_filter, inod_end_filter
        call const_filter_func_nod_by_nod(inod, ierr)
!
        nnod_near_nod_weight(inod) = nnod_near_1nod_weight
        call cal_filter_moms_each_nod_type(inod, mom_nod)
      end do
!
      end subroutine const_commute_filter_coefs
!
! -----------------------------------------------------------------------
!
      subroutine const_fluid_filter_coefs
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use cal_filter_func_each_node
      use expand_filter_area_4_1node
!
      integer(kind = kint) :: inod, ierr
!
!
      call init_4_cal_fluid_fileters
!
      write(70+my_rank,*) ' Best condition for fluid filter'
!
      do inod = inod_start_filter, inod_end_filter
        call const_fluid_filter_nod_by_nod(inod, ierr)
      end do
!
      end subroutine const_fluid_filter_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_simple_filter(dxidxs, mom_nod)
!
      use m_geometry_data
      use m_ctl_params_4_gen_filter
      use t_filter_dxdxi
      use t_filter_moments
      use m_filter_coefs
      use cal_simple_filter_each_node
      use expand_filter_area_4_1node
      use set_simple_filters
!
      integer(kind = kint) :: inod
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!
      call init_4_cal_fileters(node1%numnod, node1%internal_node,       &
     &    ele1%numele, ele1%nnod_4_ele)
!
      i_exp_level_1nod_weight = maximum_neighbour
      do inod = inod_start_filter, inod_end_filter
        call set_simple_filter_nod_by_nod(node1, inod, dxidxs%dx_nod)
!
        nnod_near_nod_weight(inod) = nnod_near_1nod_weight
        call cal_filter_moms_each_nod_type(inod, mom_nod)
      end do
!
      end subroutine set_simple_filter
!
! -----------------------------------------------------------------------
!
      subroutine set_simple_fluid_filter(dxidxs, mom_nod)
!
      use m_geometry_data
      use m_ctl_params_4_gen_filter
      use t_filter_dxdxi
      use t_filter_moments
      use m_filter_coefs
      use cal_simple_filter_each_node
      use expand_filter_area_4_1node
!
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
!
      integer(kind = kint) :: inod
!
!
      call init_4_cal_fluid_fileters
!
      write(80+my_rank,*) ' Best condition for filter'
!
      i_exp_level_1nod_weight = maximum_neighbour
      do inod = inod_start_filter, inod_end_filter
        call set_simple_fl_filter_nod_by_nod                            &
     &     (node1, inod, dxidxs%dx_nod, mom_nod)
      end do
!
      end subroutine set_simple_fluid_filter
!
! -----------------------------------------------------------------------
!
      end module cal_filter_func_node
