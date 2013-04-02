!
!      module cal_filter_func_node
!
      module cal_filter_func_node
!
!     Written by H. Matsui on Mar., 2008
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
!      subroutine const_filter_func_nod_by_nod
!      subroutine const_fluid_filter_nod_by_nod
!
!      subroutine set_simple_filter
!      subroutine set_simple_fluid_filter
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_commute_filter_coefs
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use cal_filter_func_each_node
      use expand_filter_area_4_1node
!
      integer(kind = kint) :: inod
!
!
      call init_4_cal_fileters
!
      write(70+my_rank,*) ' Best condition for filter'
!
      do inod = inod_start_filter, inod_end_filter
        call const_filter_func_nod_by_nod(inod)
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
      integer(kind = kint) :: inod
!
!
      call init_4_cal_fluid_fileters
!
      write(70+my_rank,*) ' Best condition for fluid filter'
!
      do inod = inod_start_filter, inod_end_filter
        call const_fluid_filter_nod_by_nod(inod)
      end do
!
      end subroutine const_fluid_filter_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_simple_filter
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use cal_simple_filter_each_node
      use expand_filter_area_4_1node
!
      integer(kind = kint) :: inod
!
!
      call init_4_cal_fileters
!
      i_exp_level_1nod_weight = maximum_neighbour
      do inod = inod_start_filter, inod_end_filter
        call set_simple_filter_nod_by_nod(inod)
      end do
!
      end subroutine set_simple_filter
!
! -----------------------------------------------------------------------
!
      subroutine set_simple_fluid_filter
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use cal_simple_filter_each_node
      use expand_filter_area_4_1node
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
        call set_simple_fl_filter_nod_by_nod(inod)
      end do
!
      end subroutine set_simple_fluid_filter
!
! -----------------------------------------------------------------------
!
      end module cal_filter_func_node
