!>@file   m_ctl_data_4_fem_int_pts.f90
!!@brief  module m_ctl_data_4_fem_int_pts
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2002
!!@n    Mmodified by H. Matsui in March, 2006
!
!> @brief REad integration points for FEM
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!
!!    begin intg_point_num_ctl
!!       integrate_points_ctl     2
!!
!!       intg_point_poisson_ctl   2
!!       intg_point_t_evo_ctl     2
!!    end intg_point_num_ctl
!!
!! ----------------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_4_fem_int_pts
!
      use m_precision
      use t_control_elements
!
      implicit  none
!
!
!   integeration points
!
!>      Structure for read # of integration points
      type(read_integer_item), save  :: integration_points_ctl
!
!>      Structure for read # of integration points for Poisson eq.
      type(read_integer_item), save :: intg_point_poisson_ctl
!>      Structure for read # of integration points for time integration
      type(read_integer_item), save :: intg_point_t_evo_ctl
!
!
!  label for group entry
!
      character(len=kchara), parameter                                  &
     &      :: hd_int_points = 'intg_point_num_ctl'
      integer (kind=kint) :: i_int_points = 0
!
!    4th level for integeration
!
      character(len=kchara), parameter :: hd_intgration_points          &
     &         = 'integration_points_ctl'
      character(len=kchara), parameter :: hd_intg_point_poisson         &
     &         = 'intg_point_poisson_ctl'
      character(len=kchara), parameter :: hd_intg_point_t_evo           &
     &         = 'intg_point_t_evo_ctl'
!
!
      private :: hd_int_points, i_int_points, hd_intgration_points
      private :: hd_intg_point_poisson, hd_intg_point_t_evo
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_fem_int_points_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_int_points) .eq. 0) return
      if (i_int_points .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_int_points, i_int_points)
        if(i_int_points .gt. 0) exit
!
        call read_integer_ctl_type                                      &
     &     (hd_intgration_points, integration_points_ctl)
        call read_integer_ctl_type                                      &
     &     (hd_intg_point_poisson, intg_point_poisson_ctl)
        call read_integer_ctl_type                                      &
     &     (hd_intg_point_t_evo, intg_point_t_evo_ctl)
      end do
!
      end subroutine read_fem_int_points_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_4_fem_int_pts
