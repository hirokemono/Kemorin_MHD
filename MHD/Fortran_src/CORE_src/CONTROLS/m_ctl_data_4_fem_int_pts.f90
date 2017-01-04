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
      use t_ctl_data_4_fem_int_pts
!
      implicit  none
!
!
!   integeration points
!
      type(fem_intergration_control), save  :: fint_ctl1
!
!  label for group entry
!
      character(len=kchara), parameter                                  &
     &      :: hd_int_points = 'intg_point_num_ctl'
      integer (kind=kint) :: i_int_points = 0
!
      private :: hd_int_points, i_int_points
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_fem_int_points_ctl
!
!
      call read_control_fem_int_points                                  &
     &   (hd_int_points, i_int_points, fint_ctl1)
!
      end subroutine read_fem_int_points_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_4_fem_int_pts
