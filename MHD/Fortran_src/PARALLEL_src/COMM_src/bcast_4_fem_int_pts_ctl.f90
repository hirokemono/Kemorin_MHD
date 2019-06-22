!>@file   bcast_4_fem_int_pts_ctl.f90
!!@brief  module bcast_4_fem_int_pts_ctl
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2002
!!@n    Mmodified by H. Matsui in March, 2006
!
!> @brief REad integration points for FEM
!!
!!@verbatim
!!      subroutine bcast_control_fem_int_points(fint_ctl)
!!@endverbatim
!
      module bcast_4_fem_int_pts_ctl
!
      use m_precision
      use t_ctl_data_4_fem_int_pts
!
      implicit  none
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bcast_control_fem_int_points(fint_ctl)
!
      use bcast_control_arrays
!
      type(fem_intergration_control), intent(inout) :: fint_ctl
!
!
      call bcast_ctl_type_i1(fint_ctl%integration_points_ctl)
      call bcast_ctl_type_i1(fint_ctl%intg_point_poisson_ctl)
      call bcast_ctl_type_i1(fint_ctl%intg_point_t_evo_ctl)
!
      call MPI_BCAST(fint_ctl%i_int_points, 1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_control_fem_int_points
!
!   --------------------------------------------------------------------
!
      end module bcast_4_fem_int_pts_ctl
