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
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(fem_intergration_control), intent(inout) :: fint_ctl
!
!
      call bcast_ctl_type_i1(fint_ctl%integration_points_ctl)
      call bcast_ctl_type_i1(fint_ctl%intg_point_poisson_ctl)
      call bcast_ctl_type_i1(fint_ctl%intg_point_t_evo_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (fint_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(fint_ctl%i_int_points, 0)
!
      end subroutine bcast_control_fem_int_points
!
!   --------------------------------------------------------------------
!
      end module bcast_4_fem_int_pts_ctl
