!>@file   bcast_ctl_file_const_sph.f90
!!@brief  module bcast_ctl_file_const_sph
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine load_control_4_const_shell(file_name, gen_SPH_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
!!      subroutine write_control_4_const_shell(file_name, gen_SPH_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(sph_mesh_generation_ctl), intent(in) :: gen_SPH_ctl
!!@endverbatim
!
      module bcast_ctl_file_const_sph
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_ctl_data_const_sph_mesh
!
      implicit none
!
!
      private :: bcast_sph_shell_define_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_control_4_const_shell(file_name, gen_SPH_ctl)
!
      character(len=kchara), intent(in) :: file_name
      type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_4_const_shell(file_name, gen_SPH_ctl)
      end if
!
      call bcast_sph_shell_define_ctl(gen_SPH_ctl)
!
      end subroutine load_control_4_const_shell
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine bcast_sph_shell_define_ctl(gen_SPH_ctl)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_4_platform_ctl
      use bcast_4_sphere_ctl
!
      type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
!
!
      call bcast_ctl_data_4_platform(gen_SPH_ctl%plt)
      call bcast_parallel_shell_ctl(gen_SPH_ctl%psph_ctl)
!
      call calypso_mpi_bcast_one_int(gen_SPH_ctl%i_sph_mesh_ctl, 0)
      call calypso_mpi_bcast_character                                  &
     &   (gen_SPH_ctl%fname_psph_ctl, cast_long(kchara), 0)
!
      end subroutine bcast_sph_shell_define_ctl
!
! ----------------------------------------------------------------------
!
      end module bcast_ctl_file_const_sph
