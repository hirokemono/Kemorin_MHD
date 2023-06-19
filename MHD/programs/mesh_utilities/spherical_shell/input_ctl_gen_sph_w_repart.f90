!>@file   input_ctl_gen_sph_w_repart.f90
!!@brief  module input_ctl_gen_sph_w_repart
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
!!      subroutine s_input_ctl_gen_sph_w_repart(file_name, gen_SPH_wP_c,&
!!     &          sph_files, sph_maker, repart_p)
!!        character(len=kchara), intent(in) :: file_name
!!        type(ctl_data_gen_sph_w_repart), intent(inout) :: gen_SPH_wP_c
!!        type(gen_sph_file_IO_params), intent(inout)  ::  sph_files
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!        type(volume_partioning_param), intent(inout) :: repart_p
!!@endverbatim
!
      module input_ctl_gen_sph_w_repart
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_ctl_data_gen_sph_w_repart
!
      implicit none
!
      private :: bcast_ctl_data_gen_sph_w_repart
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_ctl_gen_sph_w_repart(file_name, gen_SPH_wP_c,  &
     &          sph_files, sph_maker, repart_p)
!
      use t_ctl_params_gen_sph_shell
      use t_sph_grid_maker_in_sim
      use t_control_param_vol_grping
      use m_error_IDs
!
      character(len=kchara), intent(in) :: file_name
      type(ctl_data_gen_sph_w_repart), intent(inout) :: gen_SPH_wP_c
      type(gen_sph_file_IO_params), intent(inout)  ::  sph_files
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(volume_partioning_param), intent(inout) :: repart_p
!
      integer(kind = kint) :: ierr = 0
!
!       load control file
      if(my_rank .eq. 0) then
        call read_ctl_file_gen_sph_w_repart(file_name, gen_SPH_wP_c)
      end if
      call bcast_ctl_data_gen_sph_w_repart(gen_SPH_wP_c)
!
      if(gen_SPH_wP_c%i_sph_mesh_ctl .ne. 1) then
        call calypso_MPI_abort(gen_SPH_wP_c%i_sph_mesh_ctl,             &
     &                             'control file is broken')
      end if
!
!       set control data
      call set_control_4_gen_shell_grids                                &
     &   (my_rank, gen_SPH_wP_c%plt, gen_SPH_wP_c%psph_ctl,             &
     &    sph_files, sph_maker, ierr)
      call set_ctl_param_vol_repart(gen_SPH_wP_c%repart_ctl, repart_p)
      if(ierr .gt. 0) call calypso_mpi_abort(ierr, e_message)
!
      end subroutine s_input_ctl_gen_sph_w_repart
!
! ----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_gen_sph_w_repart(gen_SPH_wP_c)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use bcast_4_platform_ctl
      use bcast_4_sphere_ctl
      use bcast_ctl_data_vol_repart
      use transfer_to_long_integers
!
      type(ctl_data_gen_sph_w_repart), intent(inout) :: gen_SPH_wP_c
!
!
      call bcast_ctl_data_4_platform(gen_SPH_wP_c%plt)
      call bcast_parallel_shell_ctl(gen_SPH_wP_c%psph_ctl)
      call bcast_control_vol_repart(gen_SPH_wP_c%repart_ctl)
!
      call calypso_mpi_bcast_one_int(gen_SPH_wP_c%i_viz_control,  0)
      call calypso_mpi_bcast_one_int(gen_SPH_wP_c%i_sph_mesh_ctl, 0)
!
      call calypso_mpi_bcast_character                                  &
     &   (gen_SPH_wP_c%fname_vol_repart_ctl, cast_long(kchara), 0)
      call calypso_mpi_bcast_character                                  &
     &   (gen_SPH_wP_c%fname_psph_ctl, cast_long(kchara), 0)
!
      end subroutine bcast_ctl_data_gen_sph_w_repart
!
! ----------------------------------------------------------------------
!
      end module input_ctl_gen_sph_w_repart
