!>@file   t_check_and_make_SPH_mesh.f90
!!@brief  module t_check_and_make_SPH_mesh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine check_and_make_SPH_mesh                              &
!!     &         (iflag_make_SPH, sph_file_param, sph_maker)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!      subroutine mpi_gen_sph_grids(sph_file_param, gen_sph,           &
!!     &                             sph, comms_sph, sph_grp)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) :: sph_grp
!!@endverbatim
!
      module t_check_and_make_SPH_mesh
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_const_spherical_grid
      use t_file_IO_parameter
!
      implicit none
!
!>      Structure to check and construct spherical shell mesh
      type sph_grid_maker_in_sim
!>        Structure to construct grid
        type(construct_spherical_grid) :: gen_sph
!>        Structure for temporal spherical grid
        type(sph_grids) :: sph_tmp
      end type sph_grid_maker_in_sim
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine check_and_make_SPH_mesh                                &
     &         (iflag_make_SPH, sph_file_param, sph_maker)
!
      use m_error_IDs
      use calypso_mpi_logical
      use output_gen_sph_grid_modes
      use mpi_gen_sph_grids_modes
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: iflag_make_SPH
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
      type(sph_comm_tables) :: comms_sph_tmp
      type(sph_group_data) :: sph_grp_tmp
      logical :: iflag_lc
!
!
      if(my_rank .eq. izero) then
        iflag_lc =    check_exsist_rtp_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rtm_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rlm_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rj_file(my_rank, sph_file_param)
      end if
      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
      else if(iflag_make_SPH .eq. 0) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        call mpi_gen_sph_grids(sph_maker%gen_sph,                       &
     &      sph_maker%sph_tmp, comms_sph_tmp, sph_grp_tmp)
        call mpi_output_gen_sph_grids(sph_file_param,                   &
     &      sph_maker%sph_tmp, comms_sph_tmp, sph_grp_tmp)
      end if
      call calypso_mpi_barrier
!
      end subroutine check_and_make_SPH_mesh
!
! ----------------------------------------------------------------------
!
      subroutine check_and_make_SPH_rj_mode                             &
     &         (iflag_make_SPH, sph_file_param, sph_maker)
!
      use m_error_IDs
      use calypso_mpi_logical
      use mpi_gen_sph_grids_modes
      use output_gen_sph_grid_modes
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: iflag_make_SPH
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
      type(sph_comm_tables) :: comms_sph_tmp
      type(sph_group_data) :: sph_grp_tmp
      logical :: iflag_lc
!
!
      if(my_rank .eq. izero) then
        iflag_lc = check_exsist_rj_file(my_rank, sph_file_param)
      end if
      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
      else if(iflag_make_SPH .eq. 0) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        call mpi_gen_sph_rj_mode(sph_maker%gen_sph,                     &
     &      sph_maker%sph_tmp, comms_sph_tmp, sph_grp_tmp)
        call mpi_output_gen_sph_rj_mode(sph_file_param,                 &
     &      sph_maker%sph_tmp, comms_sph_tmp, sph_grp_tmp)
      end if
      call calypso_mpi_barrier
!
      end subroutine check_and_make_SPH_rj_mode
!
! ----------------------------------------------------------------------
!
      end module t_check_and_make_SPH_mesh
