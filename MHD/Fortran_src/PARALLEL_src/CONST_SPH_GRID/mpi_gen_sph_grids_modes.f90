!>@file   mpi_gen_sph_grids_modes.f90
!!@brief  module mpi_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Parallellized version)
!!
!!
!!@verbatim
!!      subroutine mpi_gen_sph_rlm_grids(stk_lc1d, sph_gl1d, stbl,      &
!!     &          sph_params, sph_rlm, comm_rlm_mul)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(nprocs)
!!      subroutine mpi_gen_sph_rtm_grids(stk_lc1d, sph_gl1d, stbl,      &
!!     &          sph_params, sph_rtm, comm_rtm_mul)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(nprocs)
!!
!!      subroutine mpi_gen_sph_rj_modes(comm_rlm_mul,                   &
!!     &          added_radial_grp, stk_lc1d, sph_gl1d, stbl,           &
!!     &          sph_params, sph_rlm, sph_rj)
!!        type(sph_comm_tbl), intent(in) :: comm_rlm_mul(nprocs)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!       type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!      subroutine mpi_gen_sph_rtp_grids(comm_rtm_mul,                  &
!!     &          added_radial_grp, r_layer_grp, med_layer_grp,         &
!!     &          stk_lc1d, sph_gl1d, stbl,                             &
!!     &          sph_params, sph_rtp, sph_rtm)
!!        type(sph_comm_tbl), intent(in) :: comm_rtm_mul(nprocs)
!!        type(comm_table_make_sph), intent(in) :: stbl
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!
!!      subroutine mpi_gen_fem_mesh_for_sph                             &
!!     &         (added_radial_grp, stk_lc1d, sph_gl1d,                 &
!!     &          sph_params, sph_rj, sph_rtp, mesh_file, stbl)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(comm_table_make_sph), intent(inout) :: stbl
!!@endverbatim
!
      module mpi_gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
!
      use m_work_time
      use calypso_mpi
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_group_data
      use t_spheric_mesh
      use t_spheric_data_IO
      use t_sph_1d_global_index
      use t_sph_mesh_1d_connect
      use t_control_1D_layering
!
      use set_local_sphere_by_global
!
      implicit none
!
      type(sph_file_data_type), save, private :: sph_file_m
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rlm_grids(stk_lc1d, sph_gl1d, stbl,        &
     &          sph_params, sph_rlm, comm_rlm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_MPI_IO_select
!
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(comm_table_make_sph), intent(in) :: stbl
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(nprocs)
!
      type(sph_comm_tbl) :: comm_rlm_lc
      integer(kind = kint) :: ip
!
!
      ip = my_rank + 1
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'start rlm table generation for', my_rank
      call const_sph_rlm_modes                                          &
     &   (my_rank, stk_lc1d, sph_gl1d, stbl, sph_rlm, comm_rlm_lc)
      call copy_sph_comm_neib(comm_rlm_lc, comm_rlm_mul(ip))
!
      call output_modes_rlm_sph_trans                                   &
     &   (sph_params, sph_rlm, comm_rlm_lc, sph_file_m)
!
      call sel_mpi_write_modes_rlm_file(nprocs, my_rank, sph_file_m)
      write(*,'(a,i6,a)') 'Spherical transform table for domain',       &
     &          my_rank, ' is done.'
!
      end subroutine mpi_gen_sph_rlm_grids
!
! -----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rtm_grids(stk_lc1d, sph_gl1d, stbl,        &
     &          sph_params, sph_rtm, comm_rtm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_MPI_IO_select
!
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(comm_table_make_sph), intent(in) :: stbl
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(nprocs)
!
      type(sph_comm_tbl) :: comm_rtm_lc
      integer(kind = kint) :: ip
!
!
!
      ip = my_rank + 1
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'start rtm table generation for',  my_rank
      call const_sph_rtm_grids                                          &
     &   (my_rank, stk_lc1d, sph_gl1d, stbl, sph_rtm, comm_rtm_lc)
      call copy_sph_comm_neib(comm_rtm_lc, comm_rtm_mul(ip))
!
      call output_geom_rtm_sph_trans                                    &
     &   (sph_params, sph_rtm, comm_rtm_lc, sph_file_m)
!
      call sel_mpi_write_geom_rtm_file(nprocs, my_rank, sph_file_m)
      write(*,'(a,i6,a)') 'Legendre transform table rtm',               &
     &          my_rank, ' is done.'
!
      end subroutine mpi_gen_sph_rtm_grids
!
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rj_modes(comm_rlm_mul,                     &
     &          added_radial_grp, stk_lc1d, sph_gl1d, stbl,             &
     &          sph_params, sph_rlm, sph_rj)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
      use sph_file_MPI_IO_select
!
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(nprocs)
      type(layering_group_list), intent(in) :: added_radial_grp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(comm_table_make_sph), intent(in) :: stbl
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
!
      call allocate_rj_1d_local_idx(sph_rj)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical modes for domain ', my_rank
      call const_sph_rj_modes(my_rank, nprocs, comm_rlm_mul,            &
     &    added_radial_grp, stk_lc1d, sph_gl1d, stbl,                   &
     &    sph_params, sph_rj, sph_rlm, sph_file_m)
!
      call sel_mpi_write_spectr_rj_file(nprocs, my_rank, sph_file_m)
      write(*,'(a,i6,a)') 'Spherical modes for domain',                 &
     &          my_rank, ' is done.'
      call deallocate_rj_1d_local_idx
!
      end subroutine mpi_gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rtp_grids(comm_rtm_mul,                    &
     &          added_radial_grp, r_layer_grp, med_layer_grp,           &
     &          stk_lc1d, sph_gl1d, stbl,                               &
     &          sph_params, sph_rtp, sph_rtm)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
      use sph_file_MPI_IO_select
!
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(nprocs)
      type(layering_group_list), intent(in) :: added_radial_grp
      type(layering_group_list), intent(in) :: r_layer_grp
      type(layering_group_list), intent(in) :: med_layer_grp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(comm_table_make_sph), intent(in) :: stbl
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
!
      call allocate_rtp_1d_local_idx(sph_rtp)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical grids for domain ',  my_rank
      call const_sph_rtp_grids(my_rank, nprocs, comm_rtm_mul,           &
     &    added_radial_grp, r_layer_grp, med_layer_grp,                 &
     &    stk_lc1d, sph_gl1d, stbl, sph_params, sph_rtp, sph_rtm,       &
     &    sph_file_m)
!
      call sel_mpi_write_geom_rtp_file(nprocs, my_rank, sph_file_m)
      write(*,'(a,i6,a)') 'Spherical grids for domain',                 &
     &          my_rank, ' is done.'
      call deallocate_rtp_1d_local_idx
!
      end subroutine mpi_gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_fem_mesh_for_sph                               &
     &         (added_radial_grp, stk_lc1d, sph_gl1d,                   &
     &          sph_params, sph_rj, sph_rtp, mesh_file, stbl)
!
      use t_file_IO_parameter
      use t_spheric_parameter
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_gauss_points
!
      use const_1d_ele_connect_4_sph
      use set_local_index_table_sph
      use set_local_sphere_by_global
      use set_sph_groups
      use gen_sph_grids_modes
      use set_FEM_mesh_4_sph
      use mpi_load_mesh_data
      use sph_file_IO_select
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(layering_group_list), intent(in) :: added_radial_grp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(field_IO_params), intent(inout) ::  mesh_file
      type(comm_table_make_sph), intent(inout) :: stbl
!
      type(mesh_data) :: femmesh
      type(group_data) :: radial_rj_grp_lc
      type(gauss_points) :: gauss_s
!
!
      if(iflag_output_mesh .eq. 0) return
!
      call const_gauss_colatitude(sph_rtp%nidx_global_rtp(2), gauss_s)
!
      call s_const_1d_ele_connect_4_sph                                 &
     &   (sph_params%iflag_shell_mode, sph_params%m_folding, sph_rtp,   &
     &    stk_lc1d, sph_gl1d, stbl)
      call set_rj_radial_grp(sph_params, sph_rj,                        &
     &    added_radial_grp, radial_rj_grp_lc)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct FEM mesh for domain ', my_rank
!
      call copy_gl_2_local_rtp_param(my_rank, stk_lc1d, sph_rtp)
      call s_const_FEM_mesh_for_sph                                     &
     &   (my_rank, sph_rtp%nidx_rtp, stbl%radius_1d_gl, gauss_s,        &
     &    stk_lc1d, sph_gl1d, sph_params, sph_rtp, radial_rj_grp_lc,    &
     &    femmesh%mesh, femmesh%group, stbl)
!
! Output mesh data
      if(iflag_output_mesh .gt. 0) then
        mesh_file%file_prefix = sph_file_head
        call mpi_output_mesh(mesh_file, femmesh%mesh, femmesh%group)
        write(*,'(a,i6,a)')                                             &
     &          'FEM mesh for domain', my_rank, ' is done.'
      end if
!
      call dealloc_groups_data(femmesh%group)
      call dealloc_mesh_type(femmesh%mesh)
      call deallocate_grp_type(radial_rj_grp_lc)
      call dealloc_gauss_colatitude(gauss_s)
!
      end subroutine mpi_gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
!
      end module mpi_gen_sph_grids_modes
