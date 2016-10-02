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
!!      subroutine mpi_gen_sph_rlm_grids                                &
!!     &         (sph_params, sph_rlm, comm_rlm_mul)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(nprocs)
!!      subroutine mpi_gen_sph_rtm_grids                                &
!!     &         (sph_params, sph_rtm, comm_rtm_mul)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(nprocs)
!!
!!      subroutine mpi_gen_sph_rj_modes                                 &
!!     &         (comm_rlm_mul, sph_params, sph_rlm, sph_rj)
!!        type(sph_comm_tbl), intent(in) :: comm_rlm_mul(nprocs)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!       type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!      subroutine mpi_gen_sph_rtp_grids                                &
!!     &         (comm_rtm_mul, sph_params, sph_rtp, sph_rtm)
!!        type(sph_comm_tbl), intent(in) :: comm_rtm_mul(nprocs)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!
!!      subroutine mpi_gen_fem_mesh_for_sph                             &
!!     &         (sph_params, sph_rj, sph_rtp)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
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
      subroutine mpi_gen_sph_rlm_grids                                  &
     &         (sph_params, sph_rlm, comm_rlm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_MPI_IO_select
!
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
      call const_sph_rlm_modes(my_rank, sph_rlm, comm_rlm_lc)
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
      subroutine mpi_gen_sph_rtm_grids                                  &
     &         (sph_params, sph_rtm, comm_rtm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_MPI_IO_select
!
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
      call const_sph_rtm_grids(my_rank, sph_rtm, comm_rtm_lc)
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
      subroutine mpi_gen_sph_rj_modes                                   &
     &         (comm_rlm_mul, sph_params, sph_rlm, sph_rj)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
      use sph_file_MPI_IO_select
!
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(nprocs)
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
!
      call allocate_rj_1d_local_idx(sph_rj)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical modes for domain ', my_rank
      call const_sph_rj_modes(my_rank, nprocs,                          &
     &    comm_rlm_mul, sph_params, sph_rj, sph_rlm, sph_file_m)
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
      subroutine mpi_gen_sph_rtp_grids                                  &
     &         (comm_rtm_mul, sph_params, sph_rtp, sph_rtm)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
      use sph_file_MPI_IO_select
!
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(nprocs)
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
!
      call allocate_rtp_1d_local_idx(sph_rtp)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical grids for domain ',  my_rank
      call const_sph_rtp_grids(my_rank, nprocs, comm_rtm_mul,           &
     &    sph_params, sph_rtp, sph_rtm, sph_file_m)
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
     &         (sph_params, sph_rj, sph_rtp)
!
      use t_spheric_parameter
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
!
      use m_gauss_points
      use m_sph_mesh_1d_connect
      use m_read_mesh_data
      use m_node_id_spherical_IO
      use m_read_mesh_data
      use const_1d_ele_connect_4_sph
      use set_local_index_table_sph
      use set_local_sphere_by_global
      use set_sph_groups
      use gen_sph_grids_modes
      use set_FEM_mesh_4_sph
      use mpi_load_mesh_data
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      type(mesh_data) :: femmesh
      type(group_data) :: radial_rj_grp_lc
!
!
      if(iflag_output_mesh .eq. 0) return
!
      call allocate_gauss_points(sph_rtp%nidx_global_rtp(2))
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
      call s_const_1d_ele_connect_4_sph                                 &
     &   (sph_params%iflag_shell_mode, sph_params%m_folding, sph_rtp)
      call set_rj_radial_grp(sph_params, sph_rj, radial_rj_grp_lc)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct FEM mesh for domain ', my_rank
!
      call copy_gl_2_local_rtp_param(my_rank, sph_rtp)
      call s_const_FEM_mesh_for_sph                                     &
     &   (my_rank, sph_rtp%nidx_rtp, radius_1d_gl,                      &
     &    sph_params, sph_rtp, radial_rj_grp_lc,                        &
     &    femmesh%mesh, femmesh%group)
!
! Output mesh data
      if(iflag_output_mesh .gt. 0) then
        mesh_file_head = sph_file_head
        call mpi_output_mesh(femmesh%mesh, femmesh%group)
        write(*,'(a,i6,a)')                                             &
     &          'FEM mesh for domain', my_rank, ' is done.'
      end if
!
      call dealloc_groups_data(femmesh%group)
      call dealloc_mesh_type(femmesh%mesh)
      call deallocate_grp_type(radial_rj_grp_lc)
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine mpi_gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
!
      end module mpi_gen_sph_grids_modes
