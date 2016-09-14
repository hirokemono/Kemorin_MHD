!>@file   para_gen_sph_grids_modes.f90
!!@brief  module para_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Parallellized version)
!!
!!
!!@verbatim
!!      subroutine para_gen_sph_rlm_grids(ndomain_sph, sph_params,      &
!!     &          sph_rlm, comm_rlm_mul)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(ndomain_sph)
!!      subroutine para_gen_sph_rtm_grids(ndomain_sph, sph_params,      &
!!     &          sph_rtm, comm_rtm_mul)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(ndomain_sph)
!!
!!      subroutine para_gen_sph_rj_modes(ndomain_sph, comm_rlm_mul,     &
!!     &          sph_params, sph_rlm, sph_rj)
!!        type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!      subroutine para_gen_sph_rtp_grids(ndomain_sph, comm_rtm_mul,    &
!!     &          sph_params, sph_rtp, sph_rtm)
!!        type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!
!!      subroutine para_gen_fem_mesh_for_sph(ndomain_sph,               &
!!     &          sph_params, sph_rj, sph_rtp)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!@endverbatim
!
      module para_gen_sph_grids_modes
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
      type(sph_file_data_type), save, private :: sph_file_p
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_rlm_grids(ndomain_sph, sph_params,        &
     &          sph_rlm, comm_rlm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(ndomain_sph)
!
      type(sph_comm_tbl) :: comm_rlm_lc
      integer(kind = kint) :: ip_rank, ip
!
!
      do ip = 0, (ndomain_sph-1) / nprocs
        ip_rank = my_rank + ip * nprocs
        if(ip_rank .ge. ndomain_sph) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'start rlm table generation for',                    &
     &            ip_rank, 'on ', my_rank, nprocs
        call const_sph_rlm_modes(ip_rank, sph_rlm, comm_rlm_lc)
        if(iflag_debug .gt. 0) write(*,*) 'copy_sph_comm_neib'
        call copy_sph_comm_neib(comm_rlm_lc, comm_rlm_mul(ip_rank+1))
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &        'output_modes_rlm_sph_trans', ip_rank
        call output_modes_rlm_sph_trans                                 &
     &     (sph_params, sph_rlm, comm_rlm_lc, sph_file_p)
!
        call sel_write_modes_rlm_file(ip_rank, sph_file_p)
        write(*,'(a,i6,a,i6)') 'Spherical transform table for domain',  &
     &          ip_rank, ' is done on process ', my_rank
      end do
!
      end subroutine para_gen_sph_rlm_grids
!
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtm_grids(ndomain_sph, sph_params,        &
     &          sph_rtm, comm_rtm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(ndomain_sph)
!
      type(sph_comm_tbl) :: comm_rtm_lc
      integer(kind = kint) :: ip_rank, ip
!
!
      do ip = 0, (ndomain_sph-1) / nprocs
        ip_rank = my_rank + ip * nprocs
        if(ip_rank .ge. ndomain_sph) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'start rtm table generation for',                    &
     &            ip_rank, 'on ', my_rank, nprocs
        call const_sph_rtm_grids(ip_rank, sph_rtm, comm_rtm_lc)
        call copy_sph_comm_neib(comm_rtm_lc, comm_rtm_mul(ip_rank+1))
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &        'output_geom_rtm_sph_trans', ip_rank
        call output_geom_rtm_sph_trans                                  &
     &     (sph_params, sph_rtm, comm_rtm_lc, sph_file_p)
!
        call sel_write_geom_rtm_file(ip_rank, sph_file_p)
        write(*,'(a,i6,a,i6)') 'Legendre transform table rtm',          &
     &          ip_rank, ' is done on process ', my_rank
      end do
!
      end subroutine para_gen_sph_rtm_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rj_modes(ndomain_sph, comm_rlm_mul,       &
     &          sph_params, sph_rlm, sph_rj)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      integer(kind = kint) :: ip_rank, ip
!
!
      call allocate_rj_1d_local_idx(sph_rj)
      do ip = 0, (ndomain_sph-1) / nprocs
        ip_rank = my_rank + ip * nprocs
        if(ip_rank .ge. ndomain_sph) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical modes for domain ',             &
     &            ip_rank,  ' on ', my_rank
        call const_sph_rj_modes(ip_rank, ndomain_sph,                   &
     &      comm_rlm_mul, sph_params, sph_rj, sph_rlm, sph_file_p)
!
        call sel_write_spectr_modes_rj_file(ip_rank, sph_file_p)
        write(*,'(a,i6,a,i6)') 'Spherical modes for domain',            &
     &          ip_rank, ' is done on process ', my_rank
      end do
      call deallocate_rj_1d_local_idx
!
      end subroutine para_gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtp_grids(ndomain_sph, comm_rtm_mul,      &
     &          sph_params, sph_rtp, sph_rtm)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
      integer(kind = kint) :: ip_rank, ip
!
!
      call allocate_rtp_1d_local_idx(sph_rtp)
      do ip = 0, (ndomain_sph-1) / nprocs
        ip_rank = my_rank + ip * nprocs
        if(ip_rank .ge. ndomain_sph) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical grids for domain ',             &
     &            ip_rank,  ' on ', my_rank
        call const_sph_rtp_grids(ip_rank, ndomain_sph, comm_rtm_mul,    &
     &      sph_params, sph_rtp, sph_rtm, sph_file_p)
!
        call sel_write_geom_rtp_file(ip_rank, sph_file_p)
        write(*,'(a,i6,a,i6)') 'Spherical grids for domain',            &
     &          ip_rank, ' is done on process ', my_rank
      end do
      call deallocate_rtp_1d_local_idx
!
      end subroutine para_gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_fem_mesh_for_sph(ndomain_sph,                 &
     &          sph_params, sph_rj, sph_rtp)
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
      use load_mesh_data
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      integer(kind = kint) :: ip_rank, ip
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
      do ip = 0, (ndomain_sph-1) / nprocs
        ip_rank = my_rank + ip * nprocs
        if(ip_rank .ge. ndomain_sph) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct FEM mesh for domain ', ip_rank,           &
     &             ' on ', my_rank
!
        call copy_gl_2_local_rtp_param(ip_rank, sph_rtp)
!
!
        call s_const_FEM_mesh_for_sph                                   &
     &     (ip_rank, sph_rtp%nidx_rtp, radius_1d_gl,                    &
     &      sph_params, sph_rtp, radial_rj_grp_lc,                      &
     &      femmesh%mesh, femmesh%group)
!
! Output mesh data
        if(iflag_output_mesh .gt. 0) then
          mesh_file_head = sph_file_head
          call output_mesh(ip_rank, femmesh%mesh, femmesh%group)
          write(*,'(a,i6,a,i6)')                                        &
     &          ip_rank, ' is done on process ', my_rank
        end if
      end do
!
      call dealloc_mesh_data_type(femmesh)
      call deallocate_grp_type(radial_rj_grp_lc)
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine para_gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
!
      end module para_gen_sph_grids_modes
