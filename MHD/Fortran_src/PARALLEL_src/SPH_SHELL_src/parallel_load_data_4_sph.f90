!>@file   parallel_load_data_4_sph.f90
!!@brief  module parallel_load_data_4_sph
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief Load spherical harmonics indexing data on multiple processes
!!
!!@verbatim
!!      subroutine load_para_SPH_and_FEM_mesh(sph, comms_sph, sph_grps, &
!!     &         mesh, group, ele_mesh)
!!      subroutine load_para_SPH_rj_mesh(sph, comms_sph, sph_grps)
!!      subroutine load_para_sph_mesh(sph, bc_rtp_grp, sph_grps)
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!
!!      subroutine load_para_rj_mesh(sph_param, sph_rj, comm_rj,        &
!!     &          radial_rj_grp, sphere_rj_grp)
!!         type(sph_shell_parameters), intent(inout) :: sph_param
!!         type(sph_rtp_grid), intent(inout) :: sph_rtp
!!         type(sph_rtm_grid), intent(inout) :: sph_rtm
!!         type(sph_rlm_grid), intent(inout) :: sph_rlm
!!         type(sph_rj_grid), intent(inout) :: sph_rj
!!         type(sph_comm_tbl), intent(inout) :: comm_rj
!!         type(group_data), intent(inout) :: radial_rj_grp
!!         type(group_data), intent(inout) :: sphere_rj_grp
!!@endverbatim
!
      module parallel_load_data_4_sph
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_mesh
      use sph_file_MPI_IO_select
      use set_loaded_data_4_sph
!
      implicit none
!
      private :: load_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_para_SPH_and_FEM_mesh(sph, comms_sph, sph_grps,   &
     &         mesh, group, ele_mesh)
!
      use t_mesh_data
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      call load_para_sph_mesh(sph, comms_sph, sph_grps)
!
      call load_FEM_mesh_4_SPH                                          &
     &   (sph%sph_params, sph%sph_rtp, sph%sph_rj,                      &
     &    sph_grps%radial_rtp_grp, sph_grps%radial_rj_grp,              &
     &    mesh, group, ele_mesh)
!
      end subroutine load_para_SPH_and_FEM_mesh
!
! -----------------------------------------------------------------------
!
      subroutine load_para_SPH_rj_mesh(sph, comms_sph, sph_grps)
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
!
      call load_para_rj_mesh                                            &
     &   (sph%sph_params, sph%sph_rj, comms_sph%comm_rj,                &
     &    sph_grps%radial_rj_grp, sph_grps%sphere_rj_grp)
!
      end subroutine load_para_SPH_rj_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_FEM_mesh_4_SPH(sph_param, sph_rtp, sph_rj,        &
     &          radial_rtp_grp, radial_rj_grp, mesh, group, ele_mesh)
!
      use calypso_mpi
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
!
      use m_spheric_constants
      use mpi_load_mesh_data
      use copy_mesh_structures
      use const_FEM_mesh_sph_mhd
      use gen_sph_grids_modes
      use mesh_IO_select
!
      type(sph_shell_parameters), intent(inout) :: sph_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rtp_grp
      type(group_data), intent(in) :: radial_rj_grp
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
      type(mesh_data) :: femmesh_s
!
!
!  --  load FEM mesh data
      if(check_exist_mesh(my_rank) .eq. 0) then
        if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
        call mpi_input_mesh(mesh, group,                                &
     &      ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
        call allocate_ele_geometry_type(mesh%ele)
        call set_fem_center_mode_4_SPH                                  &
     &     (mesh%node%internal_node, sph_rtp, sph_param)
        return
      end if
!
!  --  Construct FEM mesh
      if(sph_param%iflag_shell_mode .eq. iflag_no_FEMMESH) then
        if(sph_rj%iflag_rj_center .gt. 0) then
          sph_param%iflag_shell_mode =  iflag_MESH_w_center
        else
          sph_param%iflag_shell_mode = iflag_MESH_same
        end if
      end if
!
      if (iflag_debug.gt.0) write(*,*) 'const_FEM_mesh_4_sph_mhd'
      call const_FEM_mesh_4_sph_mhd                                     &
     &   (sph_param, sph_rtp, sph_rj, radial_rtp_grp,                   &
     &    radial_rj_grp, femmesh_s%mesh, femmesh_s%group)
!      call compare_mesh_type                                           &
!     &   (my_rank, mesh%nod_comm, mesh%node, mesh%ele, femmesh_s%mesh)
!      call compare_mesh_groups(group%nod_grp, femmesh_s%group)
!
      call set_mesh_data_from_type(femmesh_s%mesh, femmesh_s%group,     &
     &      mesh%nod_comm, mesh%node, mesh%ele,                         &
     &      ele_mesh%surf, ele_mesh%edge,                               &
     &      group%nod_grp, group%ele_grp, group%surf_grp)
!
      end subroutine load_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine load_para_sph_mesh(sph, comms_sph, sph_grps)
!
      use calypso_mpi
      use m_machine_parameter
!
      use load_data_for_sph_IO
      use set_from_recv_buf_rev
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtp_sph_trans'
      call sel_mpi_read_geom_rtp_file
      call input_geom_rtp_sph_trans                                     &
     &   (sph%sph_params%l_truncation, sph%sph_rtp, comms_sph%comm_rtp, &
     &    sph_grps%bc_rtp_grp, sph_grps%radial_rtp_grp,                 &
     &    sph_grps%theta_rtp_grp, sph_grps%zonal_rtp_grp, ierr)
      call set_reverse_import_table(sph%sph_rtp%nnod_rtp,               &
     &    comms_sph%comm_rtp%ntot_item_sr, comms_sph%comm_rtp%item_sr,  &
     &    comms_sph%comm_rtp%irev_sr)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call sel_mpi_read_spectr_rj_file
      call input_modes_rj_sph_trans                                     &
     &   (sph%sph_params%l_truncation, sph%sph_rj, comms_sph%comm_rj,   &
     &    sph_grps%radial_rj_grp, sph_grps%sphere_rj_grp, ierr)
      call set_reverse_import_table(sph%sph_rj%nnod_rj,                 &
     &    comms_sph%comm_rj%ntot_item_sr, comms_sph%comm_rj%item_sr,    &
     &    comms_sph%comm_rj%irev_sr)
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtm_sph_trans'
      call sel_mpi_read_geom_rtm_file
      call input_geom_rtm_sph_trans(sph%sph_params%l_truncation,        &
     &    sph%sph_rtm, comms_sph%comm_rtm, ierr)
      call set_reverse_import_table(sph%sph_rtm%nnod_rtm,               &
     &    comms_sph%comm_rtm%ntot_item_sr, comms_sph%comm_rtm%item_sr,  &
     &    comms_sph%comm_rtm%irev_sr)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rlm_sph_trans'
      call sel_mpi_read_modes_rlm_file
      call input_modes_rlm_sph_trans(sph%sph_params%l_truncation,       &
     &    sph%sph_rlm, comms_sph%comm_rlm, ierr)
      call set_reverse_import_table(sph%sph_rlm%nnod_rlm,               &
     &    comms_sph%comm_rlm%ntot_item_sr, comms_sph%comm_rlm%item_sr,  &
     &    comms_sph%comm_rlm%irev_sr)
!
      if (iflag_debug.gt.0) write(*,*) 'set_index_flags_4_SPH'
      call set_index_flags_4_SPH(sph%sph_params,                        &
     &    sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj,            &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm,                       &
     &    comms_sph%comm_rlm, comms_sph%comm_rj)
!
      end subroutine load_para_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine load_para_rj_mesh(sph_param, sph_rj, comm_rj,          &
     &          radial_rj_grp, sphere_rj_grp)
!
      use calypso_mpi
      use m_machine_parameter
!
      use load_data_for_sph_IO
      use set_special_sph_lm_flags
!
      use set_from_recv_buf_rev
!
      type(sph_shell_parameters), intent(inout) :: sph_param
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj
      type(group_data), intent(inout) :: radial_rj_grp
      type(group_data), intent(inout) :: sphere_rj_grp
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call sel_mpi_read_spectr_rj_file
      call input_modes_rj_sph_trans(sph_param%l_truncation,             &
     &    sph_rj, comm_rj, radial_rj_grp, sphere_rj_grp, ierr)
      call set_reverse_import_table(sph_rj%nnod_rj,                     &
     &    comm_rj%ntot_item_sr, comm_rj%item_sr, comm_rj%irev_sr)
!
      call set_index_flags_4_rj(sph_param, sph_rj, comm_rj)
!
      end subroutine load_para_rj_mesh
!
! -----------------------------------------------------------------------
!
      end module parallel_load_data_4_sph
