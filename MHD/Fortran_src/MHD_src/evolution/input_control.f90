!>@file   input_control.f90
!!@brief  module input_control
!!
!!@author H.Matsui and H.Okuda
!!@date     Programmed by H.Matsui and H.Okuda on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_4_MHD(FEM_prm, SGS_par, MHD_step,      &
!!     &          mesh, group, ele_mesh, nod_fld, IO_bc,                &
!!     &          filtering, wide_filtering, wk_filter, MHD_matrices)
!!      subroutine input_control_4_snapshot(FEM_prm, SGS_par, MHD_step, &
!!     &          mesh, group, ele_mesh, nod_fld, IO_bc,                &
!!     &          filtering, wide_filtering, wk_filter)
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(IO_boundary), intent(inout) :: IO_bc
!!        type(filtering_data_type), intent(inout) :: filtering
!!        type(filtering_data_type), intent(inout) :: wide_filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(MHD_MG_matrices), intent(inout) :: MHD_matrices
!!@endverbatim
!
!
      module input_control
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_MHD_step_parameter
      use t_mesh_data
      use t_boundary_field_IO
      use t_filtering_data
      use t_solver_djds_MHD
      use t_phys_data
      use t_file_IO_parameter
      use t_field_data_IO
      use t_ctl_data_MHD
!
      implicit none
!
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: FEM_MHD_ctl
!>      Structure for mesh file IO paramters
      type(field_IO_params), save ::  mesh1_file
!>      Structure for field data IO paramters
      type(field_IO_params), save :: FEM_udt_org_param
!
      private :: FEM_MHD_ctl
      private :: mesh1_file
      private :: input_meshes_4_MHD, boundary_file_IO_control
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_MHD(FEM_prm, SGS_par, MHD_step,        &
     &          mesh, group, ele_mesh, nod_fld, IO_bc,                  &
     &          filtering, wide_filtering, wk_filter, MHD_matrices)
!
      use t_ctl_data_sph_MHD_psf
      use m_flags_4_solvers
      use set_control_FEM_MHD
      use mpi_load_mesh_data
      use input_MG_data
      use skip_comment_f
      use ordering_field_by_viz
      use node_monitor_IO
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(MHD_step_param), intent(inout) :: MHD_step
      type(phys_data), intent(inout) :: nod_fld
!
      type(IO_boundary), intent(inout) :: IO_bc
      type(filtering_data_type), intent(inout) :: filtering
      type(filtering_data_type), intent(inout) :: wide_filtering
      type(filtering_work_type), intent(inout) :: wk_filter
      type(MHD_MG_matrices), intent(inout) :: MHD_matrices
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_fem_MHD'
      call read_control_4_fem_MHD(MHD_ctl_name, FEM_MHD_ctl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_FEM_MHD'
      call set_control_4_FEM_MHD                                        &
     &   (FEM_MHD_ctl%plt, FEM_MHD_ctl%org_plt, FEM_MHD_ctl%model_ctl,  &
     &    FEM_MHD_ctl%ctl_ctl, FEM_MHD_ctl%nmtr_ctl, mesh1_file,        &
     &    FEM_udt_org_param, FEM_prm, SGS_par, MHD_step, nod_fld)
!
!  --  load FEM mesh data
      call mpi_input_mesh(mesh1_file, mesh, group,                      &
     &    ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
!
      call input_meshes_4_MHD(SGS_par%model_p, mesh, group, IO_bc,      &
     &    SGS_par%filter_p, filtering, wide_filtering, wk_filter)
!
      if(cmp_no_case(FEM_PRM%CG11_param%METHOD, cflag_mgcg)) then
        call alloc_MHD_MG_DJDS_mat(MGCG_WK1%num_MG_level, MHD_matrices)
        call input_MG_mesh(FEM_prm%MG_file, mesh1_file)
        call input_MG_itp_tables                                        &
     &     (FEM_prm%MG_file, MHD_matrices%MG_interpolate)
      else
        MGCG_WK1%num_MG_level = 0
        call alloc_MHD_MG_DJDS_mat(MGCG_WK1%num_MG_level, MHD_matrices)
      end if
!
      call count_field_4_monitor                                        &
     &   (nod_fld%num_phys, nod_fld%num_component,                      &
     &    nod_fld%iflag_monitor, num_field_monitor, ntot_comp_monitor)
!
!
      end subroutine input_control_4_MHD
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_snapshot(FEM_prm, SGS_par, MHD_step,   &
     &          mesh, group, ele_mesh, nod_fld, IO_bc,                  &
     &          filtering, wide_filtering, wk_filter)
!
      use t_ctl_data_sph_MHD_psf
      use set_control_FEM_MHD
      use mpi_load_mesh_data
      use node_monitor_IO
      use ordering_field_by_viz
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(MHD_step_param), intent(inout) :: MHD_step
      type(phys_data), intent(inout) :: nod_fld
!
      type(IO_boundary), intent(inout) :: IO_bc
      type(filtering_data_type), intent(inout) :: filtering
      type(filtering_data_type), intent(inout) :: wide_filtering
      type(filtering_work_type), intent(inout) :: wk_filter
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_fem_snap'
      call read_control_4_fem_MHD(snap_ctl_name, FEM_MHD_ctl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_FEM_MHD'
      call set_control_4_FEM_MHD                                        &
     &   (FEM_MHD_ctl%plt, FEM_MHD_ctl%org_plt, FEM_MHD_ctl%model_ctl,  &
     &    FEM_MHD_ctl%ctl_ctl, FEM_MHD_ctl%nmtr_ctl, mesh1_file,        &
     &    FEM_udt_org_param, FEM_prm, SGS_par, MHD_step, nod_fld)
!
!  --  load FEM mesh data
      call mpi_input_mesh(mesh1_file, mesh, group,                      &
     &    ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
!
      call input_meshes_4_MHD(SGS_par%model_p, mesh, group, IO_bc,      &
     &    SGS_par%filter_p, filtering, wide_filtering, wk_filter)
!
      call count_field_4_monitor                                        &
     &   (nod_fld%num_phys, nod_fld%num_component,                      &
     &    nod_fld%iflag_monitor, num_field_monitor, ntot_comp_monitor)
!
      end subroutine input_control_4_snapshot
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_meshes_4_MHD(SGS_param, mesh, group, IO_bc,      &
     &          filter_param, filtering, wide_filtering, wk_filter)
!
      use m_machine_parameter
!
      use set_3d_filtering_group_id
      use read_filtering_data
      use set_surface_data_4_IO
      use set_edge_data_4_IO
      use node_monitor_IO
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
!
      type(IO_boundary), intent(inout) :: IO_bc
      type(SGS_filtering_params), intent(inout) :: filter_param
      type(filtering_data_type), intent(inout) :: filtering
      type(filtering_data_type), intent(inout) :: wide_filtering
      type(filtering_work_type), intent(inout) :: wk_filter
!
      integer(kind = kint) :: iflag
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_local_node_id_4_monitor'
      call set_local_node_id_4_monitor(mesh%node, group%nod_grp)
!
! ----  open data file for boundary data
!
      call boundary_file_IO_control(group, IO_bc)
!
! ---------------------------------
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 's_read_filtering_data'
      call s_read_filtering_data(SGS_param, filter_param,               &
     &    mesh%node, mesh%ele, filtering, wide_filtering, wk_filter)
!
      iflag = filter_param%iflag_SGS_filter
      if     (iflag .eq. id_SGS_3D_FILTERING                            &
     &  .or.  iflag .eq. id_SGS_3D_EZ_FILTERING                         &
     &  .or.  iflag .eq. id_SGS_3D_SMP_FILTERING                        &
     &  .or.  iflag .eq. id_SGS_3D_EZ_SMP_FILTERING ) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &       write(*,*) 's_set_3d_filtering_group_id'
        call s_set_3d_filtering_group_id                                &
     &     (filtering%filter, filter_param)
!
        if      (SGS_param%iflag_SGS .eq. id_SGS_similarity             &
     &     .and. SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &         write(*,*) 's_set_w_filtering_group_id'
          call copy_filter_group_param                                  &
     &       (filter_param%whole, filter_param%whole_wide)
          call copy_filter_group_param                                  &
     &       (filter_param%fluid, filter_param%fluid_wide)
        end if
      end if
!
      end subroutine input_meshes_4_MHD
!
! ----------------------------------------------------------------------
!
      subroutine boundary_file_IO_control(group, IO_bc)
!
      use m_physical_property
      use check_read_bc_file
!
      type(mesh_groups), intent(in) ::   group
      type(IO_boundary), intent(inout) :: IO_bc
!
      integer(kind = kint) :: iflag
!
!
      iflag = check_read_boundary_files                                 &
     &      (fl_prop1, cd_prop1, ht_prop1, cp_prop1)
      if (iflag .eq. id_no_boundary_file) return
!
      call read_bc_condition_file                                       &
     &     (my_rank, group%nod_grp, group%surf_grp, IO_bc)
!
      end subroutine boundary_file_IO_control
!
! ----------------------------------------------------------------------
!
      end module input_control
