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
!!      subroutine input_control_4_FEM_MHD                              &
!!     &         (MHD_files, FEM_prm, SGS_par, MHD_step, MHD_prop,      &
!!     &          MHD_BC, femmesh, ele_mesh, nod_fld, ele_fld,          &
!!     &          IO_bc, FEM_filters, FEM_SGS_wk, MHD_CG)
!!      subroutine input_control_4_FEM_snap                             &
!!     &         (MHD_files, FEM_prm, SGS_par, MHD_step, MHD_prop,      &
!!     &          MHD_BC, femmesh, ele_mesh, nod_fld, ele_fld,&
!!     &          IO_bc, FEM_filters, FEM_SGS_wk, MHD_CG)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(fluid_property), intent(inout) :: fl_prop
!!        type(conductive_property), intent(inout)  :: cd_prop
!!        type(scalar_property), intent(inout) :: ht_prop, cp_prop
!!        type(reference_scalar_param), intent(inout) :: ref_param_T
!!        type(reference_scalar_param), intent(inout) :: ref_param_C
!!        type(takepiro_model_param), intent(inout) :: takepito_T
!!        type(takepiro_model_param), intent(inout) :: takepito_C
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(mesh_data), intent(inout) :: femmesh
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(phys_data), intent(inout) :: nod_fld, ele_fld
!!        type(IO_boundary), intent(inout) :: IO_bc
!!        type(filters_on_FEM), intent(inout) :: FEM_filters
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(MHD_MG_matrices), intent(inout) :: MHD_mat
!!        type(FEM_MHD_solvers), intent(inout) :: MHD_CG
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
      use t_MHD_file_parameter
      use t_control_parameter
      use t_mesh_data
      use t_boundary_field_IO
      use t_phys_data
      use t_field_data_IO
      use t_ctl_data_FEM_MHD
      use t_FEM_MHD_solvers
      use t_bc_data_list
      use t_flex_delta_t_data
      use t_FEM_MHD_filter_data
      use t_work_FEM_dynamic_SGS
!
      implicit none
!
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!
!>      Control struture for MHD simulation
      type(fem_mhd_control), save :: FEM_MHD_ctl
!
      private :: FEM_MHD_ctl
      private :: input_meshes_4_MHD, boundary_file_IO_control
      private :: input_MG_mesh_4_FEM_MHD
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_FEM_MHD                                &
     &         (MHD_files, FEM_prm, SGS_par, MHD_step, MHD_prop,        &
     &          MHD_BC, femmesh, ele_mesh, nod_fld, ele_fld,            &
     &          IO_bc, FEM_filters, FEM_SGS_wk, MHD_CG)
!
      use set_control_FEM_MHD
      use mpi_load_mesh_data
      use skip_comment_f
      use ordering_field_by_viz
      use node_monitor_IO
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(mesh_data), intent(inout) :: femmesh
      type(element_geometry), intent(inout) :: ele_mesh
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(phys_data), intent(inout) :: nod_fld, ele_fld
!
      type(IO_boundary), intent(inout) :: IO_bc
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
      type(filters_on_FEM), intent(inout) :: FEM_filters
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_fem_MHD'
      call read_control_4_fem_MHD(MHD_ctl_name, FEM_MHD_ctl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_FEM_MHD'
      call set_control_4_FEM_MHD                                        &
     &   (FEM_MHD_ctl%plt, FEM_MHD_ctl%org_plt, FEM_MHD_ctl%model_ctl,  &
     &    FEM_MHD_ctl%fmctl_ctl, FEM_MHD_ctl%nmtr_ctl, MHD_files,       &
     &    FEM_prm, SGS_par, MHD_step, MHD_prop, MHD_BC,                 &
     &    MHD_CG%MGCG_WK, MHD_CG%MGCG_FEM, MHD_CG%MGCG_MHD_FEM,         &
     &    nod_fld, ele_fld)
!
!  --  load FEM mesh data
      call mpi_input_mesh                                               &
     &   (MHD_files%mesh_file_IO, nprocs, femmesh%mesh, femmesh%group,  &
     &    ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
!
      call input_meshes_4_MHD(SGS_par%model_p, MHD_prop, MHD_BC,        &
     &    femmesh%mesh, femmesh%group, IO_bc, SGS_par%filter_p,         &
     &    FEM_filters, FEM_SGS_wk%wk_filter)
!
      call input_MG_mesh_4_FEM_MHD(MHD_files, FEM_prm,                  &
     &     MHD_CG%MHD_mat,  MHD_CG%MGCG_WK,  MHD_CG%MGCG_FEM)
!
      call count_field_4_monitor                                        &
     &   (nod_fld%num_phys, nod_fld%num_component,                      &
     &    nod_fld%iflag_monitor, num_field_monitor, ntot_comp_monitor)
!
      end subroutine input_control_4_FEM_MHD
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_FEM_snap                               &
     &         (MHD_files, FEM_prm, SGS_par, MHD_step, MHD_prop,        &
     &          MHD_BC, femmesh, ele_mesh, nod_fld, ele_fld,            &
     &          IO_bc, FEM_filters, FEM_SGS_wk, MHD_CG)
!
      use set_control_FEM_MHD
      use mpi_load_mesh_data
      use node_monitor_IO
      use ordering_field_by_viz
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(mesh_data), intent(inout) :: femmesh
      type(element_geometry), intent(inout) :: ele_mesh
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(phys_data), intent(inout) :: nod_fld, ele_fld
!
      type(IO_boundary), intent(inout) :: IO_bc
      type(filters_on_FEM), intent(inout) :: FEM_filters
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_fem_snap'
      call read_control_4_fem_MHD(snap_ctl_name, FEM_MHD_ctl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_FEM_MHD'
      call set_control_4_FEM_MHD                                        &
     &   (FEM_MHD_ctl%plt, FEM_MHD_ctl%org_plt, FEM_MHD_ctl%model_ctl,  &
     &    FEM_MHD_ctl%fmctl_ctl, FEM_MHD_ctl%nmtr_ctl, MHD_files,       &
     &    FEM_prm, SGS_par, MHD_step, MHD_prop, MHD_BC,                 &
     &    MHD_CG%MGCG_WK, MHD_CG%MGCG_FEM, MHD_CG%MGCG_MHD_FEM,         &
     &    nod_fld, ele_fld)
!
!  --  load FEM mesh data
      call mpi_input_mesh                                               &
     &   (MHD_files%mesh_file_IO, nprocs, femmesh%mesh, femmesh%group,  &
     &    ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
!
      call input_meshes_4_MHD(SGS_par%model_p, MHD_prop,                &
     &    MHD_BC, femmesh%mesh, femmesh%group, IO_bc, SGS_par%filter_p, &
     &    FEM_filters, FEM_SGS_wk%wk_filter)
!
      call count_field_4_monitor                                        &
     &   (nod_fld%num_phys, nod_fld%num_component,                      &
     &    nod_fld%iflag_monitor, num_field_monitor, ntot_comp_monitor)
!
      end subroutine input_control_4_FEM_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_meshes_4_MHD                                     &
     &         (SGS_param, MHD_prop, MHD_BC, mesh, group, IO_bc,        &
     &          filter_param, FEM_filters, wk_filter)
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
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_BC_lists), intent(in) :: MHD_BC
!
!
      type(IO_boundary), intent(inout) :: IO_bc
      type(SGS_filtering_params), intent(inout) :: filter_param
      type(filters_on_FEM), intent(inout) :: FEM_filters
      type(filtering_work_type), intent(inout) :: wk_filter
!
      integer(kind = kint) :: iflag
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_local_nod_4_monitor'
      call set_local_nod_4_monitor(mesh, group)
!
! ----  open data file for boundary data
!
      call boundary_file_IO_control(MHD_prop, MHD_BC, group, IO_bc)
!
! ---------------------------------
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 's_read_filtering_data'
      call s_read_filtering_data(SGS_param, filter_param,               &
     &    mesh%node, mesh%ele, FEM_filters, wk_filter)
!
      iflag = filter_param%iflag_SGS_filter
      if     (iflag .eq. id_SGS_3D_FILTERING                            &
     &  .or.  iflag .eq. id_SGS_3D_EZ_FILTERING                         &
     &  .or.  iflag .eq. id_SGS_3D_SMP_FILTERING                        &
     &  .or.  iflag .eq. id_SGS_3D_EZ_SMP_FILTERING ) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &       write(*,*) 's_set_3d_filtering_group_id'
        call s_set_3d_filtering_group_id                                &
     &     (FEM_filters%filtering%filter, filter_param)
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
      subroutine boundary_file_IO_control                               &
     &         (MHD_prop, MHD_BC, group, IO_bc)
!
      use check_read_bc_file
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_BC_lists), intent(in) :: MHD_BC
!
      type(mesh_groups), intent(in) ::   group
      type(IO_boundary), intent(inout) :: IO_bc
!
      integer(kind = kint) :: iflag
!
!
      iflag = check_read_boundary_files(MHD_prop, MHD_BC)
      if (iflag .eq. id_no_boundary_file) return
!
      call read_bc_condition_file                                       &
     &     (my_rank, group%nod_grp, group%surf_grp, IO_bc)
!
      end subroutine boundary_file_IO_control
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      subroutine input_MG_mesh_4_FEM_MHD(MHD_files, FEM_prm,            &
     &          MHD_mat, MGCG_WK, MGCG_FEM)
!
      use m_flags_4_solvers
      use input_MG_data
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!
      type(MHD_MG_matrices), intent(inout) :: MHD_mat
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
!
!
      if(cmp_no_case(FEM_PRM%CG11_param%METHOD, cflag_mgcg)) then
        call alloc_MHD_MG_DJDS_mat(MGCG_WK%num_MG_level, MHD_mat)
        call input_MG_mesh                                              &
     &     (FEM_prm%MG_file, MGCG_WK, MGCG_FEM, MHD_files%mesh_file_IO)
        call input_MG_itp_tables(FEM_prm%MG_file, MGCG_WK, MGCG_FEM,    &
     &      MHD_mat%MG_interpolate)
      else
        MGCG_WK%num_MG_level = 0
        call alloc_MHD_MG_DJDS_mat(MGCG_WK%num_MG_level, MHD_mat)
      end if
!
      end subroutine input_MG_mesh_4_FEM_MHD
!
! ----------------------------------------------------------------------
!
      end module input_control
