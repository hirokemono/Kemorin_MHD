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
!!     &          MHD_BC, femmesh, nod_fld, ele_fld, nod_mntr, IO_bc,   &
!!     &          FEM_filters, FEM_SGS_wk, MHD_CG, viz_ctls)
!!      subroutine input_control_4_FEM_snap                             &
!!     &         (MHD_files, FEM_prm, SGS_par, MHD_step, MHD_prop,      &
!!     &          MHD_BC, femmesh, nod_fld, ele_fld, nod_mntr, IO_bc,   &
!!     &          FEM_filters, FEM_SGS_wk, MHD_CG, viz_ctls)
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
!!        type(phys_data), intent(inout) :: nod_fld, ele_fld
!!        type(IO_boundary), intent(inout) :: IO_bc
!!        type(filters_on_FEM), intent(inout) :: FEM_filters
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(MHD_MG_matrices), intent(inout) :: MHD_mat
!!        type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(viz_repartition_ctl), intent(inout) :: repart_ctl
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
      use t_control_data_vizs
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
      use t_ctl_data_SGS_model
      use t_FEM_MHD_solvers
      use t_bc_data_list
      use t_flex_delta_t_data
      use t_FEM_MHD_filter_data
      use t_work_FEM_dynamic_SGS
      use t_node_monitor_IO
!
      implicit none
!
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!
!>      Control struture for MHD simulation
      type(fem_mhd_control), save, private :: FEM_MHD_ctl1
!>      Structures for SGS controls
      type(SGS_model_control), save, private :: sgs_ctl_F
!
      private :: load_control_4_fem_MHD
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_control_4_fem_MHD                                 &
     &         (file_name, FEM_MHD_ctl, sgs_ctl, viz_ctls)
!
      use t_ctl_data_FEM_MHD
      use t_ctl_data_SGS_model
      use t_control_data_vizs
      use bcast_control_FEM_MHD
      use bcast_ctl_SGS_MHD_model
      use bcast_control_data_vizs
!
      character(len=kchara), intent(in) :: file_name
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      if(my_rank .eq. 0) then
        call read_control_4_fem_MHD(file_name,                          &
     &      FEM_MHD_ctl, sgs_ctl, viz_ctls)
      end if
!
      call bcast_fem_mhd_ctl_data(FEM_MHD_ctl)
      call bcast_sgs_ctl(sgs_ctl)
      call bcast_viz_controls(viz_ctls)
!
      if(FEM_MHD_ctl%i_mhd_ctl .ne. 1) then
        call calypso_MPI_abort(FEM_MHD_ctl%i_mhd_ctl, trim(file_name))
      end if
!
      end subroutine load_control_4_fem_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_4_FEM_MHD                                &
     &         (MHD_files, FEM_prm, SGS_par, MHD_step, MHD_prop,        &
     &          MHD_BC, femmesh, nod_fld, ele_fld, nod_mntr, IO_bc,     &
     &          FEM_filters, FEM_SGS_wk, MHD_CG, viz_ctls)
!
      use set_control_FEM_MHD
      use mpi_load_mesh_data
      use skip_comment_f
      use set_field_data_w_SGS
      use set_nodal_field_name
      use input_meshes_FEM_MHD
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(mesh_data), intent(inout) :: femmesh
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(phys_data), intent(inout) :: nod_fld, ele_fld
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
      type(IO_boundary), intent(inout) :: IO_bc
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
      type(filters_on_FEM), intent(inout) :: FEM_filters
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      if (iflag_debug.eq.1) write(*,*) 'load_control_4_fem_MHD'
      call load_control_4_fem_MHD(MHD_ctl_name, FEM_MHD_ctl1,           &
     &                            sgs_ctl_F, viz_ctls)
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_FEM_MHD'
      call set_control_4_FEM_MHD(FEM_MHD_ctl1%plt,                      &
     &    FEM_MHD_ctl1%org_plt, sgs_ctl_F, FEM_MHD_ctl1%model_ctl,      &
     &    FEM_MHD_ctl1%fmctl_ctl, FEM_MHD_ctl1%nmtr_ctl,                &
     &    MHD_files, FEM_prm, SGS_par, MHD_step, MHD_prop, MHD_BC,      &
     &    MHD_CG%MGCG_WK, MHD_CG%MGCG_FEM, MHD_CG%MGCG_MHD_FEM,         &
     &    nod_fld, ele_fld, nod_mntr)
      call dealloc_sgs_ctl(sgs_ctl_F)
      call dealloc_sph_mhd_model(FEM_MHD_ctl1%model_ctl)
!
!  --  load FEM mesh data
      call mpi_input_mesh(MHD_files%mesh_file_IO, nprocs, femmesh)
!
      call s_input_meshes_FEM_MHD(SGS_par%model_p, MHD_prop, MHD_BC,    &
     &    femmesh%mesh, femmesh%group, IO_bc, SGS_par%filter_p,         &
     &    FEM_filters, FEM_SGS_wk%wk_filter)
!
      call input_MG_mesh_4_FEM_MHD(MHD_files, FEM_prm,                  &
     &     MHD_CG%MHD_mat,  MHD_CG%MGCG_WK,  MHD_CG%MGCG_FEM)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_local_nod_4_monitor'
      call set_local_nod_4_monitor(femmesh%mesh, femmesh%group,         &
     &                             nod_mntr)
      call count_field_4_monitor(nod_fld, nod_mntr)
!
      end subroutine input_control_4_FEM_MHD
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_FEM_snap                               &
     &         (MHD_files, FEM_prm, SGS_par, MHD_step, MHD_prop,        &
     &          MHD_BC, femmesh, nod_fld, ele_fld, nod_mntr, IO_bc,     &
     &          FEM_filters, FEM_SGS_wk, MHD_CG, viz_ctls)
!
      use set_control_FEM_MHD
      use mpi_load_mesh_data
      use set_field_data_w_SGS
      use set_nodal_field_name
      use input_meshes_FEM_MHD
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(mesh_data), intent(inout) :: femmesh
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(phys_data), intent(inout) :: nod_fld, ele_fld
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
      type(IO_boundary), intent(inout) :: IO_bc
      type(filters_on_FEM), intent(inout) :: FEM_filters
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      if (iflag_debug.eq.1) write(*,*) 'load_control_4_fem_MHD'
      call load_control_4_fem_MHD(snap_ctl_name, FEM_MHD_ctl1,          &
     &                            sgs_ctl_F, viz_ctls)
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_FEM_MHD'
      call set_control_4_FEM_MHD(FEM_MHD_ctl1%plt,                      &
     &    FEM_MHD_ctl1%org_plt, sgs_ctl_F, FEM_MHD_ctl1%model_ctl,      &
     &    FEM_MHD_ctl1%fmctl_ctl, FEM_MHD_ctl1%nmtr_ctl,                &
     &    MHD_files, FEM_prm, SGS_par, MHD_step, MHD_prop, MHD_BC,      &
     &    MHD_CG%MGCG_WK, MHD_CG%MGCG_FEM, MHD_CG%MGCG_MHD_FEM,         &
     &    nod_fld, ele_fld, nod_mntr)
      call dealloc_sgs_ctl(sgs_ctl_F)
      call dealloc_sph_mhd_model(FEM_MHD_ctl1%model_ctl)
!
!  --  load FEM mesh data
      call mpi_input_mesh                                               &
     &   (MHD_files%mesh_file_IO, nprocs, femmesh)
!
      call s_input_meshes_FEM_MHD(SGS_par%model_p, MHD_prop,            &
     &    MHD_BC, femmesh%mesh, femmesh%group, IO_bc, SGS_par%filter_p, &
     &    FEM_filters, FEM_SGS_wk%wk_filter)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_local_nod_4_monitor'
      call set_local_nod_4_monitor(femmesh%mesh, femmesh%group,         &
     &                             nod_mntr)
      call count_field_4_monitor(nod_fld, nod_mntr)
!
      end subroutine input_control_4_FEM_snap
!
! ----------------------------------------------------------------------
!
      end module input_control
