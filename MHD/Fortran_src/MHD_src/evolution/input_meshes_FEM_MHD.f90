!>@file   input_meshes_FEM_MHD.f90
!!@brief  module input_meshes_FEM_MHD
!!
!!@author H.Matsui and H.Okuda
!!@date     Programmed by H.Matsui and H.Okuda on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine s_input_meshes_FEM_MHD                               &
!!     &         (SGS_param, MHD_prop, MHD_BC, mesh, group, IO_bc,      &
!!     &          filter_param, FEM_filters, wk_filter)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(IO_boundary), intent(inout) :: IO_bc
!!        type(SGS_filtering_params), intent(inout) :: filter_param
!!        type(filters_on_FEM), intent(inout) :: FEM_filters
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!      subroutine input_MG_mesh_4_FEM_MHD(MHD_files, FEM_prm,          &
!!     &          MHD_mat, MGCG_WK, MGCG_FEM)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(MHD_MG_matrices), intent(inout) :: MHD_mat
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
!!@endverbatim
!
!
      module input_meshes_FEM_MHD
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_MHD_file_parameter
      use t_control_parameter
      use t_mesh_data
      use t_boundary_field_IO
      use t_bc_data_list
      use t_FEM_MHD_filter_data
      use t_filtering_data
      use t_solver_djds_MHD
      use t_MGCG_data
!
      implicit none
!
      private :: boundary_file_IO_control
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_meshes_FEM_MHD                                 &
     &         (SGS_param, MHD_prop, MHD_BC, mesh, group, IO_bc,        &
     &          filter_param, FEM_filters, wk_filter)
!
      use m_machine_parameter
!
      use set_3d_filtering_group_id
      use read_filtering_data
      use set_surface_data_4_IO
      use set_edge_data_4_IO
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
      end subroutine s_input_meshes_FEM_MHD
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
      end module input_meshes_FEM_MHD
