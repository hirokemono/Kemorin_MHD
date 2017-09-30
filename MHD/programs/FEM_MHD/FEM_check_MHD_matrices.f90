!FEM_check_MHD_matrices.f90
!      module FEM_check_MHD_matrices
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_check_MHD_mat                                    &
!!     &        (MHD_files, bc_FEM_IO, flex_MHD, MHD_step,              &
!!     &         femmesh, ele_mesh, iphys_nod, nod_fld, FEM_model,      &
!!     &         MHD_CG, FEM_SGS, SGS_MHD_wk, fem_sq, label_sim)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(IO_boundary), intent(in) :: bc_FEM_IO
!!        type(mesh_data), intent(inout) :: femmesh
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(phys_address), intent(inout) :: iphys_nod
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(FEM_MHD_model_data), intent(inout) :: FEM_model
!!        type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
!
      module FEM_check_MHD_matrices
!
      use m_precision
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_material_property
      use t_FEM_MHD_model_data
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_FEM_MHD_time_stepping
      use t_FEM_MHD_solvers
      use t_FEM_SGS_structure
      use t_FEM_MHD_mean_square
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_check_MHD_mat                                      &
     &        (MHD_files, bc_FEM_IO, flex_MHD, MHD_step,                &
     &         femmesh, ele_mesh, iphys_nod, nod_fld, FEM_model,        &
     &         MHD_CG, FEM_SGS, SGS_MHD_wk, fem_sq, label_sim)
!
      use t_boundary_field_IO
!
      use initialization_4_MHD
!
      use construct_matrices
      use write_djds_mat_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(IO_boundary), intent(in) :: bc_FEM_IO
!
      type(mesh_data), intent(inout) :: femmesh
      type(element_geometry), intent(inout) :: ele_mesh
      type(phys_address), intent(inout) :: iphys_nod
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_MHD_model_data), intent(inout) :: FEM_model
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      character(len=kchara), intent(inout)   :: label_sim
!
!
!   matrix assembling
!
      if (iflag_debug.eq.1) write(*,*) 'init_analyzer_fl'
      call init_analyzer_fl(MHD_files, bc_FEM_IO,                       &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par, flex_MHD, MHD_step,       &
     &    femmesh%mesh, femmesh%group, ele_mesh,                        &
     &    FEM_model%MHD_mesh, FEM_SGS%FEM_filters,                      &
     &    FEM_model%MHD_prop, FEM_model%MHD_BC, FEM_model%FEM_MHD_BCs,  &
     &    FEM_SGS%Csims, iphys_nod, nod_fld, MHD_CG, SGS_MHD_wk,        &
     &    fem_sq, label_sim)
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices                                    &
     &   (femmesh, FEM_model%MHD_mesh, FEM_model%MHD_prop,              &
     &    SGS_MHD_wk%fem_int, MHD_CG%MGCG_WK, MHD_CG%MHD_mat_tbls,      &
     &    MHD_CG%MHD_mat, MHD_CG%solver_pack)
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices(MHD_step%time_d%dt,                       &
     &    FEM_model%FEM_prm, FEM_SGS%SGS_par,                           &
     &    femmesh, ele_mesh, FEM_model%MHD_mesh,                        &
     &    FEM_model%FEM_MHD_BCs, FEM_model%MHD_prop,                    &
     &    SGS_MHD_wk%fem_int,  FEM_SGS%FEM_filters%FEM_elens,           &
     &    FEM_SGS%Csims, SGS_MHD_wk%mk_MHD, SGS_MHD_wk%rhs_mat, MHD_CG)
!
      if (iflag_debug.eq.1) write(*,*) 's_write_djds_mat_MHD'
      call s_write_djds_mat_MHD                                         &
     &   (FEM_model%FEM_prm, FEM_model%MHD_prop, MHD_CG%solver_pack)
!
      end subroutine FEM_check_MHD_mat
!
! ----------------------------------------------------------------------
!
      end module FEM_check_MHD_matrices
