!FEM_check_MHD_matrices.f90
!      module FEM_check_MHD_matrices
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_check_MHD_mat                                    &
!!     &         (MHD_files, flex_MHD, MHD_step, FEM_model, MHD_CG,     &
!!     &          FEM_MHD, FEM_SGS, SGS_MHD_wk, MHD_IO, fem_sq,         &
!!     &          SR_sig, SR_r, SR_i, SR_il)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(FEM_MHD_model_data), intent(inout) :: FEM_model
!!        type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      module FEM_check_MHD_matrices
!
      use m_precision
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
      use t_FEM_mesh_field_data
      use t_material_property
      use t_FEM_MHD_model_data
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_FEM_MHD_time_stepping
      use t_FEM_MHD_solvers
      use t_FEM_SGS_structure
      use t_FEM_MHD_mean_square
      use t_MHD_IO_data
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
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
     &         (MHD_files, flex_MHD, MHD_step, FEM_model, MHD_CG,       &
     &          FEM_MHD, FEM_SGS, SGS_MHD_wk, MHD_IO, fem_sq,           &
     &          SR_sig, SR_r, SR_i, SR_il)
!
      use t_boundary_field_IO
!
      use initialization_4_MHD
!
      use construct_matrices
      use write_djds_mat_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(FEM_MHD_model_data), intent(inout) :: FEM_model
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
!   matrix assembling
!
      if (iflag_debug.eq.1) write(*,*) 'init_analyzer_fl'
      call init_analyzer_fl                                             &
     &   (MHD_files, FEM_model%bc_FEM_IO, FEM_model%FEM_prm,            &
     &    FEM_SGS%SGS_par, flex_MHD, MHD_step, FEM_MHD%geofem,          &
     &    FEM_model%MHD_mesh, FEM_SGS%FEM_filters,                      &
     &    FEM_model%MHD_prop, FEM_model%MHD_BC, FEM_model%FEM_MHD_BCs,  &
     &    FEM_SGS%Csims, FEM_MHD%iphys, FEM_SGS%iphys_LES,              &
     &    FEM_MHD%field, MHD_CG, SGS_MHD_wk, fem_sq,                    &
     &    MHD_IO%rst_IO, FEM_MHD%label_sim, FEM_MHD%v_sol,              &
     &    SR_sig, SR_r, SR_i, SR_il)
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices                                    &
     &   (FEM_MHD%geofem, FEM_model%MHD_mesh, FEM_model%MHD_prop,       &
     &    SGS_MHD_wk%fem_int, MHD_CG%MGCG_WK, MHD_CG%MHD_mat_tbls,      &
     &    MHD_CG%MHD_mat, MHD_CG%solver_pack)
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices(MHD_step%time_d%dt, FEM_model%FEM_prm,    &
     &     FEM_SGS%SGS_par, FEM_MHD%geofem, FEM_model%MHD_mesh,         &
     &    FEM_model%FEM_MHD_BCs, FEM_model%MHD_prop,                    &
     &    SGS_MHD_wk%fem_int, FEM_SGS%FEM_filters%FEM_elens,            &
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
