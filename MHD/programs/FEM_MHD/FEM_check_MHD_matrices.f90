!FEM_check_MHD_matrices.f90
!      module FEM_check_MHD_matrices
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_check_MHD_mat                                    &
!!     &         (MHD_files, bc_FEM_IO, flex_p, flex_data, MHD_step,    &
!!     &          femmesh, ele_mesh, iphys_nod, nod_fld,                &
!!     &          iphys_ele, ele_fld, ak_MHD, MHD_CG, fem_sq, label_sim)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(IO_boundary), intent(in) :: bc_FEM_IO
!!        type(mesh_data), intent(inout) :: femmesh
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(phys_address), intent(inout) :: iphys_nod, iphys_ele
!!        type(phys_data), intent(inout) :: nod_fld, ele_fld
!!        type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!!        type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!        type(flexible_stepping_data), intent(inout) :: flex_data
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
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_flex_delta_t_data
      use t_FEM_MHD_solvers
      use t_FEM_MHD_filter_data
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
     &         (MHD_files, bc_FEM_IO, flex_p, flex_data, MHD_step,      &
     &          femmesh, ele_mesh, iphys_nod, nod_fld,                  &
     &          iphys_ele, ele_fld, ak_MHD, MHD_CG, fem_sq, label_sim)
!
      use m_SGS_control_parameter
      use m_control_parameter
      use m_geometry_data_MHD
      use m_physical_property
      use m_finite_element_matrix
      use m_bc_data_velo
      use m_finite_element_matrix
      use m_3d_filter_coef_MHD
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
      type(phys_address), intent(inout) :: iphys_nod, iphys_ele
      type(phys_data), intent(inout) :: nod_fld, ele_fld
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(flexible_stepping_data), intent(inout) :: flex_data
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      character(len=kchara), intent(inout)   :: label_sim
!
!
!   matrix assembling
!
      if (iflag_debug.eq.1) write(*,*) 'init_analyzer_fl'
      call init_analyzer_fl                                             &
     &   (MHD_files, bc_FEM_IO, FEM_prm1, SGS_par1, flex_p, flex_data,  &
     &    MHD_step, femmesh%mesh, femmesh%group, ele_mesh,              &
     &    MHD_mesh1, FEM_filters1, MHD_prop1, ak_MHD, FEM_MHD1_BCs,     &
     &    Csims_FEM_MHD1, iphys_nod, nod_fld, iphys_ele, ele_fld,       &
     &    fem_int1, mk_MHD1, MHD_CG, SGS_MHD_wk1, fem_sq, label_sim)
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices                                    &
     &   (femmesh, MHD_mesh1, MHD_prop1, fem_int1, MHD_CG%MGCG_WK,      &
     &    MHD1_mat_tbls, MHD_CG%MHD_mat, MHD_CG%solver_pack)
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices(MHD_step%time_d%dt, FEM_prm1,             &
     &    SGS_par1%model_p, SGS_par1%commute_p, femmesh,                &
     &    ele_mesh, MHD_mesh1, FEM_MHD1_BCs, MHD_prop1, ak_MHD,         &
     &    fem_int1, FEM_filters1%FEM_elens, Csims_FEM_MHD1,             &
     &    MHD1_mat_tbls, mk_MHD1, SGS_MHD_wk1%rhs_mat, MHD_CG)
!
      if (iflag_debug.eq.1) write(*,*) 's_write_djds_mat_MHD'
      call s_write_djds_mat_MHD                                         &
     &   (FEM_prm1, MHD_prop1%fl_prop, MHD_prop1%cd_prop,               &
     &    MHD_prop1%ht_prop, MHD_prop1%cp_prop,                         &
     &    MHD_CG%solver_pack%Vmatrix, MHD_CG%solver_pack%Pmatrix,       &
     &    MHD_CG%solver_pack%Bmatrix, MHD_CG%solver_pack%Fmatrix,       &
     &    MHD_CG%solver_pack%Tmatrix, MHD_CG%solver_pack%Cmatrix)
!
      end subroutine FEM_check_MHD_mat
!
! ----------------------------------------------------------------------
!
      end module FEM_check_MHD_matrices
