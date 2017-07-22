!FEM_check_MHD_matrices.f90
!      module FEM_check_MHD_matrices
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_check_MHD_mat(MHD_files)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!
      module FEM_check_MHD_matrices
!
      use m_precision
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
      use t_MHD_file_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_check_MHD_mat(MHD_files)
!
      use m_MHD_step_parameter
      use m_SGS_control_parameter
      use m_control_parameter
      use m_mesh_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_physical_property
      use m_finite_element_matrix
      use m_filter_elength
      use m_layering_ele_list
      use m_sorted_node_MHD
      use m_bc_data_velo
      use m_solver_djds_MHD
      use m_boundary_field_IO
      use m_type_AMG_data
      use m_element_phys_data
!
      use initialization_4_MHD
!
      use construct_matrices
      use write_djds_mat_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
!
!   matrix assembling
!
      if (iflag_debug.eq.1) write(*,*) 'init_analyzer_fl'
      call init_analyzer_fl                                             &
     &   (MHD_files, IO_bc1, FEM_prm1, SGS_par1, MHD_step1,             &
     &    mesh1, group1, ele_mesh1, MHD_mesh1, layer_tbl1,              &
     &    MHD_prop1, ak_MHD, Csims_FEM_MHD1,                            &
     &    iphys, nod_fld1, label_sim)
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices                                    &
     &   (mesh1, MHD_mesh1, MHD_prop1, fem_int1,                        &
     &    MGCG_WK1, MHD1_mat_tbls, MHD1_matrices, solver_pack1)
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices(MHD_step1%time_d%dt, FEM_prm1,            &
     &    SGS_par1%model_p, SGS_par1%commute_p, mesh1, group1,          &
     &    ele_mesh1, MHD_mesh1, nod1_bcs, sf1_bcs, MHD_prop1, ak_MHD,   &
     &    fem_int1, FEM1_elen, Csims_FEM_MHD1, MHD1_mat_tbls,           &
     &    rhs_mat1, mhd_fem1_wk, MHD1_matrices)
!
      if (iflag_debug.eq.1) write(*,*) 's_write_djds_mat_MHD'
      call s_write_djds_mat_MHD                                         &
     &   (FEM_prm1, MHD_prop1%fl_prop, MHD_prop1%cd_prop,               &
     &    MHD_prop1%ht_prop, MHD_prop1%cp_prop,                         &
     &    solver_pack1%Vmatrix, solver_pack1%Pmatrix,                   &
     &    solver_pack1%Bmatrix, solver_pack1%Fmatrix,                   &
     &    solver_pack1%Tmatrix, solver_pack1%Cmatrix)
!
      end subroutine FEM_check_MHD_mat
!
! ----------------------------------------------------------------------
!
      end module FEM_check_MHD_matrices
