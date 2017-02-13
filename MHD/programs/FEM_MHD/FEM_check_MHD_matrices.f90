!FEM_check_MHD_matrices.f90
!      module FEM_check_MHD_matrices
!
!      modified by H. Matsui on June, 2005 
!
!      subroutine FEM_check_MHD_mat
!
      module FEM_check_MHD_matrices
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use m_t_step_parameter
      use m_t_int_parameter
!
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_check_MHD_mat
!
      use m_SGS_control_parameter
      use m_control_parameter
      use m_mesh_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_layering_ele_list
      use m_sorted_node_MHD
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_bc_data_velo
      use m_solver_djds_MHD
      use m_boundary_field_IO
!
      use initialization_4_MHD
!
      use construct_matrices
      use write_djds_mat_MHD
!
!
!   matrix assembling
!
      if (iflag_debug.eq.1) write(*,*) 'init_analyzer_fl'
      call init_analyzer_fl                                             &
     &   (FEM_prm1, SGS_par1, IO_bc1, mesh1, group1, ele_mesh1,         &
     &    MHD_mesh1, layer_tbl1, iphys, nod_fld1, label_sim)
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices(mesh1, MHD_mesh1, rhs_tbl1,        &
     &    MHD1_mat_tbls, MHD1_matrices, solver_pack1)
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices                                           &
     &   (FEM_prm1, SGS_par1%model_p, SGS_par1%commute_p,               &
     &    mesh1, group1, ele_mesh1, MHD_mesh1, nod1_bcs, sf1_bcs,       &
     &    ak_MHD, jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, FEM1_elen,    &
     &    ifld_diff, diff_coefs, rhs_tbl1, MHD1_mat_tbls,               &
     &    surf1_wk, mhd_fem1_wk, fem1_wk, MHD1_matrices, solver_pack1)
!
      if (iflag_debug.eq.1) write(*,*) 's_write_djds_mat_MHD'
      call s_write_djds_mat_MHD                                         &
     &   (solver_pack1%Vmatrix, solver_pack1%Pmatrix,                   &
     &    solver_pack1%Bmatrix, solver_pack1%Fmatrix,                   &
     &    solver_pack1%Tmatrix, solver_pack1%Cmatrix)
!
      end subroutine FEM_check_MHD_mat
!
! ----------------------------------------------------------------------
!
      end module FEM_check_MHD_matrices
