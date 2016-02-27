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
      use m_control_parameter
      use m_mesh_data
      use m_geometry_data_MHD
      use m_geometry_data
      use m_group_data
      use m_int_vol_data
      use m_jacobians
      use m_element_id_4_node
      use m_layering_ele_list
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
      call init_analyzer_fl(MHD_mesh1, layer_tbl1)
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices                                    &
     &   (mesh1%node, ele1, MHD_mesh1, rhs_tbl1, mat_tbl_q1)
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices(mesh1%node, ele1, surf1, MHD_mesh1,       &
     &    sf_grp1, jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q,              &
     &    rhs_tbl1, mat_tbl_q1, mhd_fem1_wk)
!
      if (iflag_debug.eq.1) write(*,*) 's_write_djds_mat_MHD'
      call s_write_djds_mat_MHD
!
      end subroutine FEM_check_MHD_mat
!
! ----------------------------------------------------------------------
!
      end module FEM_check_MHD_matrices
