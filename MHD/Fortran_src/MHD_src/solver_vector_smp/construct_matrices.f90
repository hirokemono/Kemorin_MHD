!
!     module construct_matrices
!
!     Written by H. Matsui on June, 2005
!
!      subroutine set_data_4_const_matrices
!      subroutine update_matrices
!      subroutine set_aiccg_matrices
!
      module construct_matrices
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
      private :: set_index_list_4_matrix
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine set_data_4_const_matrices
!
      use m_geometry_constants

      use m_geometry_data
      use t_solver_djds
      use m_solver_djds_MHD
!
!   set off_diagonal information
!
      call set_index_list_4_matrix
!
!   deallocate work arrays
!
      if (iflag_debug.eq.1) write(*,*) 'deallocation work arrays'
      call dealloc_type_4_djds_table(DJDS_entire)
      call dealloc_type_4_djds_table(DJDS_fluid)
!      call dealloc_type_4_djds_table(DJDS_conduct)
!      call dealloc_type_4_djds_table(DJDS_insulator)
!
      if (ele1%nnod_4_ele .ne. num_t_linear) then
        call dealloc_type_4_djds_table(DJDS_linear)
        call dealloc_type_4_djds_table(DJDS_fl_l)
!        call dealloc_type_4_djds_table(DJDS_cd_l)
!        call dealloc_type_4_djds_table(DJDS_ins_l)
      end if
!
      end subroutine set_data_4_const_matrices
!
! ----------------------------------------------------------------------
!
      subroutine update_matrices
!
      use m_control_parameter
      use m_t_step_parameter
!
      integer (kind = kint) :: iflag
!
      iflag = 0
      if (    iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                 &
     &  .and. iflag_commute_linear .gt. id_SGS_commute_OFF              &
     &  .and. mod(i_step_MHD,i_step_sgs_coefs) .eq. 0) iflag = 1
      iflag = iflag + iflag_flex_step_changed
!
      if (iflag .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'matrix assemble again'
        call set_aiccg_matrices
        iflag_flex_step_changed = 0
      end if
!
      end subroutine update_matrices
!
!  ----------------------------------------------------------------------
!
      subroutine set_aiccg_matrices
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_iccg_parameter
      use m_int_vol_data
      use m_jacobians
      use m_element_id_4_node
      use m_sorted_node_MHD
      use m_finite_element_matrix
      use m_filter_elength
!
      use int_vol_lumped_mat_crank
      use int_vol_poisson_matrix
      use int_vol_consist_evo_mat
      use set_aiccg_bc_vectors
      use init_iccg_matrices
      use precond_djds_MHD
      use initialize_4_MHD_AMG
      use skip_comment_f
!
!
      call reset_aiccg_matrices(node1, ele1, fluid1)
!
!   set coefficients of matrix
!
      if (iflag_debug.eq.1) write(*,*) 'matrix assemble'
!
!   Poisson matrix
!
      call int_vol_poisson_matrices(ele1, jac1_3d_l, rhs_tbl1,          &
     &    mat_tbl_l1, mat_tbl_fl_l, FEM1_elen, fem1_wk)
!
!   Diffusion matrix
!
      if (iflag_scheme .eq. id_Crank_nicolson) then
        if (iflag_debug.eq.1) write(*,*) 'int_vol_crank_mat_lump'
        call int_vol_crank_mat_lump                                     &
     &     (node1, fluid1, conduct1, mhd_fem1_wk)
        if (iflag_debug.eq.1) write(*,*) 'int_vol_crank_matrices'
        call int_vol_crank_matrices(ele1, jac1_3d_q, rhs_tbl1,          &
     &      mat_tbl_q1, mat_tbl_fl_q, mat_tbl_full_cd_q,                &
     &      FEM1_elen, fem1_wk)
      else if (iflag_scheme .eq. id_Crank_nicolson_cmass) then
        call int_vol_crank_mat_consist                                  &
     &     (ele1, jac1_3d_q, rhs_tbl1, fem1_wk)
        call int_vol_crank_matrices(ele1, jac1_3d_q, rhs_tbl1,          &
     &      mat_tbl_q1, mat_tbl_fl_q, mat_tbl_full_cd_q,                &
     &      FEM1_elen, fem1_wk)
      end if
!
!     set boundary conditions
!
       if (iflag_debug.eq.1) write(*,*) 'set_aiccg_bc_phys'
      call set_aiccg_bc_phys
!
      if (iflag_debug.eq.1) write(*,*) 'preconditioning'
      call matrix_precondition
!
!
!     set marrix for the Multigrid
!
      if(cmp_no_case(method_4_solver, 'MGCG')) then
        call const_MGCG_MHD_matrices
      end if
!
      end subroutine set_aiccg_matrices
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_index_list_4_matrix
!
      use calypso_mpi
      use m_control_parameter
      use m_geometry_constants
      use m_geometry_data
      use m_element_id_4_node
      use m_sorted_node_MHD
      use m_solver_djds_MHD
!
      use set_index_list_4_djds
!
!
!      write(*,*) 'set_index_list_4_DJDS_mat_etr'
      call alloc_type_marix_list(ele1%nnod_4_ele, rhs_tbl1, mat_tbl_q1)
      call set_index_list_4_DJDS_mat_etr                                &
     &   (node1, ele1, rhs_tbl1, DJDS_entire, mat_tbl_q1)
!
!      write(*,*) 'set_index_list_4_mat_etr_l'
      call set_index_list_4_mat_etr_l                                   &
     &   (node1, ele1, rhs_tbl1, mat_tbl_q1)
!
      if (iflag_t_evo_4_velo .ne. id_no_evolution                       &
     &  .or. iflag_t_evo_4_temp .ne. id_no_evolution                    &
     &  .or. iflag_t_evo_4_composit .ne. id_no_evolution) then
!        write(*,*) 'set_index_list_4_mat_fl'
        call set_index_list_4_mat_fl(node1, ele1, rhs_tbl1)
!        write(*,*) 'set_index_list_4_mat_fl_l'
        call set_index_list_4_mat_fl_l(node1, ele1, rhs_tbl1)
      end if
!
      if (iflag_t_evo_4_magne .ne. id_no_evolution                      &
     &     .or. iflag_t_evo_4_vect_p .eq. id_Crank_nicolson_cmass) then
        write(*,*) 'set_index_list_4_mat_cd'
        call set_index_list_4_mat_cd(node1, ele1, rhs_tbl1)
!        write(*,*) 'set_index_list_4_mat_ins'
!        call set_index_list_4_mat_ins(node1, ele1, rhs_tbl1)
!
!        write(*,*) 'set_index_list_4_mat_cd_l'
!        call set_index_list_4_mat_cd_l(node1, ele1, rhs_tbl1)
!        write(*,*) 'set_index_list_4_mat_ins_l'
!        call set_index_list_4_mat_ins_l(node1, ele1, rhs_tbl1)
      end if
!
!
      end subroutine set_index_list_4_matrix
!
! ----------------------------------------------------------------------
!
      end module construct_matrices
