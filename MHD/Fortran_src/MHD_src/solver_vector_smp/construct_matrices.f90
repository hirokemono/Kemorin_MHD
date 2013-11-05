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
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine set_data_4_const_matrices
!
      use m_geometry_constants
      use m_geometry_parameter
      use t_solver_djds
      use m_solver_djds_MHD
      use m_solver_djds_fluid
!      use m_solver_djds_conduct
!      use m_solver_djds_insulate
      use m_solver_djds_linear_fl
!      use m_solver_djds_linear_cd
!      use m_solver_djds_linear_ins
!
      use set_index_list_MHD_matrix
!
!   set off_diagonal information
!
      call set_index_list_4_matrix
!
!   deallocate work arrays
!
      if (iflag_debug.eq.1) write(*,*) 'deallocation work arrays'
      call dealloc_type_4_djds_table(DJDS_entire)
      call deallocate_4_djds_table_fl
!      call deallocate_4_djds_table_cd
!      call deallocate_4_djds_table_ins
!
      if ( nnod_4_ele .ne. num_t_linear) then
        call dealloc_type_4_djds_table(DJDS_linear)
        call deallocate_4_djds_table_fl_l
!        call deallocate_4_djds_table_cd_l
!        call deallocate_4_djds_table_ins_l
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
      use m_iccg_parameter
!
      use int_vol_lumped_mat_crank
      use int_vol_poisson_matrix
      use int_vol_consist_evo_mat
      use set_aiccg_bc_vectors
      use init_iccg_matrices
      use precond_djds_MHD
      use initialize_4_MHD_AMG
!
!
      call reset_aiccg_matrices
!
!   set coefficients of matrix
!
      if (iflag_debug.eq.1) write(*,*) 'matrix assemble'
!
!   Poisson matrix
!
      call int_vol_poisson_matrices
!
!   Diffusion matrix
!
      if (iflag_scheme .eq. id_Crank_nicolson) then
        call int_vol_crank_mat_lump
        call int_vol_crank_matrices
      else if (iflag_scheme .eq. id_Crank_nicolson_cmass) then
        call int_vol_crank_mat_consist
        call int_vol_crank_matrices
      end if
!
!     set boundary conditions
!
      call set_aiccg_bc_phys
!
      if (iflag_debug.eq.1) write(*,*) 'preconditioning'
      call matrix_precondition
!
!
!     set marrix for the Multigrid
!
      if (     ((method_4_solver(1:1).eq.'M')                           &
     &      .or.(method_4_solver(1:1).eq.'m'))                          &
     &   .and. ((method_4_solver(2:2).eq.'G')                           &
     &      .or.(method_4_solver(2:2).eq.'g'))                          &
     &   .and. ((method_4_solver(3:3).eq.'C')                           &
     &      .or.(method_4_solver(3:3).eq.'c'))                          &
     &   .and. ((method_4_solver(4:4).eq.'G')                           &
     &      .or.(method_4_solver(4:4).eq.'g')) ) then
        call const_MGCG_MHD_matrices
      end if
!
!
      end subroutine set_aiccg_matrices
!
! ----------------------------------------------------------------------
!
      end module construct_matrices
