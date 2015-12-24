!
!      module int_MHD_mass_matrices
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Oct. 2006
!
!      subroutine int_RHS_mass_matrices
!
      module int_MHD_mass_matrices
!
      use m_precision
!
      use m_geometry_constants
      use m_geometry_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
!
      use int_vol_mass_matrix
!
      implicit none
!
      private :: int_mass_matrix_trilinear, int_mass_matrices_quad
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_RHS_mass_matrices
!
      use m_control_parameter
!
      integer(kind = kint) :: num_int
!
!
      num_int = intg_point_t_evo
!
      if     (ele1%nnod_4_ele.eq.num_t_quad                             &
     &   .or. ele1%nnod_4_ele.eq.num_t_lag) then
        call int_mass_matrices_quad(num_int)
      else
        call int_mass_matrix_trilinear(num_int)
      end if
!
      end subroutine int_RHS_mass_matrices
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_trilinear(n_int)
!
      use m_machine_parameter
      use m_geometry_data_MHD
      use m_int_vol_data
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_lump_mass_matrix_linear'
      call int_lump_mass_matrix_linear                                  &
     &   (node1, ele1, jac1_3d_q, rhs_tbl1, n_int,                      &
     &    fem1_wk, f1_l, m1_lump)
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_diag fluid'
      call int_mass_matrix_diag(node1, ele1, jac1_3d_q, rhs_tbl1,       &
     &    fluid1%istack_ele_fld_smp, n_int, fem1_wk, f1_l,              &
     &    mhd_fem1_wk%mlump_fl)
!      call check_mass_martix_fluid(my_rank, node1%numnod, mhd_fem1_wk)
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_diag conductor'
      call int_mass_matrix_diag(node1, ele1, jac1_3d_q, rhs_tbl1,       &
     &    conduct1%istack_ele_fld_smp, n_int, fem1_wk, f1_l,            &
     &    mhd_fem1_wk%mlump_cd)
!      call check_mass_martix_conduct                                   &
!     &   (my_rank, node1%numnod, mhd_fem1_wk)
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_diag insulator'
      call int_mass_matrix_diag(node1, ele1, jac1_3d_q, rhs_tbl1,       &
     &    insulate1%istack_ele_fld_smp, n_int, fem1_wk, f1_l,           &
     &    mhd_fem1_wk%mlump_ins)
!
      end subroutine int_mass_matrix_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrices_quad(n_int)
!
      use m_machine_parameter
      use m_geometry_data_MHD
      use m_int_vol_data
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_lump_mass_matrix_quad'
      call int_lump_mass_matrix_quad(node1, ele1, jac1_3d_q, rhs_tbl1,  &
     &    n_int, fem1_wk, f1_l, m1_lump)
!      call check_mass_martix(my_rank, node1%numnod, m1_lump)
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_HRZ fluid'
      call int_mass_matrix_HRZ(node1, ele1, jac1_3d_q, rhs_tbl1,        &
     &    fluid1%istack_ele_fld_smp, n_int,                             &
     &    fem1_wk, f1_l, mhd_fem1_wk%mlump_fl)
!      call check_mass_martix_fluid(my_rank, node1%numnod, mhd_fem1_wk)
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_HRZ conduct'
      call int_mass_matrix_HRZ(node1, ele1, jac1_3d_q, rhs_tbl1,        &
     &    conduct1%istack_ele_fld_smp, n_int, fem1_wk, f1_l,            &
     &    mhd_fem1_wk%mlump_cd)
!      call check_mass_martix_conduct                                   &
!     &   (my_rank, node1%numnod, mhd_fem1_wk)
!
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'int_mass_matrix_HRZ insulator'
      call int_mass_matrix_HRZ(node1, ele1, jac1_3d_q, rhs_tbl1,        &
     &    insulate1%istack_ele_fld_smp, n_int,                          &
     &     fem1_wk, f1_l, mhd_fem1_wk%mlump_ins)
!
      end subroutine int_mass_matrices_quad
!
!-----------------------------------------------------------------------

      end module int_MHD_mass_matrices
