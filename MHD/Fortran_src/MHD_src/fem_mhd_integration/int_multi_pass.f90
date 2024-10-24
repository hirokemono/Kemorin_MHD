!
!      module int_multi_pass
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!
!!      subroutine int_multi_pass_vector                                &
!!     &         (iele_fsmp_stack, FEM_prm, m_lump, nod_comm, node, ele,&
!!     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl, v_sol)
!!      subroutine int_multi_pass_scalar                                &
!!     &         (iele_fsmp_stack, FEM_prm, m_lump, nod_comm, node, ele,&
!!     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl, v_sol)
!!
!!      subroutine int_multi_pass_vector_upw                            &
!!     &         (iele_fsmp_stack, iphys_upw, dt, FEM_prm, m_lump,      &
!!     &          nod_comm, node, ele, ele_fld, g_FEM, jac_3d, rhs_tbl, &
!!     &          ff_m_smp, fem_wk, f_nl, v_sol)
!!      subroutine int_multi_pass_scalar_upw                            &
!!     &         (iele_fsmp_stack, iphys_upw, dt, FEM_prm, m_lump,      &
!!     &          nod_comm, node, ele, ele_fld, g_FEM, jac_3d, rhs_tbl, &
!!     &          ff_m_smp, fem_wk, f_nl, v_sol)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(phys_data), intent(in) :: ele_fld
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(vectors_4_solver), intent(inout) :: v_sol
!
      module int_multi_pass
!
      use m_precision
      use m_constants
!
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_vector_for_solver
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector                                  &
     &         (iele_fsmp_stack, FEM_prm, m_lump, nod_comm, node, ele,  &
     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl, v_sol)
!
      use int_vol_multi_pass
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(n_vector, node, f_nl)
!
      do imulti = 2, FEM_prm%num_multi_pass
        call cal_ff_smp_2_vector(node, rhs_tbl,                         &
     &      f_nl%ff_smp, m_lump%ml, n_vector, ione, f_nl%ff)
        call nod_vector_send_recv(node%numnod, nod_comm,                &
     &                            f_nl%ff, v_sol)
!
        call int_vol_multi_pass_vector                                  &
     &     (FEM_prm%npoint_t_evo_int, iele_fsmp_stack,                  &
     &      node, ele, g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl)
      end do
!
      end subroutine int_multi_pass_vector
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar                                  &
     &         (iele_fsmp_stack, FEM_prm, m_lump, nod_comm, node, ele,  &
     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl, v_sol)
!
      use int_vol_multi_pass
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(n_vector, node, f_nl)
!
      do imulti = 2, FEM_prm%num_multi_pass
        call cal_ff_smp_2_scalar(node, rhs_tbl,                         &
     &      f_nl%ff_smp, m_lump%ml, n_vector, ione, f_nl%ff)
        call nod_scalar_send_recv(node%numnod, nod_comm,                &
     &                            f_nl%ff(1,1), v_sol)
!
        call int_vol_multi_pass_scalar                                  &
     &     (FEM_prm%npoint_t_evo_int, iele_fsmp_stack,                  &
     &      node, ele, g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl)
      end do
!
      end subroutine int_multi_pass_scalar
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_vector_upw                              &
     &          (iele_fsmp_stack, iphys_upw, dt, FEM_prm, m_lump,       &
     &           nod_comm, node, ele, ele_fld, g_FEM, jac_3d, rhs_tbl,  &
     &           ff_m_smp, fem_wk, f_nl, v_sol)
!
      use int_vol_multi_pass
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(phys_data), intent(in) :: ele_fld
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: iphys_upw
      real(kind = kreal), intent(in) :: dt
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(n_vector, node, f_nl)
!
      do imulti = 2, FEM_prm%num_multi_pass
        call cal_ff_smp_2_vector(node, rhs_tbl,                         &
     &      f_nl%ff_smp, m_lump%ml, n_vector, ione, f_nl%ff)
        call nod_vector_send_recv(node%numnod, nod_comm,                &
     &                            f_nl%ff, v_sol)
!
        call int_vol_multi_pass_vector_upw                              &
     &     (FEM_prm%npoint_t_evo_int, dt, iele_fsmp_stack,              &
     &      node, ele, g_FEM, jac_3d, rhs_tbl,                          &
     &      ele_fld%ntot_phys, iphys_upw, ele_fld%d_fld,                &
     &      ff_m_smp, fem_wk, f_nl)
      end do
!
      end subroutine int_multi_pass_vector_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_multi_pass_scalar_upw                              &
     &         (iele_fsmp_stack, iphys_upw, dt, FEM_prm, m_lump,        &
     &          nod_comm, node, ele, ele_fld, g_FEM, jac_3d, rhs_tbl,   &
     &          ff_m_smp, fem_wk, f_nl, v_sol)
!
      use int_vol_multi_pass
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(phys_data), intent(in) :: ele_fld
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: iphys_upw
      real(kind = kreal), intent(in) :: dt
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer (kind = kint) :: imulti
!
!
      call reset_ff(n_vector, node, f_nl)
!
      do imulti = 2, FEM_prm%num_multi_pass
        call cal_ff_smp_2_scalar(node, rhs_tbl,                         &
     &      f_nl%ff_smp, m_lump%ml, n_vector, ione, f_nl%ff)
        call nod_scalar_send_recv(node%numnod, nod_comm,                &
     &                            f_nl%ff(1,1), v_sol)
!
        call int_vol_multi_pass_scalar_upw                              &
     &     (FEM_prm%npoint_t_evo_int, dt, iele_fsmp_stack,              &
     &      node, ele, g_FEM, jac_3d, rhs_tbl,                          &
     &      ele_fld%ntot_phys, iphys_upw, ele_fld%d_fld,                &
     &      ff_m_smp, fem_wk, f_nl)
      end do
!
      end subroutine int_multi_pass_scalar_upw
!
!-----------------------------------------------------------------------
!
      end module int_multi_pass
