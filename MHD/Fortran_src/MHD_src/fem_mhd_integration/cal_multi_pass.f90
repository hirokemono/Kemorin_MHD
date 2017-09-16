!
!     module cal_multi_pass
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine cal_t_evo_4_vector                                   &
!!     &         (iflag_4_supg, iele_fsmp_stack, dt, FEM_prm, m_lump,   &
!!     &          nod_comm, node, ele, iphys_ele, ele_fld,              &
!!     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
!!      subroutine cal_t_evo_4_scalar                                   &
!!     &         (iflag_4_supg, iele_fsmp_stack, dt, FEM_prm, m_lump,   &
!!     &          nod_comm, node, ele, iphys_ele, ele_fld,              &
!!     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
!!
!!      subroutine cal_t_evo_4_vector_cd                                &
!!     &         (iflag_4_supg, iele_fsmp_stack, dt, FEM_prm, m_lump,   &
!!     &          nod_comm, node, ele, iphys_ele, ele_fld,              &
!!     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
!!      subroutine cal_t_evo_4_scalar_cd                                &
!!     &         (iflag_4_supg, iele_fsmp_stack, dt, FEM_prm, m_lump,   &
!!     &          nod_comm, node, ele, iphys_ele, ele_fld,              &
!!     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
!!
!!      subroutine cal_multi_pass_4_vector_ff(iele_fsmp_stack,          &
!!     &          FEM_prm, m_lump, nod_comm, node, ele, g_FEM, jac_3d,  &
!!     &          rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
!!      subroutine cal_multi_pass_4_scalar_ff(iele_fsmp_stack,          &
!!     &          FEM_prm, m_lump, nod_comm, node, ele, g_FEM, jac_3d,  &
!!     &          rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      module cal_multi_pass
!
      use m_precision
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use int_multi_pass
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_t_evo_4_vector                                     &
     &         (iflag_4_supg, iele_fsmp_stack, dt, FEM_prm, m_lump,     &
     &          nod_comm, node, ele, iphys_ele, ele_fld,                &
     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
!
      if(FEM_prm%num_multi_pass .gt. 1) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node%max_nod_smp, node%istack_nod_smp,                      &
     &      n_vector, f_nl%ff_smp, ff_m_smp)
!
        if (iflag_4_supg .eq. id_turn_ON) then
          call int_multi_pass_vector_upw                                &
     &       (iele_fsmp_stack, iphys_ele%i_velo, dt, FEM_prm, m_lump,   &
     &        nod_comm, node, ele, ele_fld, g_FEM, jac_3d, rhs_tbl,     &
     &        ff_m_smp, fem_wk, f_nl)
        else if (iflag_4_supg .eq. id_magnetic_SUPG) then
          call int_multi_pass_vector_upw                                &
     &       (iele_fsmp_stack, iphys_ele%i_magne, dt, FEM_prm, m_lump,  &
     &        nod_comm, node, ele, ele_fld, g_FEM, jac_3d, rhs_tbl,     &
     &        ff_m_smp, fem_wk, f_nl)
        else
          call int_multi_pass_vector(iele_fsmp_stack, FEM_prm, m_lump,  &
     &        nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,              &
     &        ff_m_smp, fem_wk, f_nl)
        end if
      end if
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      end subroutine cal_t_evo_4_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_t_evo_4_scalar                                     &
     &         (iflag_4_supg, iele_fsmp_stack, dt, FEM_prm, m_lump,     &
     &          nod_comm, node, ele, iphys_ele, ele_fld,                &
     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
!
      if(FEM_prm%num_multi_pass .gt. 1) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node%max_nod_smp, node%istack_nod_smp,                      &
     &      n_scalar, f_nl%ff_smp, ff_m_smp)
!
        if (iflag_4_supg .eq. id_turn_ON) then
          call int_multi_pass_scalar_upw                                &
     &        (iele_fsmp_stack, iphys_ele%i_velo, dt, FEM_prm, m_lump,  &
     &         nod_comm, node, ele, ele_fld, g_FEM, jac_3d, rhs_tbl,    &
     &         ff_m_smp, fem_wk, f_nl)
        else if (iflag_4_supg .eq. id_magnetic_SUPG) then
          call int_multi_pass_scalar_upw                                &
     &        (iele_fsmp_stack, iphys_ele%i_magne, dt, FEM_prm, m_lump, &
     &         nod_comm, node, ele, ele_fld, g_FEM, jac_3d, rhs_tbl,    &
     &         ff_m_smp, fem_wk, f_nl)
        else
          call int_multi_pass_scalar(iele_fsmp_stack, FEM_prm, m_lump,  &
     &        nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,              &
     &        ff_m_smp, fem_wk, f_nl)
        end if
!
      end if
!
      call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
!
      end subroutine cal_t_evo_4_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_t_evo_4_vector_cd                                  &
     &         (iflag_4_supg, iele_fsmp_stack, dt, FEM_prm, m_lump,     &
     &          nod_comm, node, ele, iphys_ele, ele_fld,                &
     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
!
      if(FEM_prm%num_multi_pass .gt. 1) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node%max_nod_smp, node%istack_nod_smp,                      &
     &      n_vector, f_nl%ff_smp, ff_m_smp)
!
        if (iflag_4_supg .gt. id_turn_OFF) then
          call int_multi_pass_vector_upw                                &
     &       (iele_fsmp_stack, iphys_ele%i_magne, dt, FEM_prm, m_lump,  &
     &        nod_comm, node, ele, ele_fld, g_FEM, jac_3d, rhs_tbl,     &
     &        ff_m_smp, fem_wk, f_nl)
        else
          call int_multi_pass_vector(iele_fsmp_stack, FEM_prm, m_lump,  &
     &        nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,              &
     &        ff_m_smp, fem_wk, f_nl)
        end if
!
      end if
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
!
      end subroutine cal_t_evo_4_vector_cd
!
! ----------------------------------------------------------------------
!
      subroutine cal_t_evo_4_scalar_cd                                  &
     &         (iflag_4_supg, iele_fsmp_stack, dt, FEM_prm, m_lump,     &
     &          nod_comm, node, ele, iphys_ele, ele_fld,                &
     &          g_FEM, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
!
      if(FEM_prm%num_multi_pass .gt. 1) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node%max_nod_smp, node%istack_nod_smp,                      &
     &      n_scalar, f_nl%ff_smp, ff_m_smp)
!
        if (iflag_4_supg .gt. id_turn_OFF) then
          call int_multi_pass_scalar_upw                                &
     &       (iele_fsmp_stack, iphys_ele%i_magne, dt, FEM_prm, m_lump,  &
     &        nod_comm, node, ele, ele_fld, g_FEM, jac_3d, rhs_tbl,     &
     &        ff_m_smp, fem_wk, f_nl)
        else
          call int_multi_pass_scalar(iele_fsmp_stack, FEM_prm, m_lump,  &
     &        nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,              &
     &        ff_m_smp, fem_wk, f_nl)
        end if
!
      end if
!
      call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
!
      end subroutine cal_t_evo_4_scalar_cd
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_4_vector_ff(iele_fsmp_stack,            &
     &          FEM_prm, m_lump, nod_comm, node, ele, g_FEM, jac_3d,    &
     &          rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
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
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
!
      if(FEM_prm%num_multi_pass .gt. 1) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node%max_nod_smp, node%istack_nod_smp,                      &
     &      n_vector, f_nl%ff_smp, ff_m_smp)
        call int_multi_pass_vector(iele_fsmp_stack, FEM_prm, m_lump,    &
     &      nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,                &
     &      ff_m_smp, fem_wk, f_nl)
      end if
!
      call reset_ff(node%numnod, f_l)
      call cal_ff_smp_2_ff(node, rhs_tbl, n_vector,                     &
     &    f_nl%ff_smp, f_l%ff)
!
      end subroutine cal_multi_pass_4_vector_ff
!
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_4_scalar_ff(iele_fsmp_stack,            &
     &          FEM_prm, m_lump, nod_comm, node, ele, g_FEM, jac_3d,    &
     &          rhs_tbl, ff_m_smp, fem_wk, f_l, f_nl)
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
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
!
      if(FEM_prm%num_multi_pass .gt. 1) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node%max_nod_smp, node%istack_nod_smp,                      &
     &      n_scalar, f_nl%ff_smp, ff_m_smp)
        call int_multi_pass_scalar(iele_fsmp_stack, FEM_prm, m_lump,    &
     &      nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,                &
     &      ff_m_smp, fem_wk, f_nl)
      end if
!
      call reset_ff(node%numnod, f_l)
      call cal_ff_smp_2_ff(node, rhs_tbl, n_scalar,                     &
     &    f_nl%ff_smp, f_l%ff)
!
      end subroutine cal_multi_pass_4_scalar_ff
!
! ----------------------------------------------------------------------
!
      end module cal_multi_pass
