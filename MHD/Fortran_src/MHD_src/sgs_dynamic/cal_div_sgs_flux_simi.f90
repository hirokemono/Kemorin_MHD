!
!     module cal_div_sgs_flux_simi
!
!     Written by H. Matsui
!
!!      subroutine cal_div_sgs_mf_simi(i_sgs, i_flux, i_vect, dt,       &
!!     &          FEM_prm, nod_comm, node, ele, fluid,                  &
!!     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl,           &
!!     &          fem_wk, mhd_fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_div_sgs_sf_simi(i_sgs, i_flux, i_vect, i_scalar, &
!!     &          iflag_supg, num_int, dt,                              &
!!     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,       &
!!     &          g_FEM, jac_3d, rhs_tbl, fem_wk, mlump_fl,             &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_div_sgs_idct_simi(i_sgs, i_flux, i_v, i_b, dt,   &
!!     &          FEM_prm, nod_comm, node, ele, conduct,                &
!!     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl,           &
!!     &          fem_wk, mlump_cd, f_l, f_nl, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type (lumped_mass_matrices), intent(in) :: mlump_fl
!!        type (lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_div_sgs_flux_simi
!
      use m_precision
!
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_mf_simi(i_sgs, i_flux, i_vect, dt,         &
     &          FEM_prm, nod_comm, node, ele, fluid,                    &
     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl,             &
     &          fem_wk, mlump_fl, f_l, f_nl, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use int_div_sgs_flux
      use int_div_sgs_flux_upwind
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type (lumped_mass_matrices), intent(in) :: mlump_fl
!
      integer(kind = kint), intent(in) :: i_flux, i_vect
      integer(kind = kint), intent(in) :: i_sgs
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if (FEM_prm%iflag_velo_supg .eq. id_magnetic_SUPG) then
        call int_div_sgs_mf_simi_upwind                                 &
     &     (i_flux, i_vect, FEM_prm%npoint_t_evo_int, dt,               &
     &      node, ele, fluid, nod_fld, g_FEM, jac_3d, rhs_tbl,          &
     &      ele_fld%ntot_phys, iphys_ele%i_magne, ele_fld%d_fld,        &
     &      fem_wk, f_nl)
      else if (FEM_prm%iflag_velo_supg .eq. id_turn_ON) then
        call int_div_sgs_mf_simi_upwind                                 &
     &     (i_flux, i_vect, FEM_prm%npoint_t_evo_int, dt,               &
     &      node, ele, fluid, nod_fld, g_FEM, jac_3d, rhs_tbl,          &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      fem_wk, f_nl)
      else
        call int_div_sgs_mf_simi_pg                                     &
     &     (i_flux, i_vect, FEM_prm%npoint_t_evo_int, node, ele, fluid, &
     &      nod_fld, g_FEM, jac_3d, rhs_tbl, fem_wk, f_nl)
      end if
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp, f_nl%ff,   &
     &    mlump_fl%ml, nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_sgs, nod_comm, nod_fld)
!
      end subroutine cal_div_sgs_mf_simi
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_sf_simi(i_sgs, i_flux, i_vect, i_scalar,   &
     &          iflag_supg, num_int, dt,                                &
     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,         &
     &          g_FEM, jac_3d, rhs_tbl, fem_wk, mlump_fl,               &
     &          f_l, f_nl, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use int_div_sgs_flux
      use int_div_sgs_flux_upwind
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type (lumped_mass_matrices), intent(in) :: mlump_fl
!
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
      integer(kind = kint), intent(in) :: i_sgs
      integer(kind = kint), intent(in) :: iflag_supg, num_int
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
       call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
        if(iflag_supg .gt. id_turn_OFF) then
          call int_div_sgs_sf_simi_upw                                  &
     &       (i_flux, i_vect, i_scalar, num_int, dt,                    &
     &        node, ele, fluid, nod_fld, g_FEM, jac_3d, rhs_tbl,        &
     &        ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,       &
     &        fem_wk, f_nl)
        else
          call int_div_sgs_sf_simi_pg                                   &
     &       (i_flux, i_vect, i_scalar, num_int, node, ele, fluid,      &
     &        nod_fld, g_FEM, jac_3d, rhs_tbl, fem_wk, f_nl)
        end if
!
       call set_ff_nl_smp_2_ff(n_scalar, node, rhs_tbl, f_l, f_nl)
       call cal_ff_2_scalar(node%numnod, node%istack_nod_smp, f_nl%ff,  &
     &     mlump_fl%ml, nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call scalar_send_recv(i_sgs, nod_comm, nod_fld)
!
      end subroutine cal_div_sgs_sf_simi
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_idct_simi(i_sgs, i_flux, i_v, i_b, dt,     &
     &          FEM_prm, nod_comm, node, ele, conduct,                  &
     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl,             &
     &          fem_wk, mlump_cd, f_l, f_nl, nod_fld)
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use int_div_sgs_flux
      use int_div_sgs_flux_upwind
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type (lumped_mass_matrices), intent(in) :: mlump_cd
!
      integer(kind = kint), intent(in) :: i_flux, i_v, i_b
      integer(kind = kint), intent(in) :: i_sgs
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if(FEM_prm%iflag_magne_supg .gt. id_turn_OFF) then
        call int_div_sgs_idct_simi_upw                                  &
     &     (i_flux, i_v, i_b, FEM_prm%npoint_t_evo_int, dt,             &
     &      node, ele, conduct, nod_fld, g_FEM, jac_3d, rhs_tbl,        &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      fem_wk, f_nl)
      else
        call int_div_sgs_idct_simi_pg                                   &
     &     (i_flux, i_v, i_b, FEM_prm%npoint_t_evo_int, node, ele,      &
     &      conduct, nod_fld, g_FEM, jac_3d, rhs_tbl, fem_wk, f_nl)
      end if
!
      call set_ff_nl_smp_2_ff(n_vector, node, rhs_tbl, f_l, f_nl)
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp, f_nl%ff,   &
     &    mlump_cd%ml, nod_fld%ntot_phys, i_sgs, nod_fld%d_fld)
!
! ----------   communications
!
      call vector_send_recv(i_sgs, nod_comm, nod_fld)
!
      end subroutine cal_div_sgs_idct_simi
!
!-----------------------------------------------------------------------
!
      end module cal_div_sgs_flux_simi
