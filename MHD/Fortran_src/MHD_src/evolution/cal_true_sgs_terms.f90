!
!      module cal_true_sgs_terms
!
!      Written by H. Matsui on Oct., 2005
!
!!      subroutine cal_true_sgs_terms_pre                               &
!!     &        (nod_comm, node, ele, surf, sf_grp, fluid, conduct,     &
!!     &         Vsf_bcs, iphys, iphys_ele, ele_fld,                    &
!!     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                &
!!     &         mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_true_sgs_terms_post
!!     &         (nod_comm, node, iphys, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_true_sgs_terms
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
!
      use cal_fluxes
      use copy_nodal_fields
!
      use cal_filtering_scalars
      use cal_filtering_vectors
!
      implicit none
!
      private :: cal_div_sgs_h_flux_true_pre
      private :: cal_div_sgs_m_flux_true_pre
      private :: cal_div_sgs_maxwell_true_pre
      private :: cal_div_sgs_induct_true_pre
      private :: cal_div_sgs_h_flux_true_post
      private :: cal_div_sgs_tensor_true_post
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_true_sgs_terms_pre                                 &
     &        (nod_comm, node, ele, surf, sf_grp, fluid, conduct,       &
     &         Vsf_bcs, iphys, iphys_ele, ele_fld,                      &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                  &
     &         mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use calypso_mpi
      use m_phys_labels
      use m_SGS_address
      use m_physical_property
      use m_bc_data_ene
      use m_bc_data_magne
      use m_surf_data_temp
      use m_surf_data_magne
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i
!
       do i = 1, nod_fld%num_phys
         if ( nod_fld%phys_name(i).eq.fhd_SGS_div_h_flux_true) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_h_flux_true_pre(iak_diff_hf,                &
     &         nod_comm, node, ele, surf, sf_grp, fluid,                &
     &         Tnod1_bcs, Tsf1_bcs, iphys, iphys_ele, ele_fld,          &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                  &
     &         mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_div_m_flux_true)     &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_m_flux_true_pre(iak_diff_mf, iak_diff_lor,  &
     &         nod_comm, node, ele, surf, fluid, sf_grp,                &
     &         Vsf_bcs, Bsf1_bcs, iphys, iphys_ele, ele_fld,            &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, mhd_fem_wk,      &
     &         fem_wk, f_l, f_nl, nod_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_Lorentz_true) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_maxwell_true_pre                            &
     &        (iak_diff_mf, iak_diff_lor, ex_magne,                     &
     &         nod_comm, node, ele, surf, fluid, sf_grp,                &
     &         Vsf_bcs, Bsf1_bcs, iphys, iphys_ele, ele_fld,            &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, mhd_fem_wk,      &
     &         fem_wk, f_l, f_nl, nod_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_mag_induct_true)     &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_induct_true_pre(iak_diff_uxb,               &
     &        nod_comm, node, ele, surf, conduct, sf_grp,               &
     &        Bnod1_bcs, Asf1_bcs, Bsf1_bcs, iphys, iphys_ele, ele_fld, &
     &        jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                   &
     &        mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
         end if
       end do
!
      end subroutine cal_true_sgs_terms_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_true_sgs_terms_post                                &
     &         (nod_comm, node, iphys, nod_fld)
!
      use m_phys_labels
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i
!
!
       do i = 1, nod_fld%num_phys
         if ( nod_fld%phys_name(i).eq.fhd_SGS_div_h_flux_true) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_h_flux_true_post                            &
     &        (nod_comm, node, iphys, nod_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_div_m_flux_true)     &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_tensor_true_post(iphys%i_SGS_div_mf_true,   &
     &         iphys%i_m_flux_div, iphys%i_sgs_simi,                    &
     &         nod_comm, node, nod_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_Lorentz_true) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_tensor_true_post(iphys%i_SGS_Lor_true,      &
     &         iphys%i_maxwell_div, iphys%i_sgs_simi,                   &
     &         nod_comm, node, nod_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_mag_induct_true)     &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_tensor_true_post(iphys%i_SGS_idct_true,     &
     &         iphys%i_induct_div, iphys%i_sgs_simi,                    &
     &         nod_comm, node, nod_fld)
         end if
       end do
!
      end subroutine cal_true_sgs_terms_post
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_h_flux_true_pre(iak_diff_hf,               &
     &         nod_comm, node, ele, surf, sf_grp, fluid,                &
     &         Tnod_bcs, Tsf_bcs, iphys, iphys_ele, ele_fld,            &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                  &
     &         mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use t_bc_data_temp
      use t_surface_bc_data
      use cal_terms_for_heat
!
      integer(kind=kint), intent(in) :: iak_diff_hf
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: Tnod_bcs
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_flux_vector(node, nod_fld%ntot_phys,                     &
     &    iphys%i_filter_velo, iphys%i_filter_temp, iphys%i_h_flux,     &
     &    nod_fld%d_fld)
      call cal_terms_4_heat(iphys%i_h_flux_div, iak_diff_hf,            &
     &    nod_comm, node, ele, surf, fluid, sf_grp, Tnod_bcs, Tsf_bcs,  &
     &    iphys, iphys_ele, ele_fld, jac_3d, jac_sf_grp,                &
     &    rhs_tbl, FEM_elens,  mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      call copy_scalar_component(node, nod_fld,                         &
     &    iphys%i_h_flux_div, iphys%i_SGS_div_hf_true)
!
      end subroutine cal_div_sgs_h_flux_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_m_flux_true_pre(iak_diff_mf, iak_diff_lor, &
     &         nod_comm, node, ele, surf, fluid, sf_grp,                &
     &         Vsf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld,             &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, mhd_fem_wk,      &
     &         fem_wk, f_l, f_nl, nod_fld)
!
      use cal_momentum_terms
!
      integer(kind = kint), intent(in) :: iak_diff_mf, iak_diff_lor
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_flux_tensor(node, nod_fld%ntot_phys,                     &
     &    iphys%i_filter_velo, iphys%i_filter_velo, iphys%i_m_flux,     &
     &    nod_fld%d_fld)
      call cal_terms_4_momentum                                         &
     &   (iphys%i_m_flux_div, iak_diff_mf, iak_diff_lor,                &
     &    nod_comm, node, ele, surf, fluid, sf_grp, Vsf_bcs, Bsf_bcs,   &
     &    iphys, iphys_ele, ele_fld, jac_3d, jac_sf_grp,                &
     &    rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      call copy_vector_component(node, nod_fld,                         &
     &    iphys%i_m_flux_div, iphys%i_SGS_div_mf_true)
!
      end subroutine cal_div_sgs_m_flux_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_maxwell_true_pre                           &
     &        (iak_diff_mf, iak_diff_lor, ex_magne,                     &
     &         nod_comm, node, ele, surf, fluid, sf_grp,                &
     &         Vsf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld,             &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, mhd_fem_wk,      &
     &         fem_wk, f_l, f_nl, nod_fld)
!
      use cal_momentum_terms
!
      integer(kind = kint), intent(in) :: iak_diff_mf, iak_diff_lor
      real(kind = kreal), intent(in) :: ex_magne(3)
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_maxwell_tensor(node, ex_magne, nod_fld%ntot_phys,        &
     &    iphys%i_filter_magne, iphys%i_maxwell, nod_fld%d_fld)
      call cal_terms_4_momentum                                         &
     &   (iphys%i_maxwell_div, iak_diff_mf, iak_diff_lor,               &
     &    nod_comm, node, ele, surf, fluid, sf_grp, Vsf_bcs, Bsf_bcs,   &
     &    iphys, iphys_ele, ele_fld, jac_3d, jac_sf_grp,                &
     &    rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      call copy_vector_component(node, nod_fld,                         &
     &   iphys%i_maxwell_div, iphys%i_SGS_Lor_true)
!
      end subroutine cal_div_sgs_maxwell_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_induct_true_pre(iak_diff_uxb,              &
     &         nod_comm, node, ele, surf, conduct, sf_grp,              &
     &         Bnod_bcs, Asf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld,   &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                  &
     &         mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use t_bc_data_magne
      use t_surface_bc_data
      use cal_magnetic_terms
!
      integer(kind = kint), intent(in) :: iak_diff_uxb
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: conduct
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_induction_tensor(node, nod_fld%ntot_phys,                &
     &    iphys%i_filter_magne, iphys%i_filter_velo, iphys%i_induct_t,  &
     &    nod_fld%d_fld)
      call cal_terms_4_magnetic(iphys%i_induct_div, iak_diff_uxb,       &
     &    nod_comm, node, ele, surf, conduct, sf_grp,                   &
     &    Bnod_bcs, Asf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld,        &
     &    jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk,   &
     &    f_l, f_nl, nod_fld)
      call copy_vector_component(node, nod_fld,                         &
     &    iphys%i_induct_div, iphys%i_SGS_idct_true)
!
      end subroutine cal_div_sgs_induct_true_pre
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_h_flux_true_post                           &
     &          (nod_comm, node, iphys, nod_fld)
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_scalar_component(node, nod_fld,                         &
     &    iphys%i_SGS_div_hf_true, iphys%i_sgs_simi)
      call cal_filtered_scalar(nod_comm, node,                          &
     &    iphys%i_SGS_div_hf_true, iphys%i_h_flux_div, nod_fld)
      call subtract_2_nod_scalars(node, nod_fld,                        &
     &    iphys%i_SGS_div_hf_true, iphys%i_sgs_simi,                    &
     &    iphys%i_SGS_div_hf_true)
!
      end subroutine cal_div_sgs_h_flux_true_post
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_tensor_true_post                           &
     &         (i_sgs_true, i_sgs_div, i_sgs_simi,                      &
     &          nod_comm, node, nod_fld)
!
      integer(kind = kint), intent(in) :: i_sgs_true, i_sgs_div
      integer(kind = kint), intent(in) :: i_sgs_simi
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_vector_component(node, nod_fld, i_sgs_true, i_sgs_simi)
      call cal_filtered_vector(nod_comm, node,                          &
     &    i_sgs_true, i_sgs_div, nod_fld)
      call subtract_2_nod_vectors(node, nod_fld,                        &
     &    i_sgs_true, i_sgs_simi, i_sgs_true)
!
      end subroutine cal_div_sgs_tensor_true_post
!
!-----------------------------------------------------------------------
!
      end module cal_true_sgs_terms
