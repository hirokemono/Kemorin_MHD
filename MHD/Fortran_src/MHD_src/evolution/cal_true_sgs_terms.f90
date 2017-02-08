!
!      module cal_true_sgs_terms
!
!      Written by H. Matsui on Oct., 2005
!
!!      subroutine cal_true_sgs_terms_pre                               &
!!     &         (nod_comm, node, ele, surf, sf_grp, fluid, conduct,    &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop,                   &
!!     &          nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD,          &
!!     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,               &
!!     &          ifld_diff, diff_coefs, mhd_fem_wk, fem_wk, surf_wk,   &
!!     &          f_l, f_nl, nod_fld, ele_fld)
!!      subroutine cal_true_sgs_terms_post(filter_param,                &
!!     &          nod_comm, node, iphys, filtering, wk_filter, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!
      module cal_true_sgs_terms
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
!
      use t_SGS_control_parameter
      use t_physical_property
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
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_bc_data_MHD
      use t_MHD_boundary_data
      use t_material_property
      use t_SGS_model_coefs
!
      use cal_fluxes
      use copy_nodal_fields
!
      use cal_filtering_scalars
!
      implicit none
!
      private :: cal_div_sgs_s_flux_true_pre
      private :: cal_div_sgs_m_flux_true_pre
      private :: cal_div_sgs_maxwell_true_pre
      private :: cal_div_sgs_induct_true_pre
      private :: cal_div_sgs_s_flux_true_post
      private :: cal_div_sgs_tensor_true_post
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_true_sgs_terms_pre                                 &
     &         (nod_comm, node, ele, surf, sf_grp, fluid, conduct,      &
     &          fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          nod_bcs, surf_bcs, iphys, iphys_ele, ak_MHD,            &
     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                 &
     &          ifld_diff, diff_coefs, mhd_fem_wk, fem_wk, surf_wk,     &
     &          f_l, f_nl, nod_fld, ele_fld)
!
      use calypso_mpi
      use m_phys_labels
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
      integer(kind = kint) :: i
!
       do i = 1, nod_fld%num_phys
         if ( nod_fld%phys_name(i) .eq. fhd_SGS_div_h_flux_true) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_s_flux_true_pre                             &
     &        (iflag_temp_supg, iphys%i_SGS_div_hf_true,                &
     &         iphys%i_h_flux, iphys%i_h_flux_div,                      &
     &         iphys%i_filter_temp, iphys%i_filter_velo,                &
     &         nod_comm, node, ele, fluid, ht_prop, nod_bcs%Tnod_bcs,   &
     &         iphys_ele, ele_fld, jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, &
     &         f_l, f_nl, nod_fld)
         else if(nod_fld%phys_name(i).eq.fhd_SGS_div_c_flux_true) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_s_flux_true_pre                             &
     &        (iflag_comp_supg, iphys%i_SGS_div_cf_true,                &
     &         iphys%i_c_flux, iphys%i_c_flux_div,                      &
     &         iphys%i_filter_comp, iphys%i_filter_velo,                &
     &         nod_comm, node, ele, fluid, cp_prop, nod_bcs%Cnod_bcs,   &
     &         iphys_ele, ele_fld, jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, &
     &         f_l, f_nl, nod_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_div_m_flux_true)     &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_m_flux_true_pre                             &
     &        (ifld_diff%i_mom_flux, ifld_diff%i_lorentz, nod_comm,     &
     &         node, ele, surf, sf_grp, fluid, fl_prop, cd_prop,        &
     &         surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs, iphys, iphys_ele,    &
     &         ak_MHD, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,          &
     &         diff_coefs, mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,      &
     &         nod_fld, ele_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_Lorentz_true) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_maxwell_true_pre                            &
     &        (ifld_diff%i_mom_flux, ifld_diff%i_lorentz, nod_comm,     &
     &         node, ele, surf, sf_grp, fluid, fl_prop, cd_prop,        &
     &         surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs, iphys, iphys_ele,    &
     &         ak_MHD, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,          &
     &         diff_coefs, mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,      &
     &         nod_fld, ele_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_mag_induct_true)     &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_induct_true_pre(ifld_diff%i_induction,      &
     &        nod_comm, node, ele, surf, sf_grp, conduct, cd_prop,      &
     &        nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,     &
     &        iphys, iphys_ele, ele_fld, ak_MHD, jac_3d, jac_sf_grp,    &
     &        rhs_tbl, FEM_elens, diff_coefs, mhd_fem_wk, fem_wk,       &
     &        surf_wk, f_l, f_nl, nod_fld)
         end if
       end do
!
      end subroutine cal_true_sgs_terms_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_true_sgs_terms_post(filter_param,                  &
     &          nod_comm, node, iphys, filtering, wk_filter, nod_fld)
!
      use m_phys_labels
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i
!
!
       do i = 1, nod_fld%num_phys
         if ( nod_fld%phys_name(i).eq.fhd_SGS_div_h_flux_true) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_s_flux_true_post(iphys%i_SGS_div_hf_true,   &
     &         iphys%i_h_flux_div, iphys%i_sgs_simi, filter_param,      &
     &         nod_comm, node, filtering, wk_filter, nod_fld)
         else if(nod_fld%phys_name(i).eq.fhd_SGS_div_c_flux_true) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_s_flux_true_post(iphys%i_SGS_div_cf_true,   &
     &         iphys%i_c_flux_div, iphys%i_sgs_simi, filter_param,      &
     &         nod_comm, node, filtering, wk_filter, nod_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_div_m_flux_true)     &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_tensor_true_post(iphys%i_SGS_div_mf_true,   &
     &         iphys%i_m_flux_div, iphys%i_sgs_simi, filter_param,      &
     &         nod_comm, node, filtering, wk_filter, nod_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_Lorentz_true) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_tensor_true_post(iphys%i_SGS_Lor_true,      &
     &         iphys%i_maxwell_div, iphys%i_sgs_simi, filter_param,     &
     &         nod_comm, node, filtering, wk_filter, nod_fld)
         else if ( nod_fld%phys_name(i).eq.fhd_SGS_mag_induct_true)     &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_tensor_true_post(iphys%i_SGS_idct_true,     &
     &         iphys%i_induct_div, iphys%i_sgs_simi, filter_param,      &
     &         nod_comm, node, filtering, wk_filter, nod_fld)
         end if
       end do
!
      end subroutine cal_true_sgs_terms_post
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_s_flux_true_pre(iflag_supg,                &
     &        i_div_flux_true, i_flux, i_div_flux, i_field_f, i_velo_f, &
     &        nod_comm, node, ele, fluid, property, Snod_bcs,           &
     &        iphys_ele, ele_fld, jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,  &
     &        f_l, f_nl, nod_fld)
!
      use t_bc_data_temp
      use t_surface_bc_data
      use products_nodal_fields_smp
      use cal_terms_for_heat
!
      integer(kind = kint), intent(in) :: iflag_supg
      integer(kind = kint), intent(in) :: i_div_flux_true
      integer(kind = kint), intent(in) :: i_flux, i_div_flux
      integer(kind = kint), intent(in) :: i_field_f, i_velo_f
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
!$omp parallel
      call cal_phys_scalar_product_vector                               &
     &   (i_velo_f, i_field_f, i_flux, nod_fld)
!$omp end parallel
      call cal_div_of_scalar_flux                                       &
     &   (i_div_flux, i_flux, iflag_supg,                               &
     &    nod_comm, node, ele, fluid, property, Snod_bcs,               &
     &    iphys_ele, ele_fld, jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,      &
     &    f_l, f_nl, nod_fld)
      call copy_scalar_component(nod_fld,                               &
     &    i_div_flux, i_div_flux_true)
!
      end subroutine cal_div_sgs_s_flux_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_m_flux_true_pre                            &
     &         (iak_diff_mf, iak_diff_lor, nod_comm, node, ele,         &
     &          surf, sf_grp, fluid, fl_prop, cd_prop,                  &
     &          Vsf_bcs, Bsf_bcs, iphys, iphys_ele, ak_MHD,             &
     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,     &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                 &
     &          nod_fld, ele_fld)
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
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
!
      call cal_flux_tensor                                              &
     &   (iphys%i_filter_velo, iphys%i_filter_velo, iphys%i_m_flux,     &
     &    nod_fld)
      call cal_terms_4_momentum                                         &
     &   (iphys%i_m_flux_div, iak_diff_mf, iak_diff_lor,                &
     &    SGS_par1%model_p, SGS_par1%commute_p,                         &
     &    nod_comm, node, ele, surf, sf_grp,                            &
     &    fluid, fl_prop, cd_prop, Vsf_bcs, Bsf_bcs, iphys, iphys_ele,  &
     &    ak_MHD, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,   &
     &    mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld, ele_fld)
      call copy_vector_component(nod_fld,                               &
     &    iphys%i_m_flux_div, iphys%i_SGS_div_mf_true)
!
      end subroutine cal_div_sgs_m_flux_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_maxwell_true_pre                           &
     &        (iak_diff_mf, iak_diff_lor, nod_comm,                     &
     &         node, ele, surf, sf_grp, fluid, fl_prop, cd_prop,        &
     &         Vsf_bcs, Bsf_bcs, iphys, iphys_ele, ak_MHD,              &
     &         jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,      &
     &         mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                  &
     &         nod_fld, ele_fld)
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
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
!
      call cal_maxwell_tensor(cd_prop%ex_magne,                         &
     &    iphys%i_filter_magne, iphys%i_maxwell, nod_fld)
      call cal_terms_4_momentum                                         &
     &   (iphys%i_maxwell_div, iak_diff_mf, iak_diff_lor,               &
     &    SGS_par1%model_p, SGS_par1%commute_p,                         &
     &    nod_comm, node, ele, surf, sf_grp,                            &
     &    fluid, fl_prop, cd_prop, Vsf_bcs, Bsf_bcs, iphys, iphys_ele,  &
     &    ak_MHD, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,   &
     &    mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld, ele_fld)
      call copy_vector_component(nod_fld,                               &
     &   iphys%i_maxwell_div, iphys%i_SGS_Lor_true)
!
      end subroutine cal_div_sgs_maxwell_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_induct_true_pre(iak_diff_uxb,              &
     &         nod_comm, node, ele, surf, sf_grp, conduct, cd_prop,     &
     &         Bnod_bcs, Asf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld,   &
     &         ak_MHD, jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,          &
     &         diff_coefs, mhd_fem_wk, fem_wk, surf_wk,                 &
     &         f_l, f_nl, nod_fld)
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
      type(conductive_property), intent(in) :: cd_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_induction_tensor                                         &
     &   (iphys%i_filter_magne, iphys%i_filter_velo, iphys%i_induct_t,  &
     &    nod_fld)
      call cal_terms_4_magnetic                                         &
     &   (iphys%i_induct_div, iak_diff_uxb, ak_MHD%ak_d_magne,          &
     &    SGS_par1%model_p, SGS_par1%commute_p,                         &
     &    nod_comm, node, ele, surf, conduct, sf_grp, cd_prop,          &
     &    Bnod_bcs, Asf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld,        &
     &    jac_3d, jac_sf_grp, rhs_tbl, FEM_elens, diff_coefs,           &
     &    mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
      call copy_vector_component(nod_fld,                               &
     &    iphys%i_induct_div, iphys%i_SGS_idct_true)
!
      end subroutine cal_div_sgs_induct_true_pre
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_s_flux_true_post                           &
     &          (i_div_flux_true, i_div_flux, i_sgs_simi,               &
     &           filter_param, nod_comm, node, filtering,               &
     &           wk_filter, nod_fld)
!
      integer(kind = kint), intent(in) :: i_div_flux_true
      integer(kind = kint), intent(in) :: i_div_flux, i_sgs_simi
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_scalar_component(nod_fld,                               &
     &    i_div_flux_true, i_sgs_simi)
      call cal_filtered_scalar_whole                                    &
     &   (filter_param, nod_comm, node, filtering,                      &
     &    i_div_flux_true, i_div_flux, wk_filter, nod_fld)
      call subtract_2_nod_scalars(nod_fld,                              &
     &    i_div_flux_true, i_sgs_simi, i_div_flux_true)
!
      end subroutine cal_div_sgs_s_flux_true_post
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_tensor_true_post                           &
     &         (i_sgs_true, i_sgs_div, i_sgs_simi,                      &
     &          filter_param, nod_comm, node, filtering,                &
     &          wk_filter, nod_fld)
!
      integer(kind = kint), intent(in) :: i_sgs_true, i_sgs_div
      integer(kind = kint), intent(in) :: i_sgs_simi
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_vector_component(nod_fld, i_sgs_true, i_sgs_simi)
      call cal_filtered_vector_whole                                    &
     &   (filter_param, nod_comm, node, filtering,                      &
     &    i_sgs_true, i_sgs_div, wk_filter, nod_fld)
      call subtract_2_nod_vectors(nod_fld,                              &
     &    i_sgs_true, i_sgs_simi, i_sgs_true)
!
      end subroutine cal_div_sgs_tensor_true_post
!
!-----------------------------------------------------------------------
!
      end module cal_true_sgs_terms
