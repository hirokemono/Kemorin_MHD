!
!     module cal_momentum_terms
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine cal_terms_4_momentum                                 &
!!     &       (i_field, iak_diff_mf, iak_diff_lor, dt,                 &
!!     &        FEM_prm, SGS_param, cmt_param, nod_comm, node, ele,     &
!!     &        surf, sf_grp, fluid, fl_prop, cd_prop, Vsf_bcs, Bsf_bcs,&
!!     &        iphys, iphys_ele, ak_MHD, fem_int, FEM_elens,           &
!!     &        diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat,              &
!!     &        nod_fld, ele_fld)
!!      subroutine cal_viscous_diffusion                                &
!!     &         (iak_diff_v, iak_diff_mf, iak_diff_lor,                &
!!     &          FEM_prm, SGS_param, cmt_param, nod_comm, node, ele,   &
!!     &          surf, sf_grp, fluid, fl_prop, Vnod_bcs, Vsf_bcs,      &
!!     &          Bsf_bcs, iphys, ak_MHD, fem_int, FEM_elens,           &
!!     &          diff_coefs, mlump_fl, rhs_mat, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!
      module cal_momentum_terms
!
      use m_precision
!
      use m_phys_constants
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
      use t_filter_elength
      use t_material_property
      use t_bc_data_velo
      use t_surface_bc_data
      use t_MHD_finite_element_mat
      use t_work_FEM_integration
!
      use cal_multi_pass
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use set_nodal_bc_id_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_momentum                                   &
     &       (i_field, iak_diff_mf, iak_diff_lor, dt,                   &
     &        FEM_prm, SGS_param, cmt_param, nod_comm, node, ele,       &
     &        surf, sf_grp, fluid, fl_prop, cd_prop, Vsf_bcs, Bsf_bcs,  &
     &        iphys, iphys_ele, ak_MHD, fem_int, FEM_elens,             &
     &        diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat,                &
     &        nod_fld, ele_fld)
!
      use int_vol_velo_monitor
      use int_surf_velo_pre
!
      integer (kind=kint), intent(in) :: i_field
      integer(kind= kint), intent(in) :: iak_diff_mf, iak_diff_lor
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
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
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
!
      call reset_ff_smps(node%max_nod_smp, rhs_mat%f_l, rhs_mat%f_nl)
!
      if(FEM_prm%iflag_velo_supg .eq. id_turn_ON) then
        call int_vol_velo_monitor_upwind                                &
     &     (i_field, iak_diff_mf, iak_diff_lor, iphys_ele%i_velo, dt,   &
     &      FEM_prm, SGS_param, cmt_param, node, ele, fluid,            &
     &      fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ak_MHD,        &
     &      fem_int%jcs%jac_3d, fem_int%rhs_tbl,                        &
     &      FEM_elens, diff_coefs, mhd_fem_wk,                          &
     &      rhs_mat%fem_wk, rhs_mat%f_nl, ele_fld)
      else if (FEM_prm%iflag_velo_supg .eq. id_magnetic_SUPG) then
        call int_vol_velo_monitor_upwind                                &
     &     (i_field, iak_diff_mf, iak_diff_lor, iphys_ele%i_magne, dt,  &
     &      FEM_prm, SGS_param, cmt_param, node, ele, fluid,            &
     &      fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ak_MHD,        &
     &      fem_int%jcs%jac_3d, fem_int%rhs_tbl,                        &
     &      FEM_elens, diff_coefs, mhd_fem_wk,                          &
     &      rhs_mat%fem_wk, rhs_mat%f_nl, ele_fld)
      else
       call int_vol_velo_monitor_pg(i_field, iak_diff_mf, iak_diff_lor, &
     &     FEM_prm, SGS_param, cmt_param, node, ele, fluid,             &
     &     fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ak_MHD,         &
     &     fem_int%jcs%jac_3d, fem_int%rhs_tbl,                         &
     &     FEM_elens, diff_coefs, mhd_fem_wk,                           &
     &     rhs_mat%fem_wk, rhs_mat%f_nl, ele_fld)
      end if
!
      call int_surf_velo_monitor(i_field, iak_diff_mf, iak_diff_lor,    &
     &    ak_MHD%ak_d_velo, FEM_prm%npoint_t_evo_int,                   &
     &    SGS_param, cmt_param, node, ele, surf, sf_grp, fl_prop,       &
     &    Vsf_bcs, Bsf_bcs, iphys, nod_fld,                             &
     &    fem_int%jcs%jac_sf_grp, fem_int%rhs_tbl,                      &
     &    FEM_elens, diff_coefs, rhs_mat%fem_wk, rhs_mat%surf_wk,       &
     &    rhs_mat%f_l, rhs_mat%f_nl)
!
      call cal_t_evo_4_vector                                           &
     &   (FEM_prm%iflag_velo_supg, fluid%istack_ele_fld_smp, dt,        &
     &    FEM_prm, mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld,   &
     &    fem_int%jcs%jac_3d, fem_int%rhs_tbl,                          &
     &    mhd_fem_wk%ff_m_smp, rhs_mat%fem_wk,                          &
     &    rhs_mat%f_l, rhs_mat%f_nl)
!       call set_boundary_velo_4_rhs                                    &
!     &    (node, Vnod_bcs, rhs_mat%f_l, rhs_mat%f_nl)
!
      call cal_ff_2_vector                                              &
     &   (node%numnod, node%istack_nod_smp, rhs_mat%f_nl%ff,            &
     &    mlump_fl%ml, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
      call vector_send_recv(i_field, nod_comm, nod_fld)
!
      end subroutine cal_terms_4_momentum
!
!-----------------------------------------------------------------------
!
      subroutine cal_viscous_diffusion                                  &
     &         (iak_diff_v, iak_diff_mf, iak_diff_lor,                  &
     &          FEM_prm, SGS_param, cmt_param, nod_comm, node, ele,     &
     &          surf, sf_grp, fluid, fl_prop, Vnod_bcs, Vsf_bcs,        &
     &          Bsf_bcs, iphys, ak_MHD, fem_int, FEM_elens,             &
     &          diff_coefs, mlump_fl, rhs_mat, nod_fld)
!
      use int_vol_diffusion_ele
      use int_surf_velo_pre
!
      integer (kind=kint), intent(in) :: iak_diff_v
      integer(kind= kint), intent(in) :: iak_diff_mf, iak_diff_lor
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: iphys
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, rhs_mat%f_l, rhs_mat%f_nl)
!
      call int_vol_vector_diffuse_ele                                   &
     &   (SGS_param%ifilter_final, fluid%istack_ele_fld_smp,            &
     &    FEM_prm%npoint_t_evo_int, node, ele, nod_fld,                 &
     &    fem_int%jcs%jac_3d, fem_int%rhs_tbl, FEM_elens,               &
     &    diff_coefs, iak_diff_v, one, ak_MHD%ak_d_velo, iphys%i_velo,  &
     &    rhs_mat%fem_wk, rhs_mat%f_l)
!
      call int_surf_velo_monitor                                        &
     &  (iphys%i_v_diffuse, iak_diff_mf, iak_diff_lor,                  &
     &   ak_MHD%ak_d_velo, FEM_prm%npoint_t_evo_int,                    &
     &   SGS_param, cmt_param, node, ele, surf,                         &
     &   sf_grp, fl_prop, Vsf_bcs, Bsf_bcs, iphys, nod_fld,             &
     &   fem_int%jcs%jac_sf_grp, fem_int%rhs_tbl,                       &
     &   FEM_elens, diff_coefs, rhs_mat%fem_wk, rhs_mat%surf_wk,        &
     &   rhs_mat%f_l, rhs_mat%f_nl)
!
      call set_ff_nl_smp_2_ff                                           &
     &   (n_vector, node, fem_int%rhs_tbl, rhs_mat%f_l, rhs_mat%f_nl)
!
      call set_boundary_velo_4_rhs                                      &
     &   (node, Vnod_bcs, rhs_mat%f_l, rhs_mat%f_nl)
!
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    rhs_mat%f_l%ff, mlump_fl%ml, nod_fld%ntot_phys,               &
     &    iphys%i_v_diffuse, nod_fld%d_fld)
!
      call vector_send_recv(iphys%i_v_diffuse, nod_comm, nod_fld)
!
      end subroutine cal_viscous_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_momentum_terms
