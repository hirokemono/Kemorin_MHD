!
!      module cal_sgs_fluxes
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_heat_flux(iflag_supg, num_int, dt,           &
!!     &          iflag_SGS_flux, itype_Csym_flux,                      &
!!     &          ifleld, ifield_f, ivelo, ivelo_f, i_sgs,              &
!!     &          icomp_sgs_flux, ie_dvx, SGS_param, filter_param,      &
!!     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,       &
!!     &          jac_3d, rhs_tbl, FEM_elens, filtering, sgs_coefs,     &
!!     &          sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,         &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_sgs_momentum_flux(icomp_sgs_mf, ie_dvx, dt,      &
!!     &          FEM_prm, SGS_param, filter_param,                     &
!!     &          nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,&
!!     &          jac_3d, rhs_tbl, FEM_elens, filtering, sgs_coefs,     &
!!     &          sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,         &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_sgs_maxwell(icomp_sgs_lor, ie_dbx, dt,           &
!!     &          FEM_prm, SGS_param, filter_param,                     &
!!     &          nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,&
!!     &          jac_3d, rhs_tbl, FEM_elens, filtering, sgs_coefs,     &
!!     &          sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,         &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_sgs_magne_induction                              &
!!     &         (icomp_sgs_uxb, ie_dvx, ie_dbx, dt,                    &
!!     &          FEM_prm, SGS_param, filter_param, nod_comm, node, ele,&
!!     &          conduct, cd_prop, iphys, iphys_ele, ele_fld,          &
!!     &          jac_3d, rhs_tbl,  FEM_elens, filtering,               &
!!     &          sgs_coefs, sgs_coefs_nod, wk_filter, mhd_fem_wk,      &
!!     &          fem_wk, f_l, nod_fld)
!!      subroutine cal_sgs_uxb_2_evo(icomp_sgs_uxb, ie_dvx, dt,         &
!!     &          FEM_prm, SGS_param, filter_param, nod_comm, node, ele,&
!!     &          conduct, cd_prop, iphys, iphys_ele, ele_fld, jac_3d,  &
!!     &          rhs_tbl, FEM_elens, filtering, sgs_coefs, wk_filter,  &
!!     &          mhd_fem_wk, fem_wk, f_nl, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(SGS_filtering_params), intent(in) :: filter_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_sgs_fluxes
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_material_property
      use t_SGS_model_coefs
!
      use cal_sgs_fluxes_simi
!
      implicit none
!
      private :: cal_sgs_m_flux_diffuse, const_viscosity_tensor
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_heat_flux(iflag_supg, num_int, dt,             &
     &          iflag_SGS_flux, itype_Csym_flux,                        &
     &          ifleld, ifield_f, ivelo, ivelo_f, i_sgs,                &
     &          icomp_sgs_flux, ie_dvx, SGS_param, filter_param,        &
     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,         &
     &          jac_3d, rhs_tbl, FEM_elens, filtering, sgs_coefs,       &
     &          sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,           &
     &          f_l, f_nl, nod_fld)
!
      use cal_sgs_heat_fluxes_grad
      use cal_gradient
!
      integer(kind = kint), intent(in) :: iflag_supg, num_int
      integer(kind = kint), intent(in) :: iflag_SGS_flux
      integer(kind = kint), intent(in) :: itype_Csym_flux
      integer(kind = kint), intent(in) :: ifleld, ifield_f
      integer(kind = kint), intent(in) :: i_sgs, ivelo, ivelo_f
      integer(kind = kint), intent(in) :: icomp_sgs_flux, ie_dvx
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(iflag_SGS_flux .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_s_flux_grad_w_coef', &
     &                       SGS_param%ifilter_final
      call cal_sgs_s_flux_grad_w_coef                                   &
     &   (iflag_supg, num_int, dt, itype_Csym_flux,                     &
     &    SGS_param%icoord_Csim, SGS_param%ifilter_final,               &
     &    icomp_sgs_flux, i_sgs, ifleld, ie_dvx,                        &
     &    nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d,       &
     &    rhs_tbl, FEM_elens, sgs_coefs, mhd_fem_wk%mlump_fl,           &
     &    mhd_fem_wk, fem_wk, f_l, nod_fld)
!
      else if(iflag_SGS_flux .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1) write(*,*) 'cal_sgs_sf_simi'
        call cal_sgs_sf_simi                                            &
     &     (i_sgs, ifleld, ifield_f, ivelo, ivelo_f, icomp_sgs_flux,    &
     &      filter_param, nod_comm, node, filtering, sgs_coefs_nod,     &
     &      wk_filter, nod_fld)
!
      else if(iflag_SGS_flux .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1) write(*,*) 'cal_sgs_h_flux_diffuse'
        call choose_cal_gradient_w_const                                &
     &     (iflag_supg, num_int, dt, ifleld, i_sgs, dminus,             &
     &      fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,              &
     &      nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,            &
     &      rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine cal_sgs_heat_flux
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_momentum_flux(icomp_sgs_mf, ie_dvx, dt,        &
     &          FEM_prm, SGS_param, filter_param,                       &
     &          nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,  &
     &          jac_3d, rhs_tbl, FEM_elens, filtering, sgs_coefs,       &
     &          sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,           &
     &          f_l, f_nl, nod_fld)
!
      use cal_sgs_mom_fluxes_grad
!
      integer(kind = kint), intent(in) :: icomp_sgs_mf
      integer(kind = kint), intent(in) :: ie_dvx
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (  SGS_param%iflag_SGS_m_flux .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_m_flux_grad', SGS_param%ifilter_final
        call cal_sgs_m_flux_grad_w_coef(SGS_param%ifilter_final,        &
     &      icomp_sgs_mf, iphys%i_SGS_m_flux, iphys%i_velo, ie_dvx, dt, &
     &      FEM_prm, SGS_param, nod_comm, node, ele, fluid,             &
     &      iphys_ele, ele_fld, jac_3d, FEM_elens, sgs_coefs, rhs_tbl,  &
     &      mhd_fem_wk%mlump_fl, fem_wk, mhd_fem_wk, nod_fld)
!
      else if (SGS_param%iflag_SGS_m_flux .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_mf_simi', iphys%i_SGS_m_flux
        call cal_sgs_mf_simi(iphys%i_SGS_m_flux, iphys%i_velo,          &
     &      iphys%i_filter_velo, icomp_sgs_mf,                          &
     &      filter_param, nod_comm, node, filtering, sgs_coefs_nod,     &
     &      wk_filter, nod_fld)
!
      else if (SGS_param%iflag_SGS_m_flux .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_m_flux_diffuse', iphys%i_SGS_m_flux
        call cal_sgs_m_flux_diffuse                                     &
     &     (iphys%i_velo, iphys%i_sgs_diffuse, iphys%i_SGS_m_flux, dt,  &
     &      FEM_prm, nod_comm, node, ele, fluid, iphys_ele, ele_fld,    &
     &      jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine cal_sgs_momentum_flux
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_maxwell(icomp_sgs_lor, ie_dbx, dt,             &
     &          FEM_prm, SGS_param, filter_param,                       &
     &          nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,  &
     &          jac_3d, rhs_tbl, FEM_elens, filtering, sgs_coefs,       &
     &          sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,           &
     &          f_l, f_nl, nod_fld)
!
      use cal_sgs_mom_fluxes_grad
!
      integer(kind = kint), intent(in) :: icomp_sgs_lor
      integer(kind = kint), intent(in) :: ie_dbx
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (    SGS_param%iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_maxwell_grad', SGS_param%ifilter_final
        call cal_sgs_m_flux_grad_w_coef(SGS_param%ifilter_final,        &
     &      icomp_sgs_lor, iphys%i_SGS_maxwell, iphys%i_magne, ie_dbx,  &
     &      dt, FEM_prm, SGS_param, nod_comm, node, ele, fluid,         &
     &      iphys_ele, ele_fld, jac_3d, FEM_elens, sgs_coefs, rhs_tbl,  &
     &      mhd_fem_wk%mlump_fl, fem_wk, mhd_fem_wk, nod_fld)
!
!
      else if(SGS_param%iflag_SGS_lorentz .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'cal_sgs_mf_simi',  iphys%i_SGS_maxwell
        call cal_sgs_mf_simi(iphys%i_SGS_maxwell, iphys%i_magne,        &
     &      iphys%i_filter_magne, icomp_sgs_lor,                        &
     &      filter_param, nod_comm, node, filtering, sgs_coefs_nod,     &
     &      wk_filter, nod_fld)
!
      else if(SGS_param%iflag_SGS_lorentz .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'cal_sgs_m_flux_diffuse', iphys%i_SGS_maxwell
        call cal_sgs_m_flux_diffuse                                     &
     &    (iphys%i_magne, iphys%i_sgs_diffuse, iphys%i_SGS_maxwell, dt, &
     &     FEM_prm, nod_comm, node, ele, fluid, iphys_ele, ele_fld,     &
     &     jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine cal_sgs_maxwell
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_magne_induction                                &
     &         (icomp_sgs_uxb, ie_dvx, ie_dbx, dt,                      &
     &          FEM_prm, SGS_param, filter_param, nod_comm, node, ele,  &
     &          conduct, cd_prop, iphys, iphys_ele, ele_fld,            &
     &          jac_3d, rhs_tbl,  FEM_elens, filtering,                 &
     &          sgs_coefs, sgs_coefs_nod, wk_filter, mhd_fem_wk,        &
     &          fem_wk, f_l, nod_fld)
!
      use cal_sgs_inductions_grad
!
      integer(kind = kint), intent(in) :: icomp_sgs_uxb
      integer(kind = kint), intent(in) :: ie_dvx, ie_dbx
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
!
!
      if     (SGS_param%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_induct_t_grad'
        call cal_sgs_induct_t_grad_w_coef(SGS_param%ifilter_final,      &
     &      icomp_sgs_uxb, iphys%i_SGS_induct_t,                        &
     &      iphys%i_velo, iphys%i_magne, ie_dvx, ie_dbx, dt,            &
     &      FEM_prm, SGS_param, nod_comm, node, ele, conduct, cd_prop,  &
     &      iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens, sgs_coefs,  &
     &      fem_wk, mhd_fem_wk, f_l, nod_fld)
!
      else if(SGS_param%iflag_SGS_uxb .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &      write(*,*) 'cal_sgs_induct_t_simi'
        call cal_sgs_induct_t_simi                                      &
     &     (iphys%i_SGS_induct_t, iphys%i_velo,  iphys%i_magne,         &
     &      iphys%i_filter_velo, iphys%i_filter_magne, icomp_sgs_uxb,   &
     &      filter_param, nod_comm, node, filtering, sgs_coefs_nod,     &
     &      wk_filter, nod_fld)
      end if
!
      end subroutine cal_sgs_magne_induction
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_2_evo(icomp_sgs_uxb, ie_dvx, dt,           &
     &          FEM_prm, SGS_param, filter_param, nod_comm, node, ele,  &
     &          conduct, cd_prop, iphys, iphys_ele, ele_fld, jac_3d,    &
     &          rhs_tbl, FEM_elens, filtering, sgs_coefs, wk_filter,    &
     &          mhd_fem_wk, fem_wk, f_nl, nod_fld)
!
      use cal_rotation
      use cal_sgs_uxb_grad
!
      integer(kind = kint), intent(in) :: icomp_sgs_uxb
      integer(kind = kint), intent(in) :: ie_dvx
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if     (SGS_param%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_uxb_2_ff_grad', SGS_param%ifilter_final
        call cal_sgs_uxb_2_ff_grad(SGS_param%itype_Csym_uxb,            &
     &      SGS_param%icoord_Csim, SGS_param%ifilter_final,             &
     &      icomp_sgs_uxb, ie_dvx, dt, FEM_prm, node, ele, conduct,     &
     &      cd_prop, iphys, nod_fld, iphys_ele, ele_fld, jac_3d,        &
     &      rhs_tbl, FEM_elens, sgs_coefs, mhd_fem_wk, fem_wk, f_nl)
!
      else if(SGS_param%iflag_SGS_uxb .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'cal_sgs_uxb_2_ff_simi', SGS_param%ifilter_final
        call cal_sgs_uxb_2_ff_simi(icomp_sgs_uxb, dt,                   &
     &      FEM_prm, filter_param, nod_comm, node, ele, conduct, iphys, &
     &      iphys_ele, ele_fld, jac_3d, rhs_tbl, filtering, sgs_coefs,  &
     &      wk_filter, fem_wk, f_nl, nod_fld)
!
      else if(SGS_param%iflag_SGS_uxb .eq. id_SGS_diffusion) then
         if (iflag_debug.eq.1)                                          &
     &      write(*,*) 'choose_int_vol_rotations'
         call choose_int_vol_rotations                                  &
     &      (FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int, dt,    &
     &       conduct%istack_ele_fld_smp, iphys%i_magne,                 &
     &       node, ele, nod_fld, iphys_ele, ele_fld,                    &
     &       jac_3d, rhs_tbl, fem_wk, f_nl)
      end if
!
      end subroutine cal_sgs_uxb_2_evo
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_diffuse                                 &
     &         (i_vect, i_sgs_diffuse, i_sgs, dt,                       &
     &          FEM_prm, nod_comm, node, ele, fluid,                    &
     &          iphys_ele, ele_fld, jac_3d, rhs_tbl,                    &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use cal_gradient
!
      integer (kind=kint), intent(in) :: i_sgs, i_vect, i_sgs_diffuse
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call choose_cal_gradient_w_const                                  &
     &   (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,        &
     &    i_vect, i_sgs, dminus, fluid%istack_ele_fld_smp,              &
     &    mhd_fem_wk%mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld, &
     &    jac_3d, rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
      call choose_cal_gradient_w_const                                  &
     &   (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,        &
     &    (i_vect+1),  i_sgs_diffuse, dminus, fluid%istack_ele_fld_smp, &
     &    mhd_fem_wk%mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld, &
     &    jac_3d, rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
      call choose_cal_gradient_w_const                                  &
     &   (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,        &
     &    (i_vect+2), (i_sgs_diffuse+3),                                &
     &    dminus, fluid%istack_ele_fld_smp,                             &
     &    mhd_fem_wk%mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld, &
     &    jac_3d, rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
!
!
      call const_viscosity_tensor(nod_fld%n_point, nod_fld%ntot_phys,   &
     &    i_sgs_diffuse, i_sgs, nod_fld%d_fld)
!
      end subroutine cal_sgs_m_flux_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine const_viscosity_tensor                                 &
     &         (nnod, ncomp_nod, i_sgs_diffuse, i_sgs, d_nod)
!
      use cal_gradient
!
      integer (kind = kint), intent(in) :: nnod, ncomp_nod
      integer (kind=kint), intent(in) :: i_sgs, i_sgs_diffuse
      real(kind = kreal), intent(inout) :: d_nod(nnod,ncomp_nod)
!
      integer (kind=kint) :: inod
!
!
!$omp parallel do
      do inod = 1, nnod
        d_nod(inod,i_sgs  ) = two * d_nod(inod,i_sgs  )
        d_nod(inod,i_sgs+1) =       d_nod(inod,i_sgs+1)                 &
     &                            + d_nod(inod,i_sgs_diffuse  )
        d_nod(inod,i_sgs+2) =       d_nod(inod,i_sgs+2)                 &
     &                            + d_nod(inod,i_sgs_diffuse+3)
        d_nod(inod,i_sgs+3) = two * d_nod(inod,i_sgs_diffuse+1)
        d_nod(inod,i_sgs+4) =       d_nod(inod,i_sgs_diffuse+2)         &
     &                            + d_nod(inod,i_sgs_diffuse+4)
        d_nod(inod,i_sgs+5) = two * d_nod(inod,i_sgs_diffuse+5)
      end do
!$omp end parallel do
!
      end subroutine const_viscosity_tensor
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_fluxes
