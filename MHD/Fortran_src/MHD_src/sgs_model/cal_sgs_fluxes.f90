!
!      module cal_sgs_fluxes
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_heat_flux(icomp_sgs_hf, ie_dvx,              &
!!     &          nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,&
!!     &          jac_3d, rhs_tbl, FEM_elens, filtering,                &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_sgs_momentum_flux(icomp_sgs_mf, ie_dvx,          &
!!     &          nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,&
!!     &          jac_3d, rhs_tbl, FEM_elens, filtering,                &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_sgs_maxwell(icomp_sgs_lor, ie_dbx,               &
!!     &          nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,&
!!     &          jac_3d, rhs_tbl, FEM_elens, filtering,                &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_sgs_magne_induction                              &
!!     &         (icomp_sgs_uxb, ie_dvx, ie_dbx, nod_comm, node, ele,   &
!!     &          conduct, iphys, iphys_ele, ele_fld, jac_3d, rhs_tbl,  &
!!     &          FEM_elens, filtering, mhd_fem_wk, fem_wk,             &
!!     &          f_l, nod_fld)
!!      subroutine cal_sgs_uxb_2_evo(icomp_sgs_uxb, ie_dvx,             &
!!     &        nod_comm, node, ele, conduct, iphys, iphys_ele, ele_fld,&
!!     &        jac_3d, rhs_tbl, FEM_elens, filtering,                  &
!!     &        mhd_fem_wk, fem_wk, f_nl, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
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
      use m_control_parameter
      use m_t_step_parameter
!
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
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
      subroutine cal_sgs_heat_flux(icomp_sgs_hf, ie_dvx,                &
     &          nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,  &
     &          jac_3d, rhs_tbl, FEM_elens, filtering,                  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use cal_sgs_heat_fluxes_grad
      use cal_gradient
!
      integer(kind = kint), intent(in) :: icomp_sgs_hf, ie_dvx
!
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
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (     iflag_SGS_heat .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'cal_sgs_h_flux_grad', ifilter_final
      call cal_sgs_h_flux_grad_w_coef(ifilter_final, icomp_sgs_hf,      &
     &    iphys%i_SGS_h_flux, iphys%i_sgs_temp, ie_dvx,                 &
     &    nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d,       &
     &    rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_l, nod_fld)
!
      else if (iflag_SGS_heat .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1) write(*,*) 'cal_sgs_hf_simi'
        call cal_sgs_hf_simi(iphys%i_SGS_h_flux, iphys%i_sgs_temp,      &
     &      iphys%i_filter_temp, icomp_sgs_hf,                          &
     &      nod_comm, node, iphys, filtering, nod_fld)
!
      else if (iflag_SGS_heat .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1) write(*,*) 'cal_sgs_h_flux_diffuse'
        call choose_cal_gradient_w_const(iflag_temp_supg,               &
     &      iphys%i_sgs_temp, iphys%i_SGS_h_flux, dminus,               &
     &      fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,              &
     &      nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,            &
     &      rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine cal_sgs_heat_flux
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_momentum_flux(icomp_sgs_mf, ie_dvx,            &
     &          nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,  &
     &          jac_3d, rhs_tbl, FEM_elens, filtering,                  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use cal_sgs_mom_fluxes_grad
!
      integer(kind = kint), intent(in) :: icomp_sgs_mf
      integer(kind = kint), intent(in) :: ie_dvx
!
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
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (  iflag_SGS_inertia .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_m_flux_grad', ifilter_final
        call cal_sgs_m_flux_grad_w_coef                                 &
     &     (itype_SGS_m_flux_coef, ifilter_final,                       &
     &      icomp_sgs_mf, iphys%i_SGS_m_flux, iphys%i_velo, ie_dvx,     &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld,             &
     &      jac_3d, FEM_elens, rhs_tbl, fem_wk, mhd_fem_wk, nod_fld)
!
      else if (iflag_SGS_inertia .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_mf_simi', iphys%i_SGS_m_flux
        call cal_sgs_mf_simi(iphys%i_SGS_m_flux, iphys%i_velo,          &
     &      iphys%i_filter_velo, icomp_sgs_mf,                          &
     &      nod_comm, node, filtering, nod_fld)
!
      else if (iflag_SGS_inertia .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_m_flux_diffuse', iphys%i_SGS_m_flux
        call cal_sgs_m_flux_diffuse                                     &
     &     (iphys%i_velo, iphys%i_sgs_diffuse, iphys%i_SGS_m_flux,      &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d,     &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine cal_sgs_momentum_flux
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_maxwell(icomp_sgs_lor, ie_dbx,                 &
     &          nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,  &
     &          jac_3d, rhs_tbl, FEM_elens, filtering,                  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use cal_sgs_mom_fluxes_grad
!
      integer(kind = kint), intent(in) :: icomp_sgs_lor
      integer(kind = kint), intent(in) :: ie_dbx
!
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
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (     iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_maxwell_grad', ifilter_final
        call cal_sgs_m_flux_grad_w_coef                                 &
     &     (itype_SGS_maxwell_coef, ifilter_final,                      &
     &      icomp_sgs_lor, iphys%i_SGS_maxwell, iphys%i_magne, ie_dbx,  &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld,             &
     &      jac_3d, FEM_elens, rhs_tbl, fem_wk, mhd_fem_wk, nod_fld)
!
!
      else if (iflag_SGS_lorentz .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'cal_sgs_mf_simi',  iphys%i_SGS_maxwell
        call cal_sgs_mf_simi(iphys%i_SGS_maxwell, iphys%i_magne,        &
     &      iphys%i_filter_magne, icomp_sgs_lor,                        &
     &      nod_comm, node, filtering, nod_fld)
!
      else if (iflag_SGS_lorentz .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'cal_sgs_m_flux_diffuse', iphys%i_SGS_maxwell
        call cal_sgs_m_flux_diffuse                                     &
     &     (iphys%i_magne, iphys%i_sgs_diffuse, iphys%i_SGS_maxwell,    &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d,     &
     &      rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine cal_sgs_maxwell
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_magne_induction                                &
     &         (icomp_sgs_uxb, ie_dvx, ie_dbx, nod_comm, node, ele,     &
     &          conduct, iphys, iphys_ele, ele_fld, jac_3d, rhs_tbl,    &
     &          FEM_elens, filtering, mhd_fem_wk, fem_wk,               &
     &          f_l, nod_fld)
!
      use cal_sgs_inductions_grad
!
      integer(kind = kint), intent(in) :: icomp_sgs_uxb
      integer(kind = kint), intent(in) :: ie_dvx, ie_dbx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
!
!
      if     (iflag_SGS_induction .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_induct_t_grad'
        call cal_sgs_induct_t_grad_w_coef                               &
     &     (ifilter_final, icomp_sgs_uxb, iphys%i_SGS_induct_t,         &
     &      iphys%i_velo, iphys%i_magne, ie_dvx, ie_dbx,                &
     &      nod_comm, node, ele, conduct, iphys_ele, ele_fld,           &
     &      jac_3d, rhs_tbl, FEM_elens, fem_wk, mhd_fem_wk,             &
     &      f_l, nod_fld)
!
      else if(iflag_SGS_induction .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &      write(*,*) 'cal_sgs_induct_t_simi'
        call cal_sgs_induct_t_simi(iphys%i_SGS_induct_t, iphys%i_velo,  &
     &      iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne,   &
     &      icomp_sgs_uxb, nod_comm, node, filtering, nod_fld)
      end if
!
      end subroutine cal_sgs_magne_induction
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_2_evo(icomp_sgs_uxb, ie_dvx,               &
     &        nod_comm, node, ele, conduct, iphys, iphys_ele, ele_fld,  &
     &        jac_3d, rhs_tbl, FEM_elens, filtering,                    &
     &        mhd_fem_wk, fem_wk, f_nl, nod_fld)
!
      use cal_rotation
      use cal_sgs_uxb_grad
!
      integer(kind = kint), intent(in) :: icomp_sgs_uxb
      integer(kind = kint), intent(in) :: ie_dvx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if     (iflag_SGS_induction .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &      write(*,*) 'cal_sgs_uxb_2_ff_grad', ifilter_final
        call cal_sgs_uxb_2_ff_grad                                      &
     &     (ifilter_final, icomp_sgs_uxb, ie_dvx, node, ele, conduct,   &
     &      iphys, nod_fld, iphys_ele, ele_fld, jac_3d,                 &
     &      rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
!
      else if(iflag_SGS_induction .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &      write(*,*) 'cal_sgs_uxb_2_ff_simi', ifilter_final
        call cal_sgs_uxb_2_ff_simi                                      &
     &     (icomp_sgs_uxb, nod_comm, node, ele, conduct, iphys,         &
     &      iphys_ele, ele_fld, jac_3d, rhs_tbl, filtering,             &
     &      fem_wk, f_nl, nod_fld)
!
      else if(iflag_SGS_induction .eq. id_SGS_diffusion) then
         if (iflag_debug.eq.1)                                          &
     &      write(*,*) 'choose_int_vol_rotations'
         call choose_int_vol_rotations(iflag_mag_supg,                  &
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
      subroutine cal_sgs_m_flux_diffuse(i_vect, i_sgs_diffuse, i_sgs,   &
     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d, &
     &          rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use cal_gradient
!
      integer (kind=kint), intent(in) :: i_sgs, i_vect, i_sgs_diffuse
!
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
     &   (iflag_velo_supg, i_vect, i_sgs, dminus,                       &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,                &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,              &
     &    rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
      call choose_cal_gradient_w_const                                  &
     &   (iflag_velo_supg, (i_vect+1), i_sgs_diffuse, dminus,           &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,                &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,              &
     &    rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
      call choose_cal_gradient_w_const                                  &
     &   (iflag_velo_supg, (i_vect+2), (i_sgs_diffuse+3), dminus,       &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,                &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,              &
     &    rhs_tbl, fem_wk, f_l, f_nl, nod_fld)
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
