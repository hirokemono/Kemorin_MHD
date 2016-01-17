!
!      module cal_model_coefficients
!
!      Written by H. Matsui
!
!!      subroutine s_cal_model_coefficients                             &
!!     &         (i_dvx, i_dbx, i_dfvx, i_dfbx,                         &
!!     &          nod_comm, node, ele, surf, sf_grp, iphys,             &
!!     &          iphys_ele, ele_fld, fluid, conduct, layer_tbl,        &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,  &
!!     &          m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_model_coefficients
!
      use m_precision
!
      use calypso_mpi
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
      use t_jacobian_2d
      use t_jacobian_3d
      use t_table_FEM_const
      use t_layering_ele_list
      use t_MHD_finite_element_mat
      use t_filter_elength
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_model_coefficients                               &
     &         (i_dvx, i_dbx, i_dfvx, i_dfbx,                           &
     &          nod_comm, node, ele, surf, sf_grp, iphys,               &
     &          iphys_ele, ele_fld, fluid, conduct, layer_tbl,          &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,    &
     &          m_lump, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
!
      use m_t_step_parameter
      use cal_sgs_h_flux_dynamic_simi
!
      use cal_sgs_heat_flux_dynamic
      use cal_sgs_h_flux_dynamic_simi
      use cal_diff_coef_sgs_hf
      use cal_sgs_mom_flux_dynamic
      use cal_sgs_m_flux_dynamic_simi
      use cal_diff_coef_sgs_mf
      use cal_sgs_maxwell_dynamic
      use cal_diff_coef_sgs_mxwl
      use cal_sgs_induction_dynamic
      use cal_diff_coef_sgs_induct
      use cal_sgs_uxb_dynamic_simi
!
      integer(kind = kint), intent(in) :: i_dvx, i_dfvx
      integer(kind = kint), intent(in) :: i_dbx, i_dfbx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(iflag_dynamic_SGS .eq. id_SGS_DYNAMIC_OFF) return
      if(mod(i_step_MHD, i_step_sgs_coefs) .ne. 0) return
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &            'set Csim', i_step_MHD, i_step_sgs_coefs
!
      if(iflag_t_evo_4_temp .ne. id_no_evolution) then
        if (iflag_SGS_heat .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_hf_dynamic'
          call cal_sgs_hf_dynamic(i_dvx, i_dfvx,                        &
     &        nod_comm, node, ele, iphys, iphys_ele, ele_fld,           &
     &        fluid, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,            &
     &        FEM_elen, mhd_fem_wk, fem_wk, f_l, nod_fld)
!
        else if (iflag_SGS_heat .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &          write(*,*) 's_cal_sgs_h_flux_dynamic_simi'
          call s_cal_sgs_h_flux_dynamic_simi(nod_comm, node, ele,       &
     &        iphys, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,            &
     &        m_lump, fem_wk, f_l, nod_fld)
        end if
!
        if ( iflag_commute_heat .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_hf'
          call s_cal_diff_coef_sgs_hf                                   &
     &       (i_dfvx, nod_comm, node, ele, surf, sf_grp,                &
     &        iphys, iphys_ele, ele_fld, fluid, layer_tbl,              &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,      &
     &        mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
        end if
      end if
!
!
      if(iflag_t_evo_4_velo .ne. id_no_evolution) then
!
        if (iflag_SGS_inertia .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_m_flux_dynamic'
          call cal_sgs_m_flux_dynamic(i_dvx, i_dfvx,                    &
     &        nod_comm, node, ele, iphys, iphys_ele, ele_fld,           &
     &        fluid, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,            &
     &        FEM_elen, mhd_fem_wk, fem_wk, nod_fld)
        else if (iflag_SGS_inertia .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 's_cal_sgs_m_flux_dynamic_simi'
          call s_cal_sgs_m_flux_dynamic_simi                            &
     &       (nod_comm, node, ele, iphys, layer_tbl,                    &
     &        jac_3d_q, jac_3d_l, rhs_tbl, m_lump,                      &
     &        fem_wk, f_l, nod_fld)
        end if
!
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_mf'
          call s_cal_diff_coef_sgs_mf                                   &
     &       (i_dfvx, nod_comm, node, ele, surf, sf_grp,                &
     &        iphys, iphys_ele, ele_fld, fluid, layer_tbl,              &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,      &
     &        mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
        end if
      end if
!
!
      if (iflag_4_lorentz .ne. id_turn_OFF) then
!
        if (iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_t_dynamic'
          call cal_sgs_maxwell_t_dynamic(i_dbx, i_dfbx,                 &
     &        nod_comm, node, ele, iphys, iphys_ele, ele_fld,           &
     &        fluid, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,            &
     &        FEM_elen, mhd_fem_wk, fem_wk, nod_fld)
        else if (iflag_SGS_lorentz .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_dynamic_simi'
          call cal_sgs_maxwell_dynamic_simi                             &
     &       (nod_comm, node, ele, iphys, layer_tbl,                    &
     &        jac_3d_q, jac_3d_l, rhs_tbl, m_lump,                      &
     &        fem_wk, f_l, nod_fld)
        end if
!
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1) write(*,*) 's_cal_diff_coef_sgs_mxwl'
          call s_cal_diff_coef_sgs_mxwl                                 &
     &       (i_dfbx, nod_comm, node, ele, surf, sf_grp,                &
     &        iphys, iphys_ele, ele_fld, fluid, layer_tbl,              &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,      &
     &        mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
        end if
      end if
!
!
!
      if(iflag_t_evo_4_magne .gt. id_no_evolution) then
        if(iflag_SGS_induction .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic'
          call cal_sgs_induct_t_dynamic                                 &
     &       (i_dvx, i_dbx, i_dfvx, i_dfbx,                             &
     &        nod_comm, node, ele, iphys, iphys_ele, ele_fld,           &
     &        conduct, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,          &
     &        FEM_elen, mhd_fem_wk, fem_wk, f_l, nod_fld)
        else if(iflag_SGS_induction .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic_simi'
          call cal_sgs_induct_t_dynamic_simi(nod_comm, node, ele,       &
     &        iphys, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,            &
     &        m_lump, fem_wk, f_l, nod_fld)
        end if
!
        if (iflag_commute_induction .eq. id_SGS_commute_ON) then
          if(iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_induct'
          call s_cal_diff_coef_sgs_induct                               &
     &       (i_dfvx, i_dfbx, nod_comm, node, ele, surf, sf_grp,        &
     &        iphys, iphys_ele, ele_fld, conduct, layer_tbl,            &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,      &
     &        mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
        end if
!
      else if(iflag_t_evo_4_vect_p .gt. id_no_evolution) then
!
        if(iflag_SGS_induction .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_uxb_dynamic'
          call cal_sgs_uxb_dynamic(i_dvx, i_dfvx,                       &
     &        nod_comm, node, ele, iphys, iphys_ele, ele_fld,           &
     &        conduct, layer_tbl, jac_3d_q, jac_3d_l, rhs_tbl,          &
     &        FEM_elen, mhd_fem_wk, fem_wk, f_l, nod_fld)
        else if(iflag_SGS_induction .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)  write(*,*)                             &
     &                          's_cal_sgs_uxb_dynamic_simi'
          call s_cal_sgs_uxb_dynamic_simi(nod_comm, node, ele,          &
     &        iphys, layer_tbl, jac_3d_q, jac_3d_l, nod_fld)
        end if
!
      end if
!
      end subroutine s_cal_model_coefficients
!
!-----------------------------------------------------------------------
!
      end module cal_model_coefficients
