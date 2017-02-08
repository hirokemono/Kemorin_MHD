!>@file   update_with_magne.f90
!!        module update_with_magne
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!       subroutine update_with_magnetic_field                          &
!!     &        (iak_diff_b, icomp_diff_b, ie_dbx, ie_dfbx, SGS_par,    &
!!     &         nod_comm, node, ele, surf, fluid, conduct, layer_tbl,  &
!!     &         sf_grp, Bsf_bcs, Fsf_bcs, iphys, iphys_ele,            &
!!     &         jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,             &
!!     &         FEM_elens, filtering, wide_filtering, m_lump,          &
!!     &         wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,&
!!     &         surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_data_type), intent(in) :: wide_filtering
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(dynamis_correlation_data), intent(inout) :: wk_cor
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!@endverbatim
!
      module update_with_magne
!
      use m_precision
!
      use m_machine_parameter
!
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_layering_ele_list
      use t_surface_bc_data
      use t_material_property
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine update_with_magnetic_field                             &
     &        (iak_diff_b, icomp_diff_b, ie_dbx, ie_dfbx, SGS_par,      &
     &         nod_comm, node, ele, surf, fluid, conduct, layer_tbl,    &
     &         sf_grp, Bsf_bcs, Fsf_bcs, iphys, iphys_ele,              &
     &         jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,               &
     &         FEM_elens, filtering, wide_filtering, m_lump,            &
     &         wk_cor, wk_lsq, wk_diff, wk_filter, mhd_fem_wk, fem_wk,  &
     &         surf_wk, f_l, f_nl, nod_fld, ele_fld, diff_coefs)
!
      use m_t_step_parameter
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_magne
      use cal_filtering_scalars
!
      integer(kind = kint), intent(in) :: iak_diff_b, icomp_diff_b
      integer(kind = kint), intent(in) :: ie_dbx, ie_dfbx
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer (kind = kint) :: iflag_dmc, iflag2
!
!
      if (i_step_sgs_coefs.eq.0) then
        iflag_dmc = 1
      else
        iflag_dmc = mod(i_step_MHD, i_step_sgs_coefs)
      end if
!
      if (iphys_ele%i_magne .ne. 0) then
        call vector_on_element_1st(node, ele, jac_3d_q,                 &
     &      ele%istack_ele_smp, intg_point_t_evo,                       &
     &      nod_fld%ntot_phys, iphys%i_magne, nod_fld%d_fld,            &
     &      ele_fld%ntot_phys, iphys_ele%i_magne,                       &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
!
        if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF        &
     &     .and.  iflag_dmc .eq. 0) then
         if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_similarity    &
     &     .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_similarity)   &
     &    then
           iflag2 = 3
         else if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_NL_grad  &
     &      .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad)     &
     &    then
           iflag2 = 2
         else
           iflag2 = 2
         end if
       else
         if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_similarity    &
     &     .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_similarity)   &
     &   then
           iflag2 = 1
         else
           iflag2 = 0
         end if
       end if
!
       if (iflag2.eq.1 .or. iflag2.eq.2 .or. iflag2.eq.3) then
         if (iphys%i_filter_magne .ne. 0) then
           if (iflag_debug.gt.0) write(*,*)                             &
     &         'cal_filtered_vector_whole',  iphys%i_filter_magne
           call cal_filtered_vector_whole                               &
     &        (SGS_par%filter_p, nod_comm, node, filtering,             &
     &         iphys%i_filter_magne, iphys%i_magne, wk_filter, nod_fld)
           nod_fld%iflag_update(iphys%i_filter_magne  ) = 1
           nod_fld%iflag_update(iphys%i_filter_magne+1) = 1
           nod_fld%iflag_update(iphys%i_filter_magne+2) = 1
         end if
!
         if (iflag2.eq.2 .and. iphys_ele%i_filter_magne.ne.0) then
           if (iflag_debug.gt.0) write(*,*) 'filtered_magne_on_ele'
            call vector_on_element_1st(node, ele, jac_3d_q,             &
     &          ele%istack_ele_smp, intg_point_t_evo,                   &
     &          nod_fld%ntot_phys, iphys%i_filter_magne,                &
     &          nod_fld%d_fld, ele_fld%ntot_phys,                       &
     &          iphys_ele%i_filter_magne, ele_fld%iflag_update,         &
     &          ele_fld%d_fld)
         end if
!
         if (iflag2.eq.2 .and. ie_dfbx.ne.0) then
           if (iflag_debug.gt.0) write(*,*) 'diff_filter_b_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (ele%istack_ele_smp, iphys%i_filter_magne, ie_dfbx,       &
     &         node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk)
         end if
!
         if (iflag2.eq.3 .and. iphys%i_wide_fil_magne.ne.0) then
           call cal_filtered_vector_whole                               &
     &        (SGS_par%filter_p, nod_comm, node, wide_filtering,        &
     &         iphys%i_wide_fil_magne, iphys%i_filter_magne,            &
     &         wk_filter, nod_fld)
            nod_fld%iflag_update(iphys%i_wide_fil_magne  ) = 1
            nod_fld%iflag_update(iphys%i_wide_fil_magne+1) = 1
            nod_fld%iflag_update(iphys%i_wide_fil_magne+2) = 1
         end if
       end if
!
!
      if(SGS_par%commute_p%iflag_c_magne .eq. id_SGS_commute_ON         &
     &     .and. diff_coefs%iflag_field(iak_diff_b) .eq. 0) then
        if (iflag2.eq.2 .or. iflag2.eq.3) then
          if (iflag_debug.gt.0) write(*,*) 's_cal_diff_coef_magne'
          call s_cal_diff_coef_magne(iak_diff_b, icomp_diff_b, SGS_par, &
     &        nod_comm, node, ele, surf, sf_grp, Bsf_bcs, Fsf_bcs,      &
     &        iphys, iphys_ele, ele_fld, fluid, layer_tbl,              &
     &        jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,                &
     &        FEM_elens, filtering, m_lump, wk_filter,                  &
     &        wk_cor, wk_lsq, wk_diff, fem_wk, surf_wk, f_l, f_nl,      &
     &        nod_fld, diff_coefs)
         end if
       end if
 !
 !
       if (  SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_NL_grad      &
     &  .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
        if ( ie_dbx.ne.0 ) then
           if (iflag_debug.gt.0) write(*,*) 'diff_magne_on_ele'
            call sel_int_diff_vector_on_ele                             &
     &         (ele%istack_ele_smp, iphys%i_magne, ie_dbx,              &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk)
        end if
       end if
!
      if (iphys_ele%i_current .ne. 0                                    &
     &     .and. iflag_4_rotate .eq. id_turn_ON) then
         if (iflag_debug.gt.0)  write(*,*) 'current_on_element'
        call rotation_on_element_1st(node, ele, jac_3d_q,               &
     &      conduct%istack_ele_fld_smp, intg_point_t_evo,               &
     &      nod_fld%ntot_phys, iphys%i_magne, nod_fld%d_fld,            &
     &      ele_fld%ntot_phys, iphys_ele%i_current,                     &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
!      call rotation_on_element_1st(node, ele, jac_3d_q,                &
!     &    ele%istack_ele_smp, intg_point_t_evo,                        &
!     &    nod_fld%ntot_phys, iphys%i_filter_vecp, nod_fld%d_fld,       &
!     &    ele_fld%ntot_phys, iphys_ele%i_filter_magne,                 &
!     &    ele_fld%iflag_update, ele_fld%d_fld)
!
       end subroutine update_with_magnetic_field
!
!-----------------------------------------------------------------------
!
      end module update_with_magne
