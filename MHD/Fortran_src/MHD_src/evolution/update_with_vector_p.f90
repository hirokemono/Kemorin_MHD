!>@file   update_with_vector_p.f90
!!        module update_with_vector_p
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_vector_potential                         &
!!     &        (iak_diff_b, icomp_diff_b, ie_dbx, ie_dfbx,             &
!!     &          nod_comm, node, ele, surf, fluid, conduct, layer_tbl, &
!!     &          sf_grp, Bnod_bcs, Asf_bcs, Fsf_bcs, iphys, iphys_ele, &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,  &
!!     &          filtering, wide_filtering, m_lump,                    &
!!     &          wk_cor, wk_lsq, wk_diff, wk_filter,                   &
!!     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,               &
!!     &          nod_fld, ele_fld, diff_coefs)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
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
      module update_with_vector_p
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
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_layering_ele_list
      use t_material_property
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
      use t_bc_data_magne
      use t_surface_bc_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine update_with_vector_potential                           &
     &        (iak_diff_b, icomp_diff_b, ie_dbx, ie_dfbx,               &
     &          nod_comm, node, ele, surf, fluid, conduct, layer_tbl,   &
     &          sf_grp, Bnod_bcs, Asf_bcs, Fsf_bcs, iphys, iphys_ele,   &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,    &
     &          filtering, wide_filtering, m_lump,                      &
     &          wk_cor, wk_lsq, wk_diff, wk_filter,                     &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                 &
     &          nod_fld, ele_fld, diff_coefs)
!
      use m_control_parameter
      use m_t_step_parameter
!
      use average_on_elements
      use cal_rotation_sgs
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_vector_p
      use cal_filtering_scalars
!
      integer(kind = kint), intent(in) :: iak_diff_b, icomp_diff_b
      integer(kind = kint), intent(in) :: ie_dbx, ie_dfbx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
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
      integer (kind = kint) :: iflag_dynamic, iflag2
!
!   set model coefficients for vector potential
!
      if (i_step_sgs_coefs.eq.0) then
        iflag_dynamic = 1
      else
        iflag_dynamic = mod(i_step_MHD, i_step_sgs_coefs)
      end if
!
!
      if (iflag_dynamic.eq.0 .and.  iphys%i_filter_vecp.ne.0            &
     &     .and. cmt_param1%iflag_c_magne .eq. id_SGS_commute_ON) then
!
        if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
          if (iflag_debug.gt.0) write(*,*) 'cal_filtered_vector_p'
          call cal_filtered_vector_whole(nod_comm, node, filtering,     &
     &        iphys%i_filter_vecp, iphys%i_vecp, wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%i_filter_vecp  ) = 1
          nod_fld%iflag_update(iphys%i_filter_vecp+1) = 1
          nod_fld%iflag_update(iphys%i_filter_vecp+2) = 1
        end if
!
        if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                   &
     &    .and. iflag_SGS_model.eq.id_SGS_similarity                    &
     &    .and. iphys%i_wide_fil_vecp.ne. 0) then
          if (iflag_debug.gt.0)                                         &
     &         write(*,*) 'cal_filtered_vector_p i_wide_fil_vecp'
          call cal_filtered_vector_whole                                &
     &       (nod_comm, node, wide_filtering,                           &
     &        iphys%i_wide_fil_vecp, iphys%i_filter_vecp,               &
     &        wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%i_wide_fil_vecp  ) = 1
          nod_fld%iflag_update(iphys%i_wide_fil_vecp+1) = 1
          nod_fld%iflag_update(iphys%i_wide_fil_vecp+2) = 1
        end if
!
!
        if ( diff_coefs%iflag_field(iak_diff_b) .eq. 0) then
          if(iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
            if (iflag_SGS_model .eq. id_SGS_NL_grad                     &
     &        .or. iflag_SGS_model .eq. id_SGS_similarity) then
              call s_cal_diff_coef_vector_p(iak_diff_b, icomp_diff_b,   &
     &            nod_comm, node, ele, surf, fluid, layer_tbl,          &
     &            sf_grp, Asf_bcs, Fsf_bcs, iphys, iphys_ele, ele_fld,  &
     &            jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,  &
     &            filtering, m_lump, wk_filter,                         &
     &            wk_cor, wk_lsq, wk_diff, fem_wk, surf_wk,             &
     &            f_l, f_nl, nod_fld, diff_coefs)
            end if
!
          end if
        end if
      end if
!
!   lead magnetic field
!
      if (iphys%i_magne .ne. 0) then
        if (iflag_debug.gt.0) write(*,*) 'cal_magnetic_f_by_vect_p'
        call choose_cal_rotation_sgs                                    &
     &     (cmt_param1%iflag_c_magne, iflag_mag_supg,                   &
     &      iak_diff_b, iphys%i_vecp, iphys%i_magne,                    &
     &      ele%istack_ele_smp, m_lump,                                 &
     &      nod_comm, node, ele, surf, sf_grp, iphys_ele, ele_fld,      &
     &      jac_3d_q, jac_sf_grp_q, FEM_elen, diff_coefs,               &
     &      Bnod_bcs%nod_bc_b, Asf_bcs%sgs, rhs_tbl, fem_wk, surf_wk,   &
     &      f_nl, nod_fld)
      end if
      if (iphys_ele%i_magne .ne. 0) then
        if (iflag_debug.gt.0) write(*,*) 'rot_magne_on_element'
        call rotation_on_element_1st(node, ele, jac_3d_q,               &
     &      ele%istack_ele_smp, intg_point_t_evo,                       &
     &      nod_fld%ntot_phys, iphys%i_vecp, nod_fld%d_fld,             &
     &      ele_fld%ntot_phys, iphys_ele%i_magne,                       &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
      if (iphys_ele%i_current .ne. 0                                    &
     &     .and. iflag_4_rotate .eq. id_turn_ON) then
        if (iflag_debug.gt.0) write(*,*) 'current_on_element'
        call rotation_on_element_1st(node, ele, jac_3d_q,               &
     &      conduct%istack_ele_fld_smp, intg_point_t_evo,               &
     &      nod_fld%ntot_phys, iphys%i_magne, nod_fld%d_fld,            &
     &      ele_fld%ntot_phys, iphys_ele%i_current,                     &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
!   required field for explicit filtering
!
       if(iflag_dynamic.eq.0                                            &
     &      .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
         if (        iflag_SGS_lorentz .eq. id_SGS_similarity           &
     &       .or.  iflag_SGS_induction .eq. id_SGS_similarity) then
           iflag2 = 3
         else if (   iflag_SGS_lorentz .eq. id_SGS_NL_grad              &
     &       .or.  iflag_SGS_induction .eq. id_SGS_NL_grad) then
           iflag2 = 2
         else
           iflag2 = 2
         end if
       else
         if (        iflag_SGS_lorentz .eq. id_SGS_similarity           &
     &       .or.  iflag_SGS_induction .eq. id_SGS_similarity) then
           iflag2 = 1
         else
           iflag2 = 0
         end if
       end if
!
       if (iflag_debug .ge. iflag_routine_msg) write(*,*)               &
         'flag for magnetic field filtering', iflag2
!
       if (iflag2.eq.1 .or. iflag2.eq.2 .or. iflag2.eq.3) then
         if (iphys%i_filter_magne .ne. 0) then
           if (iflag_debug.gt.0)                                        &
     &        write(*,*) 'cal_filtered_vector', iphys%i_filter_magne
           call cal_filtered_vector_whole(nod_comm, node, filtering,    &
     &         iphys%i_filter_magne, iphys%i_magne, wk_filter, nod_fld)
           nod_fld%iflag_update(iphys%i_filter_magne  ) = 1
           nod_fld%iflag_update(iphys%i_filter_magne+1) = 1
           nod_fld%iflag_update(iphys%i_filter_magne+2) = 1
         end if
!
         if (iphys_ele%i_filter_magne .ne. 0) then
           if (iflag_debug .ge. iflag_routine_msg) write(*,*)           &
     &                         'filtered_magne_on_ele'
            call vector_on_element_1st(node, ele, jac_3d_q,             &
     &          ele%istack_ele_smp, intg_point_t_evo,                   &
     &          nod_fld%ntot_phys, iphys%i_filter_magne,                &
     &          nod_fld%d_fld, ele_fld%ntot_phys,                       &
     &          iphys_ele%i_filter_magne, ele_fld%iflag_update,         &
     &          ele_fld%d_fld)
         end if
!
         if(iflag2.eq.2 .and. ie_dfbx.ne.0) then
           if (iflag_debug .ge. iflag_routine_msg) write(*,*)           &
     &                         'diff_filter_b_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (ele%istack_ele_smp, iphys%i_filter_magne, ie_dfbx,       &
     &         node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk)
         end if
!
         if(iflag2.eq.3 .and. iphys%i_wide_fil_magne.ne.0) then
           call cal_filtered_vector_whole                               &
     &        (nod_comm, node, wide_filtering,                          &
     &         iphys%i_wide_fil_magne, iphys%i_filter_magne,            &
     &         wk_filter, nod_fld)
           nod_fld%iflag_update(iphys%i_wide_fil_magne  ) = 1
           nod_fld%iflag_update(iphys%i_wide_fil_magne+1) = 1
           nod_fld%iflag_update(iphys%i_wide_fil_magne+2) = 1
         end if
!
       end if
!
!   required field for gradient model
!
!
       if (  iflag_SGS_lorentz .eq.   id_SGS_NL_grad                    &
     &  .or. iflag_SGS_induction .eq. id_SGS_NL_grad) then
         if ( ie_dbx.ne.0 ) then
           if (iflag_debug.gt.0) write(*,*) 'diff_magne_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (ele%istack_ele_smp, iphys%i_magne, ie_dbx,               &
     &         node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk)
        end if
       end if
!
       end subroutine update_with_vector_potential
!
!-----------------------------------------------------------------------
!
      end module update_with_vector_p
