!>@file   update_with_velo.f90
!!        module update_with_velo
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_velocity                                 &
!!     &         (iak_diff_v, icomp_diff_v, ie_dvx, ie_dfvx,            &
!!     &          FEM_prm, SGS_par, nod_comm, node, ele, surf, fluid,   &
!!     &          sf_grp, Vsf_bcs, Psf_bcs, iphys, iphys_ele,           &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,            &
!!     &          FEM_elens, filtering, wide_filtering, layer_tbl,      &
!!     &          wk_cor, wk_lsq, wk_diff, wk_filter,                   &
!!     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,               &
!!     &          nod_fld, ele_fld, diff_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(velocity_surf_bc_type), intent(in) :: Vsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Psf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_data_type), intent(in) :: wide_filtering
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
      module update_with_velo
!
      use m_precision
!
      use m_machine_parameter
!
      use t_FEM_control_parameter
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
      use t_material_property
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
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
      subroutine update_with_velocity                                   &
     &         (iak_diff_v, icomp_diff_v, ie_dvx, ie_dfvx,              &
     &          FEM_prm, SGS_par, nod_comm, node, ele, surf, fluid,     &
     &          sf_grp, Vsf_bcs, Psf_bcs, iphys, iphys_ele,             &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,              &
     &          FEM_elens, filtering, wide_filtering, layer_tbl,        &
     &          wk_cor, wk_lsq, wk_diff, wk_filter,                     &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                 &
     &          nod_fld, ele_fld, diff_coefs)
!
      use m_t_step_parameter
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_velo
!
      integer(kind = kint), intent(in) :: iak_diff_v, icomp_diff_v
      integer(kind = kint), intent(in) :: ie_dvx, ie_dfvx
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in) :: Vsf_bcs
      type(potential_surf_bc_type), intent(in) :: Psf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
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
!
      if (iphys_ele%i_velo .ne. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &                 write(*,*) 'velocity_on_element'
        call vector_on_element_1st(node, ele, jac_3d_q,                 &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo,                 &
     &      nod_fld%ntot_phys, iphys%i_velo, nod_fld%d_fld,             &
     &      ele_fld%ntot_phys, iphys_ele%i_velo,                        &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
      if( FEM_prm%iflag_rotate_form .eq. id_turn_ON                     &
     &      .and. iphys_ele%i_vort .ne. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &                 write(*,*) 'vorticity_on_element'
        call rotation_on_element_1st(node, ele, jac_3d_q,               &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo,                 &
     &      nod_fld%ntot_phys, iphys%i_velo, nod_fld%d_fld,             &
     &      ele_fld%ntot_phys, iphys_ele%i_vort,                        &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
!   required field for explicit filtering
!
      if (iphys%i_filter_velo .ne. 0) then
!
        if (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF       &
     &      .and. iflag_dmc.eq.0) then
          iflag2 = 1
        else if(SGS_par%model_p%iflag_SGS .eq. id_SGS_similarity) then
          iflag2 = 1
        else
          iflag2 = 0
        end if
!
        if (iflag2 .eq. 1) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &      write(*,*) 'cal_filtered_vector', iphys%i_filter_velo
          call cal_filtered_vector_whole                                &
     &       (SGS_par%filter_p, nod_comm, node, filtering,              &
     &        iphys%i_filter_velo, iphys%i_velo, wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%i_filter_velo  ) = 1
          nod_fld%iflag_update(iphys%i_filter_velo+1) = 1
          nod_fld%iflag_update(iphys%i_filter_velo+2) = 1
        end if
      end if
!
      if (iphys%i_wide_fil_velo.ne.0 .and. iflag_dmc.eq.0) then
        if (SGS_par%model_p%iflag_SGS.eq.id_SGS_similarity              &
     &    .and. SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF)  &
     &   then
          call cal_filtered_vector_whole                                &
     &       (SGS_par%filter_p, nod_comm, node, wide_filtering,         &
     &        iphys%i_wide_fil_velo, iphys%i_filter_velo,               &
     &        wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%i_wide_fil_velo  ) = 1
          nod_fld%iflag_update(iphys%i_wide_fil_velo+1) = 1
          nod_fld%iflag_update(iphys%i_wide_fil_velo+2) = 1
        end if
      end if
!
!    required field for vector potential
!
       if (SGS_par%model_p%iflag_SGS.eq.id_SGS_NL_grad                  &
     &    .and. SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF   &
     &    .and. iflag_dmc.eq.0) then
!
         if (iphys_ele%i_filter_velo.ne.0) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 'diff_filter_v_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (fluid%istack_ele_fld_smp, iphys%i_filter_velo, ie_dfvx,  &
     &         node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk)
         end if
!
        if (SGS_par%commute_p%iflag_c_velo .eq. id_SGS_commute_ON       &
     &         .and. diff_coefs%iflag_field(iak_diff_v) .eq. 0) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &                 write(*,*) 's_cal_diff_coef_velo'
          call s_cal_diff_coef_velo(iak_diff_v, icomp_diff_v,           &
     &        FEM_prm, SGS_par, nod_comm, node, ele, surf, sf_grp,      &
     &        Vsf_bcs, Psf_bcs, iphys, iphys_ele, ele_fld, fluid,       &
     &        layer_tbl, jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,     &
     &        FEM_elens, filtering, wk_filter, wk_cor, wk_lsq, wk_diff, &
     &        mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                   &
     &        nod_fld, diff_coefs)
        end if
!
      end if
!
!   required field for gradient model
!
       if (ie_dvx .ne. 0) then
         if(SGS_par%model_p%iflag_SGS .eq. id_SGS_NL_grad) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 'diff_velocity_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (fluid%istack_ele_fld_smp, iphys%i_velo, ie_dvx,          &
     &         node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk)
         end if
       end if
!
       end subroutine update_with_velocity
!
!-----------------------------------------------------------------------
!
      end module update_with_velo
