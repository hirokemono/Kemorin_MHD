!>@file   update_with_scalars.f90
!!        module update_with_scalars
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_temperature(nod_comm, node, ele, surf,   &
!!     &          fluid, sf_grp, Tsf_bcs, iphys, iphys_ele, ele_fld,    &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,  &
!!     &          filtering, wide_filtering, layer_tbl,                 &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine update_with_dummy_scalar(nod_comm, node, ele, surf,  &
!!     &          fluid, sf_grp, Csf_bcs, iphys, iphys_ele, ele_fld,    &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,  &
!!     &          filtering, wide_filtering, layer_tbl,                 &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
!!        type(scaler_surf_bc_type), intent(in) :: Csf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_data_type), intent(in) :: wide_filtering
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module update_with_scalars
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
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_layering_ele_list
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
      subroutine update_with_temperature(nod_comm, node, ele, surf,     &
     &          fluid, sf_grp, Tsf_bcs, iphys, iphys_ele, ele_fld,      &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,    &
     &          filtering, wide_filtering, layer_tbl,                   &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_t_step_parameter
      use m_SGS_model_coefs
      use m_SGS_address
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_temp
      use cal_filtering_scalars
      use copy_nodal_fields
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
      integer (kind = kint) :: iflag_dynamic, iflag2
!
!
      if (i_step_sgs_coefs .eq. 0) then
        iflag_dynamic = 1
      else
        iflag_dynamic = mod(i_step_MHD, i_step_sgs_coefs)
      end if
!
!
      if (iphys%i_sgs_temp .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg) write(*,*)               &
     &          'iflag_SGS_parterbuation', iflag_SGS_parterbuation
        if(iflag_SGS_parterbuation .eq. 1) then
          call copy_scalar_component(node, nod_fld,                     &
     &        iphys%i_par_temp, iphys%i_sgs_temp)
        else
          call copy_scalar_component(node, nod_fld,                     &
     &        iphys%i_temp, iphys%i_sgs_temp)
        end if
      end if
!
      if(iflag_debug .ge. iflag_routine_msg) write(*,*)                 &
     &            'i_filter_temp', iphys%i_filter_temp
      if(iflag_debug .ge. iflag_routine_msg) write(*,*)                 &
                  'iflag_SGS_heat', iflag_SGS_heat
      if (iphys%i_filter_temp .gt. 0) then
        if(iflag_SGS_heat .ne. id_SGS_none) then
!
          if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                 &
     &         .and. iflag_dynamic.eq.0) then
            iflag2 = 1
          else if (iflag_SGS_model .eq. id_SGS_similarity) then
            iflag2 = 1
          else
            iflag2 = 0
          end if
          if (iflag_debug .gt. 0)   write(*,*) 'iflag2', iflag2
!
          if (iflag2 .eq. 1) then
            if (iflag_debug.gt.0) write(*,*) 'cal_filtered_temperature'
            call cal_filtered_scalar_whole(nod_comm, node, filtering,   &
     &          iphys%i_filter_temp, iphys%i_sgs_temp, nod_fld)
            nod_fld%iflag_update(iphys%i_filter_temp) = 1
          end if
!
          if (iphys%i_wide_fil_temp.ne.0 .and. iflag_dynamic.eq.0) then
            if (iflag_debug.gt.0)                                       &
     &        write(*,*) 'cal_w_filtered_scalar', iphys%i_wide_fil_temp
            call cal_filtered_scalar_whole                              &
     &         (nod_comm, node, wide_filtering,                         &
     &          iphys%i_wide_fil_temp, iphys%i_filter_temp, nod_fld)
          end if
        end if
!
        if( (iphys%i_filter_buo+iphys%i_f_buo_gen) .gt. 0) then
          if (iflag_debug.gt.0) write(*,*) 'filter temp for buoyancy'
          call cal_filtered_scalar_whole(nod_comm, node, filtering,     &
     &        iphys%i_filter_temp, iphys%i_temp, nod_fld)
          nod_fld%iflag_update(iphys%i_filter_temp) = 1
        end if
      end if
!
!
!       if (ie_dtx .ne. 0) then
!         if (iflag_debug.gt.0) write(*,*) 'diff_temp_on_ele'
!         call sel_int_diff_scalar_on_ele                               &
!     &      (ele%istack_ele_smp, iphys%i_temp, ie_dtx,                 &
!     &       node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk)
!       end if
!
       if (iflag_dynamic.eq.0                                           &
     &       .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
!
!         if (iflag_SGS_heat .eq. id_SGS_NL_grad) then
!           if (ie_dftx .ne. 0) then
!             if (iflag_debug.gt.0) write(*,*) 'diff_filter_t_on_ele'
!             call sel_int_diff_scalar_on_ele                           &
!     &          (ele%istack_ele_smp, iphys%i_filter_temp, ie_dftx,     &
!     &           node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk)
!           end if
!         end if
!       end if
!
         if (iflag_commute_temp .eq. id_SGS_commute_ON) then
           if ( iflag_diff_coefs(iak_diff_t) .eq. 0) then
!
             if (iflag_SGS_heat .eq. id_SGS_NL_grad) then
               if (iflag_debug.gt.0)  write(*,*) 's_cal_diff_coef_temp'
               call s_cal_diff_coef_temp(iak_diff_t, icomp_diff_t,      &
     &             nod_comm, node, ele, surf, sf_grp, Tsf_bcs,          &
     &             iphys, iphys_ele, ele_fld, fluid, layer_tbl,         &
     &             jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,           &
     &             FEM_elen, filtering, mhd_fem_wk, fem_wk,             &
     &             f_l, f_nl, nod_fld)
             end if
           end if
!
         end if
       end if
!
       end subroutine update_with_temperature
!
!-----------------------------------------------------------------------
!
      subroutine update_with_dummy_scalar(nod_comm, node, ele, surf,    &
     &          fluid, sf_grp, Csf_bcs, iphys, iphys_ele, ele_fld,      &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elen,    &
     &          filtering, wide_filtering, layer_tbl,                   &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_t_step_parameter
      use m_SGS_model_coefs
      use m_SGS_address
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_temp
      use cal_filtering_scalars
      use copy_nodal_fields
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type), intent(in) :: Csf_bcs
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
      integer (kind = kint) :: iflag_dynamic, iflag2
!
!
      if (i_step_sgs_coefs.eq.0) then
        iflag_dynamic = 1
      else
        iflag_dynamic = mod(i_step_MHD, i_step_sgs_coefs)
      end if
!
      if (iphys%i_sgs_composit .ne. 0) then
        call copy_scalar_component(node, nod_fld,                       &
     &      iphys%i_light, iphys%i_sgs_composit)
      end if
!
      iflag2 = 0
!
      if (iphys%i_filter_comp .ne. 0                                    &
     &       .and. iflag_SGS_heat.ne.id_SGS_none) then
        if (iflag2.eq.1) then
          if (iflag_debug.gt.0)   write(*,*) 'cal_filtered_composition'
          call cal_filtered_scalar_whole(nod_comm, node, filtering,     &
     &        iphys%i_filter_comp, iphys%i_sgs_composit, nod_fld)
          nod_fld%iflag_update(iphys%i_filter_comp) = 1
        end if
!
!        if (iphys%i_wide_fil_temp.ne.0 .and. iflag_dynamic.eq.0) then
!          if (iflag_debug.gt.0)                                        &
!     &      write(*,*) 'cal_w_filtered_scalar', iphys%i_wide_fil_temp
!          call cal_filtered_scalar_whole                               &
!     &       (nod_comm, node, wide_filtering,                          &
!     &        iphys%i_wide_fil_temp, iphys%i_filter_comp, nod_fld)
!        end if
      end if
!
!
!       if (iflag_dynamic.eq.0                                          &
!     &     .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
!         if (iflag_commute_composit .eq. id_SGS_commute_ON) then
!           if ( iflag_diff_coefs(iak_diff_t) .eq. 0) then
!
!             if (iflag_SGS_heat .eq. id_SGS_NL_grad) then
!               if (iflag_debug.gt.0)  write(*,*) 's_cal_diff_coef_temp'
!               call s_cal_diff_coef_temp(iak_diff_t, icomp_diff_t,     &
!     &             nod_comm, node, ele, surf, sf_grp, Csf_bcs,         &
!     &             iphys, iphys_ele, ele_fld, fluid, layer_tbl,        &
!     &             jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,          &
!     &             FEM_elen, filtering, mhd_fem_wk, fem_wk,            &
!     &             f_l, f_nl, nod_fld)
!             end if
!
!           end if
!         end if
!       end if
!
!
!      call sel_int_diff_scalar_on_ele                                  &
!     &   (ele%istack_ele_smp, iphys%i_light, ie_dcx,                   &
!     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk)
!
!      call sel_int_diff_scalar_on_ele                                  &
!     &   (ele%istack_ele_smp, iphys%i_filter_comp, ie_dfcx,            &
!     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk)
!
!
       end subroutine update_with_dummy_scalar
!
!-----------------------------------------------------------------------
!
      end module update_with_scalars
