!>@file   update_with_scalars.f90
!!        module update_with_scalars
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_temperature(layer_tbl)
!!      subroutine update_with_dummy_scalar(layer_tbl)
!!@endverbatim
!
      module update_with_scalars
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_int_vol_data
      use m_nod_comm_table
      use m_finite_element_matrix
!
      use t_layering_ele_list
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine update_with_temperature(layer_tbl)
!
      use m_t_step_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_filter_elength
      use m_SGS_model_coefs
      use m_SGS_address
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_temp
      use cal_w_filtering_scalars
      use copy_nodal_fields
!
      type(layering_tbl), intent(in) :: layer_tbl
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
          call copy_scalar_component(node1, nod_fld1,                   &
     &        iphys%i_par_temp, iphys%i_sgs_temp)
        else
          call copy_scalar_component(node1, nod_fld1,                   &
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
            call cal_filtered_scalar(nod_comm, node1,                   &
     &          iphys%i_filter_temp, iphys%i_sgs_temp, nod_fld1)
            nod_fld1%iflag_update(iphys%i_filter_temp) = 1
          end if
!
          if (iphys%i_wide_fil_temp.ne.0 .and. iflag_dynamic.eq.0) then
            if (iflag_debug.gt.0)                                       &
     &        write(*,*) 'cal_w_filtered_scalar', iphys%i_wide_fil_temp
            call cal_w_filtered_scalar(iphys%i_wide_fil_temp,           &
     &          iphys%i_filter_temp, nod_comm, node1, nod_fld1)
                nod_fld1%iflag_update(iphys%i_wide_fil_temp) = 1
          end if
        end if
!
        if( (iphys%i_filter_buo+iphys%i_f_buo_gen) .gt. 0) then
          if (iflag_debug.gt.0) write(*,*) 'filter temp for buoyancy'
          call cal_filtered_scalar(nod_comm, node1,                     &
     &        iphys%i_filter_temp, iphys%i_temp, nod_fld1)
          nod_fld1%iflag_update(iphys%i_filter_temp) = 1
        end if
      end if
!
!
!       if (ie_dtx .ne. 0) then
!         if (iflag_debug.gt.0) write(*,*) 'diff_temp_on_ele'
!         call sel_int_diff_scalar_on_ele                               &
!     &      (ele1%istack_ele_smp, iphys%i_temp, ie_dtx,                &
!     &       node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l, mhd_fem1_wk)
!       end if
!
       if (iflag_dynamic.eq.0                                           &
     &       .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
!
!         if (iflag_SGS_heat .eq. id_SGS_NL_grad) then
!           if (ie_dftx .ne. 0) then
!             if (iflag_debug.gt.0) write(*,*) 'diff_filter_t_on_ele'
!             call sel_int_diff_scalar_on_ele                           &
!     &          (ele1%istack_ele_smp, iphys%i_filter_temp, ie_dftx,    &
!     &           node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l,          &
!     &           mhd_fem1_wk)
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
     &             nod_comm, node1, ele1, surf1, sf_grp1,               &
     &             iphys, iphys_ele, fld_ele1, fluid1, layer_tbl,       &
     &             jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,    &
     &             FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl,        &
     &             nod_fld1)
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
      subroutine update_with_dummy_scalar(layer_tbl)
!
      use m_t_step_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_SGS_model_coefs
      use m_SGS_address
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_temp
      use cal_w_filtering_scalars
      use copy_nodal_fields
!
      type(layering_tbl), intent(in) :: layer_tbl
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
        call copy_scalar_component(node1, nod_fld1,                     &
     &      iphys%i_light, iphys%i_sgs_composit)
      end if
!
      iflag2 = 0
!
      if (iphys%i_filter_comp .ne. 0                                    &
     &       .and. iflag_SGS_heat.ne.id_SGS_none) then
        if (iflag2.eq.1) then
          if (iflag_debug.gt.0)   write(*,*) 'cal_filtered_composition'
          call cal_filtered_scalar(nod_comm, node1,                     &
     &        iphys%i_filter_comp, iphys%i_sgs_composit, nod_fld1)
          nod_fld1%iflag_update(iphys%i_filter_comp) = 1
        end if
!
!        if (iphys%i_wide_fil_temp.ne.0 .and. iflag_dynamic.eq.0) then
!          if (iflag_debug.gt.0)                                        &
!     &      write(*,*) 'cal_w_filtered_scalar', iphys%i_wide_fil_temp
!          call cal_w_filtered_scalar(iphys%i_wide_fil_temp,            &
!     &        iphys%i_filter_comp, nod_comm, node1, nod_fld1)
!              nod_fld1%iflag_update(iphys%i_wide_fil_temp) = 1
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
!     &              nod_comm, node1, ele1, surf1, sf_grp1,             &
!     &              iphys, iphys_ele, fld_ele1, fluid1, layer_tbl,     &
!     &              jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,  &
!     &              FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl,      &
!     &              nod_fld1)
!             end if
!
!           end if
!         end if
!       end if
!
!
!      call sel_int_diff_scalar_on_ele                                  &
!     &   (ele1%istack_ele_smp, iphys%i_light, ie_dcx,                  &
!     &    node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l, mhd_fem1_wk)
!
!      call sel_int_diff_scalar_on_ele                                  &
!     &   (ele1%istack_ele_smp, iphys%i_filter_comp, ie_dfcx,           &
!     &    node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l, mhd_fem1_wk)
!
!
       end subroutine update_with_dummy_scalar
!
!-----------------------------------------------------------------------
!
      end module update_with_scalars
