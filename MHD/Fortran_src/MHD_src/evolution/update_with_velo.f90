!>@file   update_with_velo.f90
!!        module update_with_velo
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_velocity(layer_tbl)
!!@endverbatim
!
      module update_with_velo
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
      subroutine update_with_velocity(layer_tbl)
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
      use m_geometry_data_MHD
      use m_filter_elength
      use m_SGS_model_coefs
      use m_SGS_address
!
      use average_on_elements
      use cal_filtering_vectors
      use cal_diff_vector_on_ele
      use cal_diff_coef_velo
      use cal_w_filtering_scalars
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
!
      if (iphys_ele%i_velo .ne. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &                 write(*,*) 'velocity_on_element'
        call vector_on_element_1st(node1, ele1, jac1_3d_q,              &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      nod_fld1%ntot_phys, iphys%i_velo, nod_fld1%d_fld,           &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo,                       &
     &      fld_ele1%iflag_update, fld_ele1%d_fld)
      end if
!
       if ( iflag_4_rotate .eq. id_turn_ON                              &
     &      .and. iphys_ele%i_vort .ne. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &                 write(*,*) 'vorticity_on_element'
        call rotation_on_element_1st(node1, ele1, jac1_3d_q,            &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      nod_fld1%ntot_phys, iphys%i_velo, nod_fld1%d_fld,           &
     &      fld_ele1%ntot_phys, iphys_ele%i_vort,                       &
     &      fld_ele1%iflag_update, fld_ele1%d_fld)
      end if
!
!   required field for explicit filtering
!
      if (iphys%i_filter_velo .ne. 0) then
!
        if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                   &
     &      .and. iflag_dynamic.eq.0) then
          iflag2 = 1
        else if (iflag_SGS_model .eq. id_SGS_similarity) then
          iflag2 = 1
        else
          iflag2 = 0
        end if
!
        if (iflag2 .eq. 1) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &      write(*,*) 'cal_filtered_vector', iphys%i_filter_velo
          call cal_filtered_vector(nod_comm, node1,                     &
     &        iphys%i_filter_velo, iphys%i_velo, nod_fld1)
          nod_fld1%iflag_update(iphys%i_filter_velo  ) = 1
          nod_fld1%iflag_update(iphys%i_filter_velo+1) = 1
          nod_fld1%iflag_update(iphys%i_filter_velo+2) = 1
        end if
      end if
!
      if (iphys%i_wide_fil_velo.ne.0 .and. iflag_dynamic.eq.0) then
        if (iflag_SGS_model.eq.id_SGS_similarity                        &
     &    .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
          call cal_w_filtered_vector(iphys%i_wide_fil_velo,             &
     &        iphys%i_filter_velo, nod_comm, node1, nod_fld1)
          nod_fld1%iflag_update(iphys%i_wide_fil_velo  ) = 1
          nod_fld1%iflag_update(iphys%i_wide_fil_velo+1) = 1
          nod_fld1%iflag_update(iphys%i_wide_fil_velo+2) = 1
        end if
      end if
!
!    required field for vector potential
!
       if (iflag_SGS_model.eq.id_SGS_NL_grad                            &
     &    .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF               &
     &    .and. iflag_dynamic.eq.0) then
!
         if (iphys_ele%i_filter_velo.ne.0) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 'diff_filter_v_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (fluid1%istack_ele_fld_smp, iphys%i_filter_velo, ie_dfvx, &
     &         node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l,             &
     &         mhd_fem1_wk)
         end if
!
        if (iflag_commute_velo .eq. id_SGS_commute_ON                   &
     &         .and. iflag_diff_coefs(iak_diff_v) .eq. 0) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &                 write(*,*) 's_cal_diff_coef_velo'
          call s_cal_diff_coef_velo(iak_diff_v, icomp_diff_v,           &
     &        nod_comm, node1, ele1, surf1, sf_grp1,                    &
     &        iphys, iphys_ele, fld_ele1, fluid1, layer_tbl,            &
     &        jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,         &
     &        FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
        end if
!
      end if
!
!   required field for gradient model
!
       if (ie_dvx .ne. 0) then
         if ( iflag_SGS_model .eq. id_SGS_NL_grad) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 'diff_velocity_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (fluid1%istack_ele_fld_smp, iphys%i_velo, ie_dvx,         &
     &         node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l,             &
     &         mhd_fem1_wk)
         end if
       end if
!
       end subroutine update_with_velocity
!
!-----------------------------------------------------------------------
!
      end module update_with_velo
