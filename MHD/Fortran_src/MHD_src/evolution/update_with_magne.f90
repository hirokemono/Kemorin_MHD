!>@file   update_with_magne.f90
!!        module update_with_magne
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_magnetic_field(layer_tbl)
!!@endverbatim
!
      module update_with_magne
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
       subroutine update_with_magnetic_field(layer_tbl)
!
      use m_t_step_parameter
      use m_geometry_data_MHD
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_SGS_model_coefs
      use m_SGS_address
      use m_filter_elength
!
      use average_on_elements
      use cal_filtering_vectors
      use cal_diff_vector_on_ele
      use cal_diff_coef_magne
      use cal_w_filtering_scalars
!
      type(layering_tbl), intent(in) :: layer_tbl
!
      integer (kind = kint) :: iflag_dynamic, iflag2
!
      if (i_step_sgs_coefs.eq.0) then
        iflag_dynamic = 1
      else
        iflag_dynamic = mod(i_step_MHD, i_step_sgs_coefs)
      end if
!
      if (iphys_ele%i_magne .ne. 0) then
        call vector_on_element_1st(node1, ele1, jac1_3d_q,              &
     &      ele1%istack_ele_smp, intg_point_t_evo,                      &
     &      nod_fld1%ntot_phys, iphys%i_magne, nod_fld1%d_fld,          &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne,                      &
     &      fld_ele1%iflag_update, fld_ele1%d_fld)
      end if
!
!
       if(iflag_dynamic.eq.0                                            &
     &       .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
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
       if (iflag2.eq.1 .or. iflag2.eq.2 .or. iflag2.eq.3) then
         if (iphys%i_filter_magne .ne. 0) then
           if (iflag_debug.gt.0)                                        &
     &         write(*,*) 'cal_filtered_vector', iphys%i_filter_magne
           call cal_filtered_vector(nod_comm, node1,                    &
     &         iphys%i_filter_magne, iphys%i_magne, nod_fld1)
           nod_fld1%iflag_update(iphys%i_filter_magne  ) = 1
           nod_fld1%iflag_update(iphys%i_filter_magne+1) = 1
           nod_fld1%iflag_update(iphys%i_filter_magne+2) = 1
         end if
!
         if (iflag2.eq.2 .and. iphys_ele%i_filter_magne.ne.0) then
           if (iflag_debug.gt.0) write(*,*) 'filtered_magne_on_ele'
            call vector_on_element_1st(node1, ele1, jac1_3d_q,          &
     &          ele1%istack_ele_smp, intg_point_t_evo,                  &
     &          nod_fld1%ntot_phys, iphys%i_filter_magne,               &
     &          nod_fld1%d_fld, fld_ele1%ntot_phys,                     &
     &          iphys_ele%i_filter_magne, fld_ele1%iflag_update,        &
     &          fld_ele1%d_fld)
         end if
!
         if (iflag2.eq.2 .and. ie_dfbx.ne.0) then
           if (iflag_debug.gt.0) write(*,*) 'diff_filter_b_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (ele1%istack_ele_smp, iphys%i_filter_magne, ie_dfbx,      &
     &         node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l,             &
     &         mhd_fem1_wk)
         end if
!
         if (iflag2.eq.3 .and. iphys%i_wide_fil_magne.ne.0) then
           call cal_w_filtered_vector(iphys%i_wide_fil_magne,           &
     &         iphys%i_filter_magne, nod_comm, node1, nod_fld1)
            nod_fld1%iflag_update(iphys%i_wide_fil_magne  ) = 1
            nod_fld1%iflag_update(iphys%i_wide_fil_magne+1) = 1
            nod_fld1%iflag_update(iphys%i_wide_fil_magne+2) = 1
         end if
       end if
!
!
      if(iflag_commute_magne .eq. id_SGS_commute_ON                     &
     &     .and. iflag_diff_coefs(iak_diff_b) .eq. 0) then
        if (iflag2.eq.2 .or. iflag2.eq.3) then
          if (iflag_debug.gt.0) write(*,*) 's_cal_diff_coef_magne'
          call s_cal_diff_coef_magne(iak_diff_b, icomp_diff_b,          &
     &        nod_comm, node1, ele1, surf1, sf_grp1,                    &
     &        iphys, iphys_ele, fld_ele1, layer_tbl,                    &
     &        jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,         &
     &        FEM1_elen, m1_lump, fem1_wk, f1_l, f1_nl, nod_fld1)
         end if
       end if
 !
 !
       if (  iflag_SGS_lorentz .eq.   id_SGS_NL_grad                    &
     &  .or. iflag_SGS_induction .eq. id_SGS_NL_grad) then
        if ( ie_dbx.ne.0 ) then
           if (iflag_debug.gt.0) write(*,*) 'diff_magne_on_ele'
            call sel_int_diff_vector_on_ele                             &
     &         (ele1%istack_ele_smp, iphys%i_magne, ie_dbx,             &
     &          node1, ele1, nod_fld1, jac1_3d_q, jac1_3d_l,            &
     &          mhd_fem1_wk)
        end if
       end if
!
      if (iphys_ele%i_current .ne. 0                                    &
     &     .and. iflag_4_rotate .eq. id_turn_ON) then
         if (iflag_debug.gt.0)  write(*,*) 'current_on_element'
        call rotation_on_element_1st(node1, ele1, jac1_3d_q,            &
     &      conduct1%istack_ele_fld_smp, intg_point_t_evo,              &
     &      nod_fld1%ntot_phys, iphys%i_magne, nod_fld1%d_fld,          &
     &      fld_ele1%ntot_phys, iphys_ele%i_current,                    &
     &      fld_ele1%iflag_update, fld_ele1%d_fld)
      end if
!
!      call rotation_on_element_1st(node1, ele1, jac1_3d_q,             &
!     &    ele1%istack_ele_smp, intg_point_t_evo,                       &
!     &    nod_fld1%ntot_phys, iphys%i_filter_vecp, nod_fld1%d_fld,     &
!     &    fld_ele1%ntot_phys, iphys_ele%i_filter_magne,                &
!     &    fld_ele1%iflag_update, fld_ele1%d_fld)
!
       end subroutine update_with_magnetic_field
!
!-----------------------------------------------------------------------
!
      end module update_with_magne
