!
!     module int_vol_velo_pre
!
!     numerical integration for finite elememt equations of momentum
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_velo_pre_ele
!      subroutine int_vol_velo_pre_ele_upw
!      subroutine int_vol_velo_pre_ele_upm
!
!
      module int_vol_velo_pre
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_parameter
      use m_geometry_data_MHD
      use m_geometry_data
      use m_phys_constants
      use m_physical_property
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_SGS_address
!
      implicit none
!
      private :: int_vol_velo_pre_ele_upwind
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_pre_ele
!
      use m_node_phys_address
      use m_element_phys_address
      use m_element_phys_data
      use m_finite_element_matrix
      use m_fem_gauss_int_coefs
      use m_int_vol_data
!
      use cal_add_smp
      use nodal_fld_cst_to_ele_1st
      use gravity_vec_each_ele_1st
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nodal_field_1st
      use fem_skv_vector_diff_1st
      use fem_skv_nonlinear_1st
      use fem_skv_div_sgs_flux_1st
      use fem_skv_lorentz_full_1st
!
      integer(kind=kint) :: k2, num_int
!
!  ---------  set number of integral points
!
      num_int = intg_point_t_evo
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
!
      do k2=1, nnod_4_ele
!
!  -----   set advection  --------
!
        if (coef_nega_v .ne. 0.0d0) then
          call vector_cst_phys_2_each_ele(k2, iphys%i_velo,             &
     &        coef_nega_v, velo_1)
!
!  -----  Inertia including Reynolds stress by rotation form --------
!
          if (iflag_4_rotate .eq. 1) then
!
            if(iflag_SGS_inertia .ne. id_SGS_none                       &
     &        .and. iflag_commute_inertia .eq. id_SGS_commute_ON) then
              call SGS_tensor_cst_each_ele_1st(k2, iphys%i_velo,        &
     &            iphys%i_SGS_m_flux, coef_nega_v, sgs_t, tensor_e)
!
              call fem_skv_rot_inertia_1st(iele_fl_smp_stack,           &
     &            num_int, k2, velo_1, d_ele(1,iphys_ele%i_vort), sk6)
              call fem_skv_div_sgs_tensor_1st                           &
     &               (iele_fl_smp_stack, num_int, k2, n_filter_final,   &
     &                ak_diff(1,iak_diff_mf), sgs_t, tensor_e, sk6)
            else if(iflag_SGS_inertia .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(k2, iphys%i_SGS_m_flux,   &
     &              coef_nega_v, sgs_t)
              call fem_skv_inertia_rot_sgs_1st(iele_fl_smp_stack,       &
     &              num_int, k2, velo_1, sgs_t,                         &
     &              d_ele(1,iphys_ele%i_vort), sk6)
            else
              call fem_skv_rot_inertia_1st(iele_fl_smp_stack,           &
     &            num_int, k2, velo_1, d_ele(1,iphys_ele%i_vort), sk6)
            end if
!
!  -----  Inertia including Reynolds stress --------
!
          else
            if(iflag_SGS_inertia .ne. id_SGS_none                       &
     &        .and. iflag_commute_inertia .eq. id_SGS_commute_ON) then
              call SGS_tensor_cst_each_ele_1st(k2, iphys%i_velo,        &
     &            iphys%i_SGS_m_flux, coef_nega_v, sgs_t, tensor_e)
              call fem_skv_vec_inertia_modsgs_1st(iele_fl_smp_stack,    &
     &            num_int, k2, n_filter_final, ak_diff(1,iak_diff_mf),  &
     &            velo_1, sgs_t, tensor_e, d_ele(1,iphys_ele%i_velo),   &
     &            sk6)
            else if(iflag_SGS_inertia .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(k2, iphys%i_SGS_m_flux,   &
     &              coef_nega_v, sgs_t)
              call fem_skv_vec_inertia_sgs_1st(iele_fl_smp_stack,       &
     &            num_int, k2, velo_1, sgs_t,                           &
     &            d_ele(1,iphys_ele%i_velo), sk6)
            else
              call fem_skv_vector_inertia_1st(iele_fl_smp_stack,        &
     &          num_int, k2, velo_1, d_ele(1,iphys_ele%i_velo), sk6)
            end if
          end if
        end if
!
!  -----   set Lorentz force  --------
!
        if (iflag_4_lorentz .gt. 0) then
          if (iflag_4_lorentz.eq.1 .and. iflag_4_rotate.eq.1) then
            call vector_cst_phys_2_each_ele(k2, iphys%i_vecp,           &
     &          coef_lor, vect_1)
!$omp parallel
            call add_const_to_vector_smp(np_smp, numele,                &
     &          iele_smp_stack, d_ele(1,iphys_ele%i_magne),             &
     &          ex_magne, vect_e)
!$omp end parallel
!
            call fem_skv_lorentz_rot_1st(iele_fl_smp_stack,             &
     &          num_int, k2, vect_1, vect_e, sk6)
          else if (iflag_4_rotate .eq. 0) then
            call vector_cst_phys_2_each_ele(k2, iphys%i_magne,          &
     &          coef_lor, magne_1)
!$omp parallel
            call add_const_to_vector_smp(np_smp, numele,                &
     &          iele_smp_stack, d_ele(1,iphys_ele%i_magne),             &
     &          ex_magne, vect_e)
!$omp end parallel
!
            call fem_skv_vector_inertia_1st(iele_fl_smp_stack,          &
     &            num_int, k2, magne_1, vect_e, sk6)
          end if
!
!    set SGS Lorentz force
!
          if ( iflag_SGS_lorentz .ne. id_SGS_none) then
            if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
              call SGS_tensor_cst_each_ele_1st(k2, iphys%i_magne,       &
     &              iphys%i_SGS_maxwell, coef_lor, sgs_t, tensor_e)
              call fem_skv_div_sgs_tensor_1st                           &
     &            (iele_fl_smp_stack, num_int, k2, n_filter_final,      &
     &             ak_diff(1,iak_diff_lor), sgs_t, tensor_e, sk6)
            else
              call tensor_cst_phys_2_each_ele(k2, iphys%i_SGS_maxwell,  &
     &             coef_lor, tensor_e)
              call fem_skv_div_tensor(iele_fl_smp_stack,                &
     &             num_int, k2, tensor_e, sk6)
            end if
          end if
!
        end if
!
!  --------  set coriolis force
!
        if ( iflag_4_coriolis .eq. 1 ) then
          call vector_cst_phys_2_each_ele(k2, iphys%i_velo,             &
     &        coef_cor, velo_1)
          call fem_skv_coriolis_1st(iele_fl_smp_stack, num_int, k2,     &
     &        velo_1, angular, sk6)
        end if
!
! ---------  set buoyancy
!
        if(iflag_4_gravity.eq.1 .and. iflag_4_composit_buo.eq.1) then
          call set_double_gvec_each_ele_1st(k2, iphys%i_temp,           &
     &        iphys%i_light, ak_buo, ak_comp_buo, vect_e)
          call fem_skv_vector_1st(iele_fl_smp_stack, num_int, k2,       &
     &        vect_e, sk6)
        else if (iflag_4_gravity .eq. 1) then
          call set_gravity_vec_each_ele_1st(k2, iphys%i_temp, ak_buo,   &
     &        vect_e)
          call fem_skv_vector_1st(iele_fl_smp_stack, num_int, k2,       &
     &        vect_e, sk6)
        else if (iflag_4_composit_buo .eq. 1) then
          call set_gravity_vec_each_ele_1st(k2, iphys%i_light,          &
     &        ak_comp_buo, vect_e)
          call fem_skv_vector_1st(iele_fl_smp_stack, num_int, k2,       &
     &        vect_e, sk6)
        else if (iflag_4_filter_gravity .eq. 1) then
          call set_gravity_vec_each_ele_1st(k2, iphys%i_filter_temp,    &
     &        ak_buo, vect_e)
          call fem_skv_vector_1st(iele_fl_smp_stack, num_int, k2,       &
     &        vect_e, sk6)
        end if
!
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_velo_pre_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_pre_ele_upw
!
      use m_element_phys_address
!
      call int_vol_velo_pre_ele_upwind(iphys_ele%i_velo)
!
      end subroutine int_vol_velo_pre_ele_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_pre_ele_upm
!
      use m_element_phys_address
!
      call int_vol_velo_pre_ele_upwind(iphys_ele%i_magne)
!
      end subroutine int_vol_velo_pre_ele_upm
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_pre_ele_upwind(ie_upw)
!
      use m_node_phys_address
      use m_element_phys_address
      use m_element_phys_data
      use m_finite_element_matrix
      use m_fem_gauss_int_coefs
      use m_int_vol_data
!
      use cal_add_smp
      use nodal_fld_cst_to_ele_1st
      use gravity_vec_each_ele_1st
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nodal_fld_upw_1st
      use fem_skv_vect_diff_upw_1st
      use fem_skv_nonlinear_upw_1st
      use fem_skv_div_sgs_flux_upw_1
      use fem_skv_lorentz_full_1st
!
      integer(kind = kint), intent(in) :: ie_upw
      integer(kind = kint) :: k2, num_int
!
!  ---------  set number of integral points
!
      num_int = intg_point_t_evo
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
!
      do k2=1, nnod_4_ele
!
!  -----   set advection  --------
!
        if (coef_nega_v .ne. 0.0d0) then
          call vector_cst_phys_2_each_ele(k2, iphys%i_velo,             &
     &        coef_nega_v, velo_1)
!
!  -----  Inertia including Reynolds stress by rotation form --------
!
          if (iflag_4_rotate .eq. 1) then
            if(iflag_SGS_inertia .ne. id_SGS_none                       &
     &        .and. iflag_commute_inertia .eq. id_SGS_commute_ON) then
              call SGS_tensor_cst_each_ele_1st(k2, iphys%i_velo,        &
     &            iphys%i_SGS_m_flux, coef_nega_v, sgs_t, tensor_e)
!
              call fem_skv_rot_inertia_upw_1st(iele_fl_smp_stack,       &
     &            num_int, k2, velo_1, d_ele(1,iphys_ele%i_vort),       &
     &            d_ele(1,ie_upw), sk6)
              call fem_skv_div_sgs_tensor_1st_upw                       &
     &               (iele_fl_smp_stack, num_int, k2, n_filter_final,   &
     &                ak_diff(1,iak_diff_mf), d_ele(1,ie_upw),          &
     &                sgs_t, tensor_e, sk6)
            else if(iflag_SGS_inertia .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(k2, iphys%i_SGS_m_flux,   &
     &              coef_nega_v, sgs_t)
              call fem_skv_inertia_rot_sgs_upw_1(iele_fl_smp_stack,     &
     &            num_int, k2, velo_1, sgs_t, d_ele(1,ie_upw),          &
     &            d_ele(1,ie_upw), sk6)
            else
              call fem_skv_rot_inertia_upw_1st(iele_fl_smp_stack,       &
     &            num_int, k2, velo_1, d_ele(1,iphys_ele%i_vort),       &
     &            d_ele(1,ie_upw), sk6)
            end if
!
!  -----  Inertia including Reynolds stress --------
!
          else
            if(iflag_SGS_inertia .ne. id_SGS_none                       &
     &        .and. iflag_commute_inertia .eq. id_SGS_commute_ON) then
              call SGS_tensor_cst_each_ele_1st(k2, iphys%i_velo,        &
     &            iphys%i_SGS_m_flux, coef_nega_v, sgs_t, tensor_e)
              call fem_skv_vec_inertia_msgs_upw_1(iele_fl_smp_stack,    &
     &            num_int, k2, n_filter_final, ak_diff(1,iak_diff_mf),  &
     &            velo_1, sgs_t, tensor_e, d_ele(1,iphys_ele%i_velo),   &
     &            d_ele(1,ie_upw), sk6)
            else if(iflag_SGS_inertia .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(k2, iphys%i_SGS_m_flux,   &
     &              coef_nega_v, sgs_t)
              call fem_skv_vcl_inertia_sgs_upw_1(iele_fl_smp_stack,     &
     &            num_int, k2, velo_1, sgs_t,                           &
     &            d_ele(1,iphys_ele%i_velo), d_ele(1,ie_upw), sk6)
            else
              call fem_skv_vector_inertia_upw_1st(iele_fl_smp_stack,    &
     &            num_int, k2, velo_1, d_ele(1,iphys_ele%i_velo),       &
     &            d_ele(1,ie_upw), sk6)
            end if
          end if
!
          if (iflag_4_rotate .eq. 1) then
          else
          end if
!
!    set Reynolds stress
!
          if ( iflag_SGS_inertia .ne. id_SGS_none) then
            if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
              call SGS_tensor_cst_each_ele_1st(k2, iphys%i_velo,        &
     &            iphys%i_SGS_m_flux, coef_nega_v, sgs_t, tensor_e)
              call fem_skv_div_sgs_tensor_1st_upw                       &
     &               (iele_fl_smp_stack, num_int, k2, n_filter_final,   &
     &                ak_diff(1,iak_diff_mf), d_ele(1,ie_upw),          &
     &                sgs_t, tensor_e, sk6)
            else
              call tensor_cst_phys_2_each_ele(k2, iphys%i_SGS_m_flux,   &
     &             coef_nega_v, tensor_e)
              call fem_skv_div_tsr_upw(iele_fl_smp_stack, num_int, k2,  &
     &             d_ele(1,ie_upw), tensor_e, sk6)
            end if
          end if
        end if
!
!  -----   set Lorentz force  --------
!
        if (iflag_4_lorentz .gt. 0) then
          if (iflag_4_rotate .eq. 1) then
            call vector_cst_phys_2_each_ele(k2, iphys%i_vecp,           &
     &          coef_lor, vect_1)
!$omp parallel
            call add_const_to_vector_smp(np_smp, numele,                &
     &          iele_smp_stack, d_ele(1,iphys_ele%i_magne),             &
     &          ex_magne, vect_e)
!$omp end parallel
!
            call fem_skv_lorentz_rot_1st(iele_fl_smp_stack,             &
     &          num_int, k2, vect_1, vect_e, sk6)
          else
            call vector_cst_phys_2_each_ele(k2, iphys%i_magne,          &
     &          coef_lor, magne_1)
!$omp parallel
            call add_const_to_vector_smp(np_smp, numele,                &
     &          iele_smp_stack, d_ele(1,iphys_ele%i_magne),             &
     &          ex_magne, vect_e)
!$omp end parallel
!
            call fem_skv_vector_inertia_upw_1st(iele_fl_smp_stack,      &
     &          num_int, k2, magne_1, vect_e, d_ele(1,ie_upw), sk6)
          end if
!
!    set SGS Lorentz force
!
          if ( iflag_SGS_lorentz .ne. id_SGS_none) then
            if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
              call SGS_tensor_cst_each_ele_1st(k2, iphys%i_magne,       &
     &            iphys%i_SGS_maxwell, coef_lor, sgs_t, tensor_e)
              call fem_skv_div_sgs_tensor_1st_upw                       &
     &               (iele_fl_smp_stack, num_int, k2, n_filter_final,   &
     &                ak_diff(1,iak_diff_lor), d_ele(1,ie_upw),         &
     &                sgs_t, tensor_e, sk6)
            else
              call tensor_cst_phys_2_each_ele(k2, iphys%i_SGS_maxwell,  &
     &             coef_lor, tensor_e)
              call fem_skv_div_tsr_upw(iele_fl_smp_stack, num_int, k2,  &
     &             d_ele(1,ie_upw), tensor_e, sk6)
            end if
          end if
        end if
!
!  --------  set coriolis force
!
        if ( iflag_4_coriolis .eq. 1 ) then
          call vector_cst_phys_2_each_ele(k2, iphys%i_velo,             &
     &        coef_cor, velo_1)
          call fem_skv_coriolis_upw_1st(iele_fl_smp_stack,              &
     &        num_int, k2, velo_1, angular, d_ele(1,ie_upw), sk6)
        end if
!
! ---------  set buoyancy
!
        if(iflag_4_gravity.eq.1 .and. iflag_4_composit_buo.eq.1) then
          call set_double_gvec_each_ele_1st(k2, iphys%i_temp,           &
     &        iphys%i_light, ak_buo, ak_comp_buo, vect_e)
          call fem_skv_vector_field_upw_1st(iele_fl_smp_stack,          &
     &        num_int, k2, d_ele(1,ie_upw), vect_e, sk6)
        else if (iflag_4_gravity .eq. 1) then
          call set_gravity_vec_each_ele_1st(k2, iphys%i_temp, ak_buo,   &
     &        vect_e)
          call fem_skv_vector_field_upw_1st(iele_fl_smp_stack,          &
     &        num_int, k2, d_ele(1,ie_upw), vect_e, sk6)
        else if (iflag_4_composit_buo .eq. 1) then
          call set_gravity_vec_each_ele_1st(k2, iphys%i_light,          &
     &        ak_comp_buo, vect_e)
          call fem_skv_vector_field_upw_1st(iele_fl_smp_stack,          &
     &        num_int, k2, d_ele(1,ie_upw), vect_e, sk6)
        else if (iflag_4_filter_gravity .eq. 1) then
          call set_gravity_vec_each_ele_1st(k2, iphys%i_filter_temp,    &
     &        ak_buo, vect_e)
          call fem_skv_vector_field_upw_1st(iele_fl_smp_stack,          &
     &        num_int, k2, d_ele(1,ie_upw), vect_e, sk6)
        end if
!
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_velo_pre_ele_upwind
!
!-----------------------------------------------------------------------
!
      end module int_vol_velo_pre
