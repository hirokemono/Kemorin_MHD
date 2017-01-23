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
!!      subroutine int_vol_velo_pre_ele                                 &
!!     &        (node, ele, fluid, iphys, nod_fld, ak_MHD,              &
!!     &         ncomp_ele, d_ele, iphys_ele, iak_diff_mf, iak_diff_lor,&
!!     &         jac_3d, rhs_tbl, FEM_elens, diff_coefs,                &
!!     &         mhd_fem_wk, fem_wk, f_nl)
!!      subroutine int_vol_velo_pre_ele_upwind                          &
!!     &         (node, ele, fluid, iphys, nod_fld, ak_MHD,             &
!!     &          ncomp_ele, ie_upw, d_ele, iphys_ele,                  &
!!     &          iak_diff_mf, iak_diff_lor, jac_3d, rhs_tbl, FEM_elens,&
!!     &          diff_coefs, mhd_fem_wk, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      module int_vol_velo_pre
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use m_physical_property
      use m_fem_gauss_int_coefs
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_phys_address
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_pre_ele                                   &
     &        (node, ele, fluid, iphys, nod_fld, ak_MHD,                &
     &         ncomp_ele, d_ele, iphys_ele, iak_diff_mf, iak_diff_lor,  &
     &         jac_3d, rhs_tbl, FEM_elens, diff_coefs,                  &
     &         mhd_fem_wk, fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_cst_to_element
      use gravity_vec_each_ele
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nodal_field_type
      use fem_skv_vector_diff_type
      use fem_skv_nonlinear_type
      use fem_skv_div_sgs_flux_type
      use fem_skv_lorentz_full_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: iak_diff_mf, iak_diff_lor
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      type(phys_address), intent(in) :: iphys_ele
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind=kint) :: k2, num_int
!
!  ---------  set number of integral points
!
      num_int = intg_point_t_evo
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
!
!  -----   set advection  --------
!
        if (fl_prop1%coef_nega_v .ne. 0.0d0) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &        iphys%i_velo, fl_prop1%coef_nega_v, mhd_fem_wk%velo_1)
!
!  -----  Inertia including Reynolds stress by rotation form --------
!
          if (iflag_4_rotate .eq. id_turn_ON) then
!
            if(iflag_SGS_inertia .ne. id_SGS_none                       &
     &        .and. iflag_commute_inertia .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &           iphys%i_velo, iphys%i_SGS_m_flux, fl_prop1%coef_nega_v,&
     &           mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
!
              call fem_skv_rot_inertia_type                             &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele%i_vort),         &
     &            ele, jac_3d, fem_wk%sk6)
              call fem_skv_div_sgs_tensor                               &
     &           (fluid%istack_ele_fld_smp, num_int, k2, ifilter_final, &
     &            diff_coefs%num_field, iak_diff_mf, diff_coefs%ak,     &
     &            ele, jac_3d, FEM_elens, mhd_fem_wk%sgs_t1,            &
     &            fem_wk%tensor_1, fem_wk%sk6)
            else if(iflag_SGS_inertia .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(node, ele, nod_fld,       &
     &            k2, iphys%i_SGS_m_flux, fl_prop1%coef_nega_v,         &
     &            mhd_fem_wk%sgs_t1)
              call fem_skv_inertia_rot_sgs_pg                           &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            ele, jac_3d, mhd_fem_wk%velo_1,                       &
     &            mhd_fem_wk%sgs_t1, d_ele(1,iphys_ele%i_vort),         &
     &            fem_wk%sk6)
            else
              call fem_skv_rot_inertia_type                             &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele%i_vort),         &
     &            ele, jac_3d, fem_wk%sk6)
            end if
!
!  -----  Inertia including Reynolds stress --------
!
          else
            if(iflag_SGS_inertia .ne. id_SGS_none                       &
     &        .and. iflag_commute_inertia .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &           iphys%i_velo, iphys%i_SGS_m_flux, fl_prop1%coef_nega_v,&
     &           mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
              call fem_skv_vec_inertia_modsgs_pg                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2, ifilter_final, &
     &            diff_coefs%num_field, iak_diff_mf, diff_coefs%ak,     &
     &            ele, jac_3d, FEM_elens, mhd_fem_wk%velo_1,            &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1,                   &
     &            d_ele(1,iphys_ele%i_velo), fem_wk%sk6)
            else if(iflag_SGS_inertia .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(node, ele, nod_fld,       &
     &            k2, iphys%i_SGS_m_flux, fl_prop1%coef_nega_v,         &
     &            mhd_fem_wk%sgs_t1)
              call fem_skv_vec_inertia_sgs_pg                           &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            ele, jac_3d, mhd_fem_wk%velo_1,                       &
     &            mhd_fem_wk%sgs_t1, d_ele(1,iphys_ele%i_velo),         &
     &            fem_wk%sk6)
            else
              call fem_skv_vector_inertia_type                          &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele%i_velo),         &
     &            ele, jac_3d, fem_wk%sk6)
            end if
          end if
        end if
!
!  -----   set Lorentz force  --------
!
        if (iflag_4_lorentz .gt. id_turn_OFF) then
          if (iflag_4_lorentz .eq. id_turn_ON                           &
     &         .and. iflag_4_rotate .eq. id_turn_ON) then
            call vector_cst_phys_2_each_ele(node, ele, nod_fld,         &
     &          k2, iphys%i_vecp, fl_prop1%coef_lor, mhd_fem_wk%vecp_1)
!$omp parallel
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele%i_magne), cd_prop1%ex_magne,          &
     &          fem_wk%vector_1)
!$omp end parallel
!
            call fem_skv_lorentz_rot_galerkin                           &
     &         (fluid%istack_ele_fld_smp, num_int, k2,                  &
     &          mhd_fem_wk%vecp_1, fem_wk%vector_1,                     &
     &          ele, jac_3d, fem_wk%sk6)
          else if (iflag_4_rotate .eq. id_turn_OFF) then
            call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,     &
     &          iphys%i_magne, fl_prop1%coef_lor, mhd_fem_wk%magne_1)
!$omp parallel
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele%i_magne), cd_prop1%ex_magne,          &
     &          fem_wk%vector_1)
!$omp end parallel
!
            call fem_skv_vector_inertia_type(fluid%istack_ele_fld_smp,  &
     &          num_int, k2, mhd_fem_wk%magne_1, fem_wk%vector_1,       &
     &          ele, jac_3d, fem_wk%sk6)
          end if
!
!    set SGS Lorentz force
!
          if ( iflag_SGS_lorentz .ne. id_SGS_none) then
            if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys%i_magne, iphys%i_SGS_maxwell, fl_prop1%coef_lor,&
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
              call fem_skv_div_sgs_tensor                               &
     &           (fluid%istack_ele_fld_smp, num_int, k2, ifilter_final, &
     &            diff_coefs%num_field, iak_diff_lor, diff_coefs%ak,    &
     &            ele, jac_3d, FEM_elens, mhd_fem_wk%sgs_t1,            &
     &            fem_wk%tensor_1, fem_wk%sk6)
            else
              call tensor_cst_phys_2_each_ele                           &
     &           (node, ele, nod_fld, k2, iphys%i_SGS_maxwell,          &
     &            fl_prop1%coef_lor, fem_wk%tensor_1)
              call fem_skv_div_tensor                                   &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            ele, jac_3d, fem_wk%tensor_1, fem_wk%sk6)
            end if
          end if
!
        end if
!
!  --------  set coriolis force
!
        if ( iflag_4_coriolis .eq. id_FORCE_ele_int ) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld,           &
     &        k2, iphys%i_velo, fl_prop1%coef_cor, mhd_fem_wk%velo_1)
          call fem_skv_coriolis_type                                    &
     &       (fluid%istack_ele_fld_smp, num_int, k2,                    &
     &        mhd_fem_wk%velo_1, fl_prop1%sys_rot, ele, jac_3d,         &
     &        fem_wk%sk6)
        end if
!
! ---------  set buoyancy
!
        if(iflag_4_gravity .eq. id_FORCE_ele_int                        &
     &     .and. iflag_4_composit_buo .eq. id_FORCE_ele_int) then
          call set_double_gvec_each_ele                                 &
     &       (node, ele, nod_fld, k2, iphys%i_temp, iphys%i_light,      &
     &        fl_prop1%i_grav, fl_prop1%grav,                           &
     &        ak_MHD%ak_buo, ak_MHD%ak_comp_buo, fem_wk%vector_1)
          call fem_skv_vector_type                                      &
     &       (fluid%istack_ele_fld_smp, num_int, k2,                    &
     &        ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
        else if (iflag_4_gravity .eq. id_FORCE_ele_int) then
          call set_gravity_vec_each_ele(node, ele, nod_fld, k2,         &
     &        iphys%i_temp, fl_prop1%i_grav, fl_prop1%grav,             &
     &        ak_MHD%ak_buo, fem_wk%vector_1)
          call fem_skv_vector_type                                      &
     &       (fluid%istack_ele_fld_smp, num_int, k2,                    &
     &        ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
        else if (iflag_4_composit_buo .eq. id_FORCE_ele_int) then
          call set_gravity_vec_each_ele(node, ele, nod_fld, k2,         &
     &        iphys%i_light, fl_prop1%i_grav, fl_prop1%grav,            &
     &        ak_MHD%ak_comp_buo, fem_wk%vector_1)
          call fem_skv_vector_type                                      &
     &       (fluid%istack_ele_fld_smp, num_int, k2,                    &
     &        ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
        else if (iflag_4_filter_gravity .eq. id_FORCE_ele_int) then
          call set_gravity_vec_each_ele(node, ele, nod_fld, k2,         &
     &        iphys%i_filter_temp, fl_prop1%i_grav, fl_prop1%grav,      &
     &        ak_MHD%ak_buo, fem_wk%vector_1)
          call fem_skv_vector_type                                      &
     &       (fluid%istack_ele_fld_smp, num_int, k2,                    &
     &        ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
        end if
!
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_velo_pre_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_pre_ele_upwind                            &
     &         (node, ele, fluid, iphys, nod_fld, ak_MHD,               &
     &          ncomp_ele, ie_upw, d_ele, iphys_ele,                    &
     &          iak_diff_mf, iak_diff_lor, jac_3d, rhs_tbl, FEM_elens,  &
     &          diff_coefs, mhd_fem_wk, fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_cst_to_element
      use gravity_vec_each_ele
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nodal_fld_upw_type
      use fem_skv_vect_diff_upw_type
      use fem_skv_nonlinear_upw_type
      use fem_skv_div_sgs_flux_upw
      use fem_skv_lorentz_full_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: iak_diff_mf, iak_diff_lor
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      type(phys_address), intent(in) :: iphys_ele
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: k2, num_int
!
!  ---------  set number of integral points
!
      num_int = intg_point_t_evo
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
!
!  -----   set advection  --------
!
        if (fl_prop1%coef_nega_v .ne. 0.0d0) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &        iphys%i_velo, fl_prop1%coef_nega_v, mhd_fem_wk%velo_1)
!
!  -----  Inertia including Reynolds stress by rotation form --------
!
          if (iflag_4_rotate .eq. id_turn_ON) then
            if(iflag_SGS_inertia .ne. id_SGS_none                       &
     &        .and. iflag_commute_inertia .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &           iphys%i_velo, iphys%i_SGS_m_flux, fl_prop1%coef_nega_v,&
     &           mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
!
              call fem_skv_rot_inertia_upwind                           &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele%i_vort),         &
     &            d_ele(1,ie_upw), ele, jac_3d, fem_wk%sk6)
              call fem_skv_div_sgs_tensor_upwind                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2, ifilter_final, &
     &            diff_coefs%num_field, iak_diff_mf, diff_coefs%ak,     &
     &            ele, jac_3d, FEM_elens, d_ele(1,ie_upw),              &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1, fem_wk%sk6)
            else if(iflag_SGS_inertia .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(node, ele, nod_fld,       &
     &            k2, iphys%i_SGS_m_flux, fl_prop1%coef_nega_v,         &
     &            mhd_fem_wk%sgs_t1)
              call fem_skv_inertia_rot_sgs_upwind                       &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            ele, jac_3d, mhd_fem_wk%velo_1,                       &
     &            mhd_fem_wk%sgs_t1, d_ele(1,ie_upw), d_ele(1,ie_upw),  &
     &            fem_wk%sk6)
            else
              call fem_skv_rot_inertia_upwind                           &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele%i_vort),         &
     &            d_ele(1,ie_upw), ele, jac_3d, fem_wk%sk6)
            end if
!
!  -----  Inertia including Reynolds stress --------
!
          else
            if(iflag_SGS_inertia .ne. id_SGS_none                       &
     &        .and. iflag_commute_inertia .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &           iphys%i_velo, iphys%i_SGS_m_flux, fl_prop1%coef_nega_v,&
     &           mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
              call fem_skv_vec_inertia_msgs_upw                         &
     &           (fluid%istack_ele_fld_smp, num_int, k2, ifilter_final, &
     &            diff_coefs%num_field, iak_diff_mf, diff_coefs%ak,     &
     &            ele, jac_3d, FEM_elens, mhd_fem_wk%velo_1,            &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1,                   &
     &            d_ele(1,iphys_ele%i_velo), d_ele(1,ie_upw),           &
     &            fem_wk%sk6)
            else if(iflag_SGS_inertia .ne. id_SGS_none) then
              call tensor_cst_phys_2_each_ele(node, ele, nod_fld,       &
     &            k2, iphys%i_SGS_m_flux, fl_prop1%coef_nega_v,         &
     &            mhd_fem_wk%sgs_t1)
              call fem_skv_vcl_inertia_sgs_upwind                       &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            ele, jac_3d, mhd_fem_wk%velo_1,                       &
     &            mhd_fem_wk%sgs_t1, d_ele(1,iphys_ele%i_velo),         &
     &            d_ele(1,ie_upw), fem_wk%sk6)
            else
              call fem_skv_vector_inertia_upwind                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            mhd_fem_wk%velo_1, d_ele(1,iphys_ele%i_velo),         &
     &            d_ele(1,ie_upw), ele, jac_3d, fem_wk%sk6)
            end if
          end if
!
!    set Reynolds stress
!
          if ( iflag_SGS_inertia .ne. id_SGS_none) then
            if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &           iphys%i_velo, iphys%i_SGS_m_flux, fl_prop1%coef_nega_v,&
     &           mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
              call fem_skv_div_sgs_tensor_upwind                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2, ifilter_final, &
     &            diff_coefs%num_field, iak_diff_mf, diff_coefs%ak,     &
     &            ele, jac_3d, FEM_elens, d_ele(1,ie_upw),              &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1, fem_wk%sk6)
            else
              call tensor_cst_phys_2_each_ele(node, ele, nod_fld,       &
     &            k2, iphys%i_SGS_m_flux, fl_prop1%coef_nega_v,         &
     &            fem_wk%tensor_1)
              call fem_skv_div_tsr_upw                                  &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            d_ele(1,ie_upw), ele, jac_3d, fem_wk%tensor_1,        &
     &            fem_wk%sk6)
            end if
          end if
        end if
!
!  -----   set Lorentz force  --------
!
        if (iflag_4_lorentz .gt. id_turn_OFF) then
          if (iflag_4_rotate .eq. id_turn_ON) then
            call vector_cst_phys_2_each_ele(node, ele, nod_fld,         &
     &          k2, iphys%i_vecp, fl_prop1%coef_lor, mhd_fem_wk%vecp_1)
!$omp parallel
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele%i_magne), cd_prop1%ex_magne,          &
     &          fem_wk%vector_1)
!$omp end parallel
!
            call fem_skv_lorentz_rot_galerkin                           &
     &         (fluid%istack_ele_fld_smp, num_int, k2,                  &
     &          mhd_fem_wk%vecp_1, fem_wk%vector_1,                     &
     &          ele, jac_3d, fem_wk%sk6)
          else
            call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,     &
     &          iphys%i_magne, fl_prop1%coef_lor, mhd_fem_wk%magne_1)
!$omp parallel
            call add_const_to_vector_smp(ele%numele,                    &
     &          d_ele(1,iphys_ele%i_magne), cd_prop1%ex_magne,          &
     &          fem_wk%vector_1)
!$omp end parallel
!
            call fem_skv_vector_inertia_upwind                          &
     &         (fluid%istack_ele_fld_smp, num_int, k2,                  &
     &          mhd_fem_wk%magne_1, fem_wk%vector_1,                    &
     &          d_ele(1,ie_upw), ele, jac_3d, fem_wk%sk6)
          end if
!
!    set SGS Lorentz force
!
          if ( iflag_SGS_lorentz .ne. id_SGS_none) then
            if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
              call SGS_const_tensor_each_ele(node, ele, nod_fld, k2,    &
     &            iphys%i_magne, iphys%i_SGS_maxwell, fl_prop1%coef_lor,&
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
              call fem_skv_div_sgs_tensor_upwind                        &
     &           (fluid%istack_ele_fld_smp, num_int, k2, ifilter_final, &
     &            diff_coefs%num_field, iak_diff_lor, diff_coefs%ak,    &
     &            ele, jac_3d, FEM_elens, d_ele(1,ie_upw),              &
     &            mhd_fem_wk%sgs_t1, fem_wk%tensor_1, fem_wk%sk6)
            else
              call tensor_cst_phys_2_each_ele                           &
     &           (node, ele, nod_fld, k2, iphys%i_SGS_maxwell,          &
     &            fl_prop1%coef_lor, fem_wk%tensor_1)
              call fem_skv_div_tsr_upw                                  &
     &           (fluid%istack_ele_fld_smp, num_int, k2,                &
     &            d_ele(1,ie_upw), ele, jac_3d, fem_wk%tensor_1,        &
     &            fem_wk%sk6)
            end if
          end if
        end if
!
!  --------  set coriolis force
!
        if ( iflag_4_coriolis .eq. id_FORCE_ele_int ) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld,           &
     &        k2, iphys%i_velo, fl_prop1%coef_cor, mhd_fem_wk%velo_1)
          call fem_skv_coriolis_upwind(fluid%istack_ele_fld_smp,        &
     &        num_int, k2, mhd_fem_wk%velo_1, fl_prop1%sys_rot,         &
     &        d_ele(1,ie_upw), ele, jac_3d, fem_wk%sk6)
        end if
!
! ---------  set buoyancy
!
        if(iflag_4_gravity .eq. id_FORCE_ele_int                        &
     &     .and. iflag_4_composit_buo .eq. id_FORCE_ele_int) then
          call set_double_gvec_each_ele                                 &
     &       (node, ele, nod_fld, k2, iphys%i_temp, iphys%i_light,      &
     &        fl_prop1%i_grav, fl_prop1%grav,                           &
     &        ak_MHD%ak_buo, ak_MHD%ak_comp_buo, fem_wk%vector_1)
          call fem_skv_vector_field_upwind(fluid%istack_ele_fld_smp,    &
     &        num_int, k2, d_ele(1,ie_upw), ele, jac_3d,                &
     &        fem_wk%vector_1, fem_wk%sk6)
        else if (iflag_4_gravity .eq. id_FORCE_ele_int) then
          call set_gravity_vec_each_ele(node, ele, nod_fld, k2,         &
     &        iphys%i_temp, fl_prop1%i_grav, fl_prop1%grav,             &
     &        ak_MHD%ak_buo, fem_wk%vector_1)
          call fem_skv_vector_field_upwind(fluid%istack_ele_fld_smp,    &
     &        num_int, k2, d_ele(1,ie_upw), ele, jac_3d,                &
     &        fem_wk%vector_1, fem_wk%sk6)
        else if (iflag_4_composit_buo .eq. id_FORCE_ele_int) then
          call set_gravity_vec_each_ele(node, ele, nod_fld, k2,         &
     &        iphys%i_light, fl_prop1%i_grav, fl_prop1%grav,            &
     &        ak_MHD%ak_comp_buo, fem_wk%vector_1)
          call fem_skv_vector_field_upwind(fluid%istack_ele_fld_smp,    &
     &        num_int, k2, d_ele(1,ie_upw), ele, jac_3d,                &
     &        fem_wk%vector_1, fem_wk%sk6)
        else if (iflag_4_filter_gravity .eq. id_FORCE_ele_int) then
          call set_gravity_vec_each_ele(node, ele, nod_fld, k2,         &
     &        iphys%i_filter_temp, fl_prop1%i_grav, fl_prop1%grav,      &
     &        ak_MHD%ak_buo, fem_wk%vector_1)
          call fem_skv_vector_field_upwind(fluid%istack_ele_fld_smp,    &
     &        num_int, k2, d_ele(1,ie_upw), ele,jac_3d,                 &
     &        fem_wk%vector_1, fem_wk%sk6)
        end if
!
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_velo_pre_ele_upwind
!
!-----------------------------------------------------------------------
!
      end module int_vol_velo_pre
