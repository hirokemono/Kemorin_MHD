!
!      module int_vol_diffusion_ele
!
! numerical integration for diffusion terms (Lapracians)
!      Written by H. Matsui on July, 2005
!
!      subroutine int_vol_viscosity_ele
!      subroutine int_vol_vecp_diffuse_ele
!      subroutine int_vol_magne_diffuse_ele
!      subroutine int_vol_termal_diffuse_ele
!      subroutine int_vol_p_termal_diffuse_ele
!      subroutine int_vol_composition_diffuse_ele
!
!      subroutine int_vol_viscous_ele_monitor
!      subroutine int_vol_vp_diffuse_ele_monitor
!      subroutine int_vol_m_diffuse_ele_monitor
!      subroutine int_vol_t_diffuse_ele_monitor
!      subroutine int_vol_ds_diffuse_ele_monitor
!
!      subroutine int_vol_viscosity_co
!      subroutine int_vol_magne_diffuse_co
!      subroutine int_vol_vecp_diffuse_co
!
      module int_vol_diffusion_ele
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use m_physical_property
      use m_ele_material_property
      use m_control_parameter
      use m_t_int_parameter
!
      implicit none
!
      private :: int_vol_scalar_diffuse_ele, int_vol_vector_diffuse_ele
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_viscosity_ele
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
!
      if (coef_velo.gt.zero .and. coef_exp_v.gt.zero) then
        call int_vol_vector_diffuse_ele(iele_fl_smp_stack,              &
     &      iak_diff_v, coef_exp_v, ak_d_velo, iphys%i_velo)
      end if
!
      end subroutine int_vol_viscosity_ele

!  ---------------------------------------------------------------------
!
      subroutine int_vol_vecp_diffuse_ele
!
      use m_node_phys_address
      use m_geometry_parameter
      use m_SGS_address
!
!
      if (coef_magne.gt.zero .and. coef_exp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(iele_smp_stack,                 &
     &      iak_diff_b, coef_exp_b, ak_d_magne, iphys%i_vecp)
      end if
!
      end subroutine int_vol_vecp_diffuse_ele
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_magne_diffuse_ele
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
!
      if (coef_magne.gt.zero .and. coef_exp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(iele_cd_smp_stack,              &
     &      iak_diff_b, coef_exp_b, ak_d_magne, iphys%i_magne)
      end if
!
      end subroutine int_vol_magne_diffuse_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_termal_diffuse_ele
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
      if (coef_temp.gt.zero .and. coef_exp_t.gt.zero) then
        call int_vol_scalar_diffuse_ele(iele_fl_smp_stack,              &
     &      iak_diff_t, coef_exp_t, ak_d_temp, iphys%i_temp)
      end if
!
      end subroutine int_vol_termal_diffuse_ele
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_p_termal_diffuse_ele
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
      if (coef_temp.gt.zero .and. coef_exp_t.gt.zero) then
        call int_vol_scalar_diffuse_ele(iele_fl_smp_stack,              &
     &      iak_diff_t, coef_exp_t, ak_d_temp, iphys%i_par_temp)
      end if
!
      end subroutine int_vol_p_termal_diffuse_ele
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_composition_diffuse_ele
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
      if (coef_scalar.gt.zero .and. coef_exp_c.gt.zero) then
        call int_vol_scalar_diffuse_ele(iele_fl_smp_stack,              &
     &      iak_diff_c, coef_exp_c, ak_d_composit, iphys%i_light)
      end if
!
      end subroutine int_vol_composition_diffuse_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_viscous_ele_monitor
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
      call int_vol_vector_diffuse_ele(iele_fl_smp_stack,                &
     &    iak_diff_v, one, ak_d_velo, iphys%i_velo)
!
      end subroutine int_vol_viscous_ele_monitor
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_vp_diffuse_ele_monitor
!
      use m_node_phys_address
      use m_geometry_parameter
      use m_SGS_address
!
!
      call int_vol_vector_diffuse_ele(iele_smp_stack,                   &
     &    iak_diff_b, one, ak_d_magne, iphys%i_vecp)
!
      end subroutine int_vol_vp_diffuse_ele_monitor
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_m_diffuse_ele_monitor
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
!
      call int_vol_vector_diffuse_ele(iele_cd_smp_stack,                &
     &    iak_diff_b, one, ak_d_magne, iphys%i_magne)
!
      end subroutine int_vol_m_diffuse_ele_monitor
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_t_diffuse_ele_monitor
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
      call int_vol_scalar_diffuse_ele(iele_fl_smp_stack,                &
     &      iak_diff_t, one, ak_d_temp, iphys%i_temp)
!
      end subroutine int_vol_t_diffuse_ele_monitor
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_ds_diffuse_ele_monitor
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
      call int_vol_scalar_diffuse_ele(iele_fl_smp_stack,                &
     &      iak_diff_c, one, ak_d_composit, iphys%i_light)
!
      end subroutine int_vol_ds_diffuse_ele_monitor
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_viscosity_co
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
      if (coef_imp_v.gt.zero) then
        call int_vol_vector_diffuse_ele(iele_fl_smp_stack,              &
     &      iak_diff_v, coef_imp_v, ak_d_velo, iphys%i_velo)
      end if
!
      end subroutine int_vol_viscosity_co
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_vecp_diffuse_co
!
      use m_node_phys_address
      use m_geometry_parameter
      use m_SGS_address
!
      if (coef_imp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(iele_smp_stack,                 &
     &      iak_diff_b, coef_imp_b, ak_d_magne, iphys%i_vecp)
      end if
!
      end subroutine int_vol_vecp_diffuse_co
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_magne_diffuse_co
!
      use m_node_phys_address
      use m_geometry_data_MHD
      use m_SGS_address
!
      if (coef_imp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(iele_cd_smp_stack,              &
     &      iak_diff_b, coef_imp_b, ak_d_magne, iphys%i_magne)
      end if
!
      end subroutine int_vol_magne_diffuse_co
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_scalar_diffuse_ele(iele_fsmp_stack,            &
     &          iak_diff, coef_crank, ak_d, i_scalar)
!
      use m_machine_parameter
      use m_geometry_parameter
      use int_vol_fractional_1st
      use int_vol_sgs_fractional_1st
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: i_scalar, iak_diff
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(numele)
!
!
      if (iak_diff .gt. 0) then
        call int_vol_scalar_sgs_diffuse_1st(iele_fsmp_stack,            &
     &      intg_point_t_evo, coef_crank, ak_d, i_scalar,               &
     &      n_filter_final, iak_diff)
      else
        call int_vol_scalar_diffuse_1st(iele_fsmp_stack,                &
     &      intg_point_t_evo, coef_crank, ak_d, i_scalar)
      end if
!
      end subroutine int_vol_scalar_diffuse_ele
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_vector_diffuse_ele(iele_fsmp_stack,            &
     &          iak_diff, coef_crank, ak_d, i_vector)
!
      use m_machine_parameter
      use m_geometry_parameter
      use int_vol_fractional_1st
      use int_vol_sgs_fractional_1st
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: i_vector, iak_diff
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(numele)
!
!
      if (iak_diff .gt. 0) then
        call int_vol_vector_sgs_diffuse_1st(iele_fsmp_stack,            &
     &      intg_point_t_evo, coef_crank, ak_d, i_vector,               &
     &      n_filter_final, iak_diff)
      else
        call int_vol_vector_diffuse_1st(iele_fsmp_stack,                &
     &      intg_point_t_evo, coef_crank, ak_d, i_vector)
      end if
!
      end subroutine int_vol_vector_diffuse_ele
!
!  ---------------------------------------------------------------------
!
      end module int_vol_diffusion_ele
