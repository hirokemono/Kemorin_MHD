!cal_sgs_m_flux_sgs_buo.f90
!      module cal_sgs_m_flux_sgs_buo
!
!      written by H. Matsui on Aug., 2007
!
!      subroutine cal_sgs_mom_flux_with_sgs_buo
!
      module cal_sgs_m_flux_sgs_buo
!
      use m_precision
!
      use m_phys_constants
      use m_machine_parameter
!
      implicit none
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_mom_flux_with_sgs_buo
!
      use m_control_parameter
      use m_phys_constants
      use m_node_phys_address
      use m_SGS_address
      use m_node_phys_data
      use m_physical_property
      use m_ele_info_4_dynamical
      use m_work_4_dynamic_model
!
      use cal_sgs_fluxes
      use cal_momentum_terms
      use products_nodal_fields_smp
      use sgs_buoyancy_flux
      use merge_dynamic_coefs
      use set_sgs_diff_model_coefs
      use int_rms_ave_ele_grps_1st
!
      integer(kind = kint), parameter :: ncomp_sgs_buo= 6
!      integer(kind = kint) :: i
!
!
!   lead SGS momentum flux using original model coefficient
!
      call set_model_coefs_2_ele(itype_SGS_m_flux_coef, n_sym_tensor,   &
     &    iak_sgs_mf, icomp_sgs_mf)
!
      call cal_sgs_momentum_flux
!
!   lead work of Reynolds stress
!
      call cal_terms_4_momentum(iphys%i_SGS_div_m_flux)
!
!$omp parallel
      call cal_phys_dot_product(iphys%i_velo, iphys%i_SGS_div_m_flux,   &
     &    iphys%i_reynolds_wk)
!$omp end parallel
!
!   lead SGS buoyancy flux
!
      if(iflag_4_gravity .gt. id_turn_OFF) then
        call cal_SGS_gravity_flux(coef_buo, iphys%i_SGS_h_flux,         &
     &      iphys%i_SGS_buo_wk)
      end if
      if(iflag_4_composit_buo .gt. id_turn_OFF) then
        call cal_SGS_gravity_flux(coef_comp_buo, iphys%i_SGS_c_flux,    &
     &      iphys%i_SGS_comp_buo_wk)
       end if
!
!   take RMS of SGS buoyancy flux and work of Reynolds stress
!
      if(iflag_4_gravity .gt. id_turn_OFF) then
        call int_vol_2rms_ave_ele_grps_1st(intg_point_t_evo,            &
     &      n_layer_d, n_item_layer_d, layer_stack, item_layer,         &
     &      d_nod(1,iphys%i_reynolds_wk), d_nod(1,iphys%i_SGS_buo_wk),  &
     &      sgs_l(1,1), sgs_l(1,4), sgs_l(1,2), sgs_l(1,5) )
!
        if(iflag_4_composit_buo .gt. id_turn_OFF) then
          call int_vol_rms_ave_ele_grps_1st(intg_point_t_evo,           &
     &        n_layer_d, n_item_layer_d, layer_stack, item_layer,       &
     &        d_nod(1,iphys%i_SGS_comp_buo_wk), sgs_l(1,3), sgs_l(1,6))
        end if
      else if(iflag_4_composit_buo .gt. id_turn_OFF) then
        call int_vol_2rms_ave_ele_grps_1st(intg_point_t_evo,            &
     &      n_layer_d, n_item_layer_d, layer_stack,                     &
     &      item_layer, d_nod(1,iphys%i_reynolds_wk),                   &
     &      d_nod(1,iphys%i_SGS_comp_buo_wk), sgs_l(1,1), sgs_l(1,4),   &
     &      sgs_l(1,3), sgs_l(1,6) )
      end if
!
!
      call lsq_model_coefs_4_comps(ncomp_sgs_buo)
!
!   Parameterize model coeffisient including SGS Buoyancy
!
      if(iflag_4_gravity .gt. id_turn_OFF) then
!        call cal_Csim_buo_by_Reynolds_ratio(n_layer_d, ifive,          &
!     &      sgs_c_coef(1,icomp_sgs_tbuo), sgs_f_coef(1,iak_sgs_tbuo) )
        call single_Csim_buo_by_mf_ratio(n_layer_d, ifive,              &
     &      sgs_c_coef(1,icomp_sgs_tbuo), sgs_f_coef(1,iak_sgs_tbuo) )
        call clippging_sgs_coefs(ncomp_sgs_buo,                         &
     &      iak_sgs_tbuo, icomp_sgs_tbuo)
      end if
      if(iflag_4_composit_buo .gt. id_turn_OFF) then
!        call cal_Csim_buo_by_Reynolds_ratio(n_layer_d, isix,           &
!     &      sgs_c_coef(1,icomp_sgs_cbuo), sgs_f_coef(1,iak_sgs_cbuo) )
        call single_Csim_buo_by_mf_ratio(n_layer_d, isix,               &
     &      sgs_c_coef(1,icomp_sgs_cbuo), sgs_f_coef(1,iak_sgs_cbuo) )
        call clippging_sgs_coefs(ncomp_sgs_buo,                         &
     &      iak_sgs_tbuo, icomp_sgs_tbuo)
      end if
!
!      if(iflag_debug .gt. 0) then
!        write(*,*) 'sgs_f_coef, icomp_sgs_tbuo', iak_sgs_tbuo
!        do i = 1, n_layer_d
!          write(*,'(i10,1pe20.12)') i, sgs_f_coef(i,iak_sgs_tbuo)
!        end do
!        write(*,*) 'sgs_f_coef, icomp_sgs_tbuo', icomp_sgs_cbuo
!        do i = 1, n_layer_d
!          write(*,'(i10,1p6e20.12)') i,                                &
!     &              sgs_c_coef(i,icomp_sgs_tbuo:icomp_sgs_tbuo+5)
!        end do
!      end if
!
      end subroutine cal_sgs_mom_flux_with_sgs_buo
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_m_flux_sgs_buo
