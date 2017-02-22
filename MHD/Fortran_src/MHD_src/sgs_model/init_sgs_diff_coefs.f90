!
!     module init_sgs_diff_coefs
!
!      Written by H. Matsui on 2004
!      Modified by H. Matsui on July, 2007
!
!!      subroutine define_sgs_diff_coefs                                &
!!     &         (numele, SGS_param, cmt_param, layer_tbl, fl_prop,     &
!!     &          ifld_diff, icomp_diff, wk_diff, diff_coefs)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(SGS_terms_address), intent(inout) :: ifld_sgs
!!        type(SGS_terms_address), intent(inout) :: icomp_sgs
!!        type(SGS_terms_address), intent(inout) :: ifld_diff
!!        type(SGS_terms_address), intent(inout) :: icomp_diff
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      module init_sgs_diff_coefs
!
      use m_precision
      use m_machine_parameter
      use t_time_stepping_parameter
      use t_SGS_control_parameter
      use t_physical_property
!
      implicit none
!
      private :: count_sgs_diff_coefs, set_sgs_diff_addresses
      private :: check_sgs_diff_addresses
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine define_sgs_diff_coefs                                  &
     &         (numele, SGS_param, cmt_param, layer_tbl, fl_prop,       &
     &          ifld_diff, icomp_diff, wk_diff, diff_coefs)
!
      use calypso_mpi
      use m_phys_labels
!
      use m_control_parameter
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: numele
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(layering_tbl), intent(in) :: layer_tbl
      type(fluid_property), intent(in) :: fl_prop
!
      type(SGS_terms_address), intent(inout) :: ifld_diff, icomp_diff
!
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer(kind = kint) :: ntot_diff_comp
!
!
      call count_sgs_diff_coefs                                         &
     &   (evo_magne, evo_vect_p, evo_temp, evo_comp,                    &
     &    SGS_param, cmt_param, fl_prop, ntot_diff_comp, diff_coefs)
      call alloc_sgs_coefs_layer(layer_tbl%e_grp%num_grp,               &
     &    diff_coefs%num_field, ntot_diff_comp, wk_diff)
!
      call alloc_SGS_num_coefs(diff_coefs)
      call alloc_SGS_coefs(numele, diff_coefs)
!
      call set_sgs_diff_addresses                                       &
     &   (evo_magne, evo_vect_p, evo_temp, evo_comp,                    &
     &    SGS_param, cmt_param, fl_prop, ifld_diff, icomp_diff,         &
     &    wk_diff, diff_coefs)
      diff_coefs%ntot_comp = diff_coefs%num_field
!
      if(iflag_debug .gt. 0) call check_sgs_diff_addresses              &
     &                     (ifld_diff, icomp_diff, wk_diff, diff_coefs)
!
!
      end subroutine define_sgs_diff_coefs
!
!  ------------------------------------------------------------------
!
      subroutine count_sgs_diff_coefs                                   &
     &         (evo_B, evo_A, evo_T, evo_C, SGS_param, cmt_param,       &
     &          fl_prop, ntot_diff_comp, diff_coefs)
!
      use calypso_mpi
      use m_phys_labels
!
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_model_coefs
!
      type(time_evolution_params), intent(in) :: evo_B, evo_A
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(fluid_property), intent(in) :: fl_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      integer(kind = kint), intent(inout) :: ntot_diff_comp
!
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!    count coefficients for SGS terms
!
      diff_coefs%num_field = 0
      ntot_diff_comp = 0
      if (evo_T%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_h_flux .ne. id_SGS_none) then
          if (cmt_param%iflag_c_hf .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 3
          end if
        end if
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_m_flux .ne. id_SGS_none) then
          if (cmt_param%iflag_c_mf .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 9
          end if
        end if
!
        if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
          if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 9
          end if
        end if
      end if
!
      if (evo_B%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          if(cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 9
          end if
        end if
      end if
!
      if (evo_C%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_c_flux .ne. id_SGS_none) then
          if (cmt_param%iflag_c_cf .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 3
          end if
        end if
      end if
!
      if (evo_T%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &      .and. cmt_param%iflag_c_temp .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 3
        end if
      end if
!
      if (evo_C%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &      .and. cmt_param%iflag_c_light .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 3
        end if
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &       .and. cmt_param%iflag_c_velo .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 9
        end if
      end if
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &      .and. cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 9
        end if
      end if
!
      if (evo_B%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .gt. id_SGS_none                         &
     &      .and. cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 9
        end if
      end if
!
      end subroutine count_sgs_diff_coefs
!
!  ------------------------------------------------------------------
!
      subroutine set_sgs_diff_addresses                                 &
     &         (evo_B, evo_A, evo_T, evo_C, SGS_param, cmt_param,       &
     &          fl_prop, ifld_diff, icomp_diff, wk_diff, diff_coefs)
!
      use calypso_mpi
      use m_phys_labels
!
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_model_coefs
!
      type(time_evolution_params), intent(in) :: evo_B, evo_A
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(fluid_property), intent(in) :: fl_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(SGS_terms_address), intent(inout) :: ifld_diff, icomp_diff
!
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer(kind = kint) :: id, jd
!
!
       id = 1
       jd = 1
       if (evo_T%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_h_flux .ne. id_SGS_none) then
           if (cmt_param%iflag_c_hf .eq. id_SGS_commute_ON) then
             icomp_diff%i_heat_flux = id
             ifld_diff%i_heat_flux =  jd
             wk_diff%name(jd) = fhd_SGS_h_flux
             diff_coefs%num_comps(jd) = 3
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
       end if
!
       if (fl_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_m_flux .ne. id_SGS_none) then
           if (cmt_param%iflag_c_mf .eq. id_SGS_commute_ON) then
             icomp_diff%i_mom_flux = id
             ifld_diff%i_mom_flux = jd
             wk_diff%name(jd) = fhd_SGS_m_flux
             diff_coefs%num_comps(jd) = 9
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
!
         if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
           if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
             icomp_diff%i_lorentz = id
             ifld_diff%i_lorentz = jd
             wk_diff%name(jd) = fhd_SGS_Lorentz
             diff_coefs%num_comps(jd) = 9
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
       end if
!
       if (evo_B%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
           if (cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
             icomp_diff%i_induction = id
             ifld_diff%i_induction =  jd
             wk_diff%name(jd) = fhd_SGS_induction
             diff_coefs%num_comps(jd) = 9
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
       end if
!
       if (evo_C%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_c_flux .ne. id_SGS_none) then
           if(cmt_param%iflag_c_cf .eq. id_SGS_commute_ON) then
             icomp_diff%i_comp_flux = id
             ifld_diff%i_comp_flux =  jd
             wk_diff%name(jd) = fhd_SGS_c_flux
             diff_coefs%num_comps(jd) = 3
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
       end if
!
!
      if (evo_T%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. cmt_param%iflag_c_temp .eq. id_SGS_commute_ON) then
            icomp_diff%i_temp = id
            ifld_diff%i_temp = jd
            wk_diff%name(jd) = fhd_temp
            diff_coefs%num_comps(jd) = 3
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      if (evo_C%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. cmt_param%iflag_c_light .eq. id_SGS_commute_ON) then
            icomp_diff%i_light = id
            ifld_diff%i_light = jd
            wk_diff%name(jd) = fhd_light
            diff_coefs%num_comps(jd) = 3
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. cmt_param%iflag_c_velo .eq. id_SGS_commute_ON) then
            icomp_diff%i_velo = id
            ifld_diff%i_velo = jd
            wk_diff%name(jd) = fhd_velo
            diff_coefs%num_comps(jd) = 9
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
            icomp_diff%i_magne = id
            ifld_diff%i_magne = jd
            wk_diff%name(jd) = fhd_magne
            diff_coefs%num_comps(jd) = 9
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      if (evo_B%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
            icomp_diff%i_magne = id
            ifld_diff%i_magne = jd
            wk_diff%name(jd) = fhd_magne
            diff_coefs%num_comps(jd) = 9
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
       diff_coefs%istack_comps(0) = 0
       do id = 1, diff_coefs%num_field
         diff_coefs%istack_comps(id) = diff_coefs%istack_comps(id-1)    &
     &                               + diff_coefs%num_comps(id)
       end do
!
      end subroutine set_sgs_diff_addresses
!
!  ------------------------------------------------------------------
!
      subroutine check_sgs_diff_addresses                               &
     &         (ifld_diff, icomp_diff, wk_diff, diff_coefs)
!
      use calypso_mpi
!
      use t_ele_info_4_dynamic
      use t_SGS_model_coefs
!
!
      type(SGS_terms_address), intent(inout) :: ifld_diff, icomp_diff
!
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
        write(*,*) 'wk_diff%ntot_comp', wk_diff%ntot_comp
        write(*,*) 'diff_coefs%num_field', diff_coefs%num_field
!
        if(ifld_diff%i_heat_flux .gt. 0) then
          write(*,*) 'iak_diff_hf',                                     &
     &             ifld_diff%i_heat_flux, icomp_diff%i_heat_flux,       &
     &             diff_coefs%num_comps(ifld_diff%i_heat_flux),         &
     &             trim(wk_diff%name(ifld_diff%i_heat_flux))
        end if
        if(ifld_diff%i_mom_flux .gt. 0) then
          write(*,*) 'iak_diff_mf',                                     &
     &             ifld_diff%i_mom_flux, icomp_diff%i_mom_flux,         &
     &             diff_coefs%num_comps(ifld_diff%i_mom_flux),          &
     &             trim(wk_diff%name(ifld_diff%i_mom_flux))
        end if
        if(ifld_diff%i_lorentz .gt. 0) then
          write(*,*) 'iak_diff_lor',                                    &
     &             ifld_diff%i_lorentz, icomp_diff%i_lorentz,           &
     &             diff_coefs%num_comps(ifld_diff%i_lorentz),           &
     &             trim(wk_diff%name(ifld_diff%i_lorentz))
        end if
        if(ifld_diff%i_induction .gt. 0) then
          write(*,*) 'iak_diff_uxb',                                    &
     &             ifld_diff%i_induction, icomp_diff%i_induction,       &
     &             diff_coefs%num_comps(ifld_diff%i_induction),         &
     &             trim(wk_diff%name(ifld_diff%i_induction))
        end if
!
        if(ifld_diff%i_temp .gt. 0) then
          write(*,*) 'iak_diff_t',                                      &
     &             ifld_diff%i_temp, icomp_diff%i_temp,                 &
     &             diff_coefs%num_comps(ifld_diff%i_temp),              &
     &             trim(wk_diff%name(ifld_diff%i_temp))
        end if
        if(ifld_diff%i_velo .gt. 0) then
          write(*,*) 'iak_diff_v', ifld_diff%i_velo, icomp_diff%i_velo, &
     &             diff_coefs%num_comps(ifld_diff%i_velo),              &
     &             trim(wk_diff%name(ifld_diff%i_velo))
        end if
        if(ifld_diff%i_magne .gt. 0) then
          write(*,*) 'iak_diff_b',                                      &
     &             ifld_diff%i_magne, icomp_diff%i_magne,               &
     &             diff_coefs%num_comps(ifld_diff%i_magne),             &
     &             trim(wk_diff%name(ifld_diff%i_magne))
        end if
        if(ifld_diff%i_light .gt. 0) then
          write(*,*) 'iak_diff_c',                                      &
     &             ifld_diff%i_light, icomp_diff%i_light,               &
     &             diff_coefs%num_comps(ifld_diff%i_light),             &
     &             trim(wk_diff%name(ifld_diff%i_light))
        end if
!
      end subroutine check_sgs_diff_addresses
!
! -------------------------------------------------------------------
!
      end module init_sgs_diff_coefs
