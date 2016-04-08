!
!     module count_sgs_components
!
!      Written by H. Matsui on 2004
!      Modified by H. Matsui on July, 2007
!
!!      subroutine s_count_sgs_components(numnod, numele, layer_tbl,    &
!!     &          ifld_sgs, icomp_sgs, ifld_diff, icomp_diff,           &
!!     &         wk_sgs, wk_diff, sgs_coefs, sgs_coefs_nod, diff_coefs)
!!      subroutine set_SGS_addresses(iphys_elediff)
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(SGS_terms_address), intent(inout) :: ifld_sgs
!!        type(SGS_terms_address), intent(inout) :: icomp_sgs
!!        type(SGS_terms_address), intent(inout) :: ifld_diff
!!        type(SGS_terms_address), intent(inout) :: icomp_diff
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(MHD_coefficients_type), intent(inout) :: sgs_coefs
!!        type(MHD_coefficients_type), intent(inout) :: sgs_coefs_nod
!!        type(MHD_coefficients_type), intent(inout) :: diff_coefs
!
      module count_sgs_components
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_count_sgs_components(numnod, numele, layer_tbl,      &
     &          ifld_sgs, icomp_sgs, ifld_diff, icomp_diff,             &
     &          wk_sgs, wk_diff, sgs_coefs, sgs_coefs_nod, diff_coefs)
!
      use calypso_mpi
      use m_phys_labels
      use m_control_parameter
!
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(SGS_terms_address), intent(inout) :: ifld_sgs, icomp_sgs
      type(SGS_terms_address), intent(inout) :: ifld_diff, icomp_diff
!
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
      type(MHD_coefficients_type), intent(inout) :: sgs_coefs
      type(MHD_coefficients_type), intent(inout) :: sgs_coefs_nod
      type(MHD_coefficients_type), intent(inout) :: diff_coefs
!
      integer(kind = kint) :: ntot_diff_comp
      integer(kind = kint) :: i, j, id, jd
!
!
!    count coefficients for SGS terms
!
      sgs_coefs%num_field = 0
      sgs_coefs%ntot_comp = 0
      diff_coefs%num_field = 0
      ntot_diff_comp = 0
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        if (iflag_SGS_heat .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
          if (iflag_commute_heat .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 3
          end if
        end if
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if (iflag_SGS_inertia .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 6
          if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 9
          end if
        end if
!
        if (iflag_SGS_lorentz .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 6
          if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 9
          end if
        end if
!
        if (iflag_SGS_gravity .ne. id_SGS_none) then
          if(iflag_4_gravity .gt. id_turn_OFF) then
            sgs_coefs%num_field = sgs_coefs%num_field + 1
            sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 6
          end if
          if(iflag_4_composit_buo .gt. id_turn_OFF) then
            sgs_coefs%num_field = sgs_coefs%num_field + 1
            sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 6
          end if
        end if
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if (iflag_SGS_induction .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
        end if
      end if
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        if (iflag_SGS_induction .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
          if(iflag_commute_induction .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 9
          end if
        end if
      end if
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        if (iflag_SGS_comp_flux .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
          if (iflag_commute_c_flux .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 3
          end if
        end if
      end if
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        if(iflag_SGS_model .ne. id_SGS_none                             &
     &      .and. iflag_commute_temp .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 3
        end if
      end if
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        if(iflag_SGS_model .ne. id_SGS_none                             &
     &      .and. iflag_commute_composit .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 3
        end if
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if(iflag_SGS_model .ne. id_SGS_none                             &
     &        .and. iflag_commute_velo .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 9
        end if
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if(iflag_SGS_model .ne. id_SGS_none                             &
     &       .and. iflag_commute_magne .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 9
        end if
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        if(iflag_SGS_model .gt. id_SGS_none                             &
     &      .and. iflag_commute_magne .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 9
        end if
      end if
!
!   set index for model coefficients
!
      call alloc_sgs_coefs_layer(layer_tbl%e_grp%num_grp,               &
     &    sgs_coefs%num_field, sgs_coefs%ntot_comp, wk_sgs)
      call alloc_sgs_coefs_layer(layer_tbl%e_grp%num_grp,               &
     &    diff_coefs%num_field, ntot_diff_comp, wk_diff)
!
      call alloc_MHD_num_coefs(sgs_coefs)
      call alloc_MHD_coefs(numele, sgs_coefs)
!
      call alloc_MHD_num_coefs(diff_coefs)
      call alloc_MHD_coefs(numele, diff_coefs)
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                     &
     &      .or. iflag_SGS_model.eq.id_SGS_similarity)  then
        call copy_MHD_num_coefs(sgs_coefs, sgs_coefs_nod)
        call alloc_MHD_coefs(numnod, sgs_coefs_nod)
      end if
!
       i = 1
       j = 1
       id = 1
       jd = 1
       if (iflag_t_evo_4_temp .gt. id_no_evolution) then
         if (iflag_SGS_heat .ne. id_SGS_none) then
           icomp_sgs%i_heat_flux = i
           ifld_sgs%i_heat_flux =  j
           wk_sgs%name(j) = fhd_SGS_h_flux
           sgs_coefs%num_comps(j) = 3
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
           if (iflag_commute_heat .eq. id_SGS_commute_ON) then
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
       if (iflag_t_evo_4_velo .gt. id_no_evolution) then
         if (iflag_SGS_inertia .ne. id_SGS_none) then
           icomp_sgs%i_mom_flux = i
           ifld_sgs%i_mom_flux =  j
           wk_sgs%name(j) = fhd_SGS_m_flux
           sgs_coefs%num_comps(j) = 6
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
           if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
             icomp_diff%i_mom_flux = id
             ifld_diff%i_mom_flux = jd
             wk_diff%name(jd) = fhd_SGS_m_flux
             diff_coefs%num_comps(jd) = 9
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
!
         if (iflag_SGS_lorentz .ne. id_SGS_none) then
           icomp_sgs%i_lorentz = i
           ifld_sgs%i_lorentz =  j
           wk_sgs%name(j) = fhd_SGS_maxwell_t
           sgs_coefs%num_comps(j) = 6
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
           if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
             icomp_diff%i_lorentz = id
             ifld_diff%i_lorentz = jd
             wk_diff%name(jd) = fhd_SGS_Lorentz
             diff_coefs%num_comps(jd) = 9
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
!
        if (iflag_SGS_gravity .ne. id_SGS_none) then
          if(iflag_4_gravity .gt. 0) then
            icomp_sgs%i_buoyancy = i
            ifld_sgs%i_buoyancy =  j
            wk_sgs%name(j) = fhd_SGS_buoyancy
            sgs_coefs%num_comps(j) = 6
            i = i + sgs_coefs%num_comps(j)
            j = j + 1
          end if
          if(iflag_4_composit_buo .gt. id_turn_OFF) then
            icomp_sgs%i_comp_buoyancy = i
            ifld_sgs%i_comp_buoyancy =  j
            wk_sgs%name(j) = fhd_SGS_comp_buo
            sgs_coefs%num_comps(j) = 6
            i = i + sgs_coefs%num_comps(j)
            j = j + 1
          end if
        end if
       end if
!
       if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
         if (iflag_SGS_induction .ne. id_SGS_none) then
           icomp_sgs%i_induction = i
           ifld_sgs%i_induction =  j
           wk_sgs%name(j) = fhd_SGS_induction
           sgs_coefs%num_comps(j) = 3
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
         end if
       end if
       if (iflag_t_evo_4_magne .gt. id_no_evolution) then
         if (iflag_SGS_induction .ne. id_SGS_none) then
           icomp_sgs%i_induction = i
           ifld_sgs%i_induction =  j
           wk_sgs%name(j) = fhd_SGS_induction
           sgs_coefs%num_comps(j) = 3
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
           if (iflag_commute_induction .eq. id_SGS_commute_ON) then
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
       if (iflag_t_evo_4_composit .gt. id_no_evolution) then
         if (iflag_SGS_comp_flux .ne. id_SGS_none) then
           icomp_sgs%i_comp_flux = i
           ifld_sgs%i_comp_flux =  j
           wk_sgs%name(j) = fhd_SGS_c_flux
           sgs_coefs%num_comps(j) = 3
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
           if (iflag_commute_c_flux .eq. id_SGS_commute_ON) then
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
       sgs_coefs%istack_comps(0) = 0
       do i = 1, sgs_coefs%num_field
         sgs_coefs%istack_comps(i) = sgs_coefs%istack_comps(i-1)        &
     &                              + sgs_coefs%num_comps(i)
       end do
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        if(iflag_SGS_model .ne. id_SGS_none                             &
     &      .and. iflag_commute_temp .eq. id_SGS_commute_ON) then
            icomp_diff%i_temp = id
            ifld_diff%i_temp = jd
            wk_diff%name(jd) = fhd_temp
            diff_coefs%num_comps(jd) = 3
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        if(iflag_SGS_model .ne. id_SGS_none                             &
     &        .and. iflag_commute_composit .eq. id_SGS_commute_ON) then
            icomp_diff%i_light = id
            ifld_diff%i_light = jd
            wk_diff%name(jd) = fhd_light
            diff_coefs%num_comps(jd) = 3
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if(iflag_SGS_model .ne. id_SGS_none                             &
     &      .and. iflag_commute_velo .eq. id_SGS_commute_ON) then
            icomp_diff%i_velo = id
            ifld_diff%i_velo = jd
            wk_diff%name(jd) = fhd_velo
            diff_coefs%num_comps(jd) = 9
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if(iflag_SGS_model .ne. id_SGS_none                             &
     &      .and. iflag_commute_magne .eq. id_SGS_commute_ON) then
            icomp_diff%i_magne = id
            ifld_diff%i_magne = jd
            wk_diff%name(jd) = fhd_magne
            diff_coefs%num_comps(jd) = 9
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        if(iflag_SGS_model .ne. id_SGS_none                             &
     &      .and. iflag_commute_magne .eq. id_SGS_commute_ON) then
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
       do i = 1, diff_coefs%num_field
         diff_coefs%istack_comps(i) = diff_coefs%istack_comps(i-1)      &
     &                               + diff_coefs%num_comps(i)
       end do
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'num_sgs_kinds', sgs_coefs%num_field
        write(*,*) 'num_sgs_coefs', sgs_coefs%ntot_comp
!
        if(ifld_sgs%i_heat_flux .gt. 0) then
          write(*,*) 'iak_sgs_hf',                                      &
     &              ifld_sgs%i_heat_flux, icomp_sgs%i_heat_flux,        &
     &              sgs_coefs%num_comps(ifld_sgs%i_heat_flux),          &
     &              trim(wk_sgs%name(ifld_sgs%i_heat_flux))
        end if
        if(ifld_sgs%i_mom_flux .gt. 0) then
          write(*,*) 'iak_sgs_mf',                                      &
     &              ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,          &
     &              sgs_coefs%num_comps(ifld_sgs%i_mom_flux),           &
     &              trim(wk_sgs%name(ifld_sgs%i_mom_flux))
        end if
        if(ifld_sgs%i_lorentz .gt. 0) then
          write(*,*) 'iak_sgs_lor',                                     &
     &              ifld_sgs%i_lorentz, icomp_sgs%i_lorentz,            &
     &              sgs_coefs%num_comps(ifld_sgs%i_lorentz),            &
     &              trim(wk_sgs%name(ifld_sgs%i_lorentz))
        end if
        if(ifld_sgs%i_buoyancy .gt. 0) then
          write(*,*) 'iak_sgs_tbuo',                                    &
     &              ifld_sgs%i_buoyancy, icomp_sgs%i_buoyancy,          &
     &              sgs_coefs%num_comps(ifld_sgs%i_buoyancy),           &
     &              trim(wk_sgs%name(ifld_sgs%i_buoyancy))
        end if
        if(ifld_sgs%i_comp_buoyancy .gt. 0) then
          write(*,*) 'iak_sgs_cbuo',                                    &
     &             ifld_sgs%i_comp_buoyancy, icomp_sgs%i_comp_buoyancy, &
     &             sgs_coefs%num_comps(ifld_sgs%i_comp_buoyancy),       &
     &             trim(wk_sgs%name(ifld_sgs%i_comp_buoyancy))
        end if
        if(ifld_sgs%i_induction .gt. 0) then
          write(*,*) 'iak_sgs_uxb',                                     &
     &               ifld_sgs%i_induction, icomp_sgs%i_induction,       &
     &               sgs_coefs%num_comps(ifld_sgs%i_induction),         &
     &               trim(wk_sgs%name(ifld_sgs%i_induction))
        end if
        if(ifld_sgs%i_comp_flux .gt. 0) then
          write(*,*) 'iak_sgs_cf',                                      &
     &               ifld_sgs%i_comp_flux, icomp_sgs%i_comp_flux,       &
     &               sgs_coefs%num_comps(ifld_sgs%i_comp_flux),         &
     &               trim(wk_sgs%name(ifld_sgs%i_comp_flux))
        end if
        diff_coefs%ntot_comp = diff_coefs%num_field
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
      end if
!
      end subroutine s_count_sgs_components
!
!  ------------------------------------------------------------------
!
      subroutine set_SGS_addresses(iphys_elediff)
!
      use m_control_parameter
      use t_material_property
!
      type(SGS_terms_address), intent(inout) :: iphys_elediff
!
      integer(kind = kint) :: i
!
      i = 1
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (  iflag_SGS_heat .ne.      id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none ) then
         iphys_elediff%i_velo = i
         iphys_elediff%i_filter_velo = i + 9
         i = i + 18
        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         iphys_elediff%i_magne = i
         iphys_elediff%i_filter_magne = i + 9
         i = i + 18
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         iphys_elediff%i_magne = i
         iphys_elediff%i_filter_magne = i + 9
         i = i + 18
        end if
!
      else if (iflag_SGS_model .ne. id_SGS_none                         &
     &   .and. iflag_dynamic_SGS .eq. id_SGS_DYNAMIC_OFF) then
        if (   iflag_SGS_heat .ne.     id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none) then
         iphys_elediff%i_velo = i
         i = i + 9
        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         iphys_elediff%i_magne = i
         i = i + 9
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         iphys_elediff%i_magne = i
         i = i + 9
        end if
      end if
!
      end subroutine set_SGS_addresses
!
!  ------------------------------------------------------------------
!
      end module count_sgs_components
      