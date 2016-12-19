!
!     module count_sgs_components
!
!      Written by H. Matsui on 2004
!      Modified by H. Matsui on July, 2007
!
!!      subroutine define_sgs_components(numnod, numele, layer_tbl,     &
!!     &          ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs, sgs_coefs_nod)
!!      subroutine set_SGS_ele_fld_addresses(iphys_elediff)
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(SGS_terms_address), intent(inout) :: ifld_sgs
!!        type(SGS_terms_address), intent(inout) :: icomp_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
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
      subroutine define_sgs_components(numnod, numele, layer_tbl,       &
     &          ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs, sgs_coefs_nod)
!
      use calypso_mpi
      use m_phys_labels
      use m_control_parameter
!
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(SGS_terms_address), intent(inout) :: ifld_sgs, icomp_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
!
!
      call s_count_sgs_components(sgs_coefs)
!
!   set index for model coefficients
!
      call alloc_sgs_coefs_layer(layer_tbl%e_grp%num_grp,               &
     &    sgs_coefs%num_field, sgs_coefs%ntot_comp, wk_sgs)
!
      call alloc_SGS_num_coefs(sgs_coefs)
      call alloc_SGS_coefs(numele, sgs_coefs)
!
      call set_sgs_addresses                                            &
     &   (ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs)
      call check_sgs_addresses                                          &
     &   (ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs)
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                     &
     &      .or. iflag_SGS_model.eq.id_SGS_similarity)  then
        call copy_SGS_num_coefs(sgs_coefs, sgs_coefs_nod)
        call alloc_SGS_coefs(numnod, sgs_coefs_nod)
      end if
!
      end subroutine define_sgs_components
!
!  ------------------------------------------------------------------
!
      subroutine s_count_sgs_components(sgs_coefs)
!
      use calypso_mpi
      use m_phys_labels
      use m_control_parameter
!
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_model_coefs
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!    count coefficients for SGS terms
!
      sgs_coefs%num_field = 0
      sgs_coefs%ntot_comp = 0
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        if (iflag_SGS_heat .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
        end if
      end if
!
      if (evo_velo%iflag_scheme .gt. id_no_evolution) then
        if (iflag_SGS_inertia .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 6
        end if
!
        if (iflag_SGS_lorentz .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 6
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
      if (evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        if (iflag_SGS_induction .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
        end if
      end if
      if (evo_magne%iflag_scheme .gt. id_no_evolution) then
        if (iflag_SGS_induction .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
        end if
      end if
!
      if (evo_comp%iflag_scheme .gt. id_no_evolution) then
        if (iflag_SGS_comp_flux .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
        end if
      end if
!
      end subroutine s_count_sgs_components
!
!  ------------------------------------------------------------------
!
      subroutine set_sgs_addresses                                      &
     &         (ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs)
!
      use calypso_mpi
      use m_phys_labels
      use m_control_parameter
!
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_model_coefs
!
      type(SGS_terms_address), intent(inout) :: ifld_sgs, icomp_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
      integer(kind = kint) :: i, j, id, jd
!
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
         end if
       end if
!
       if (evo_velo%iflag_scheme .gt. id_no_evolution) then
         if (iflag_SGS_inertia .ne. id_SGS_none) then
           icomp_sgs%i_mom_flux = i
           ifld_sgs%i_mom_flux =  j
           wk_sgs%name(j) = fhd_SGS_m_flux
           sgs_coefs%num_comps(j) = 6
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
         end if
!
         if (iflag_SGS_lorentz .ne. id_SGS_none) then
           icomp_sgs%i_lorentz = i
           ifld_sgs%i_lorentz =  j
           wk_sgs%name(j) = fhd_SGS_maxwell_t
           sgs_coefs%num_comps(j) = 6
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
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
       if (evo_vect_p%iflag_scheme .gt. id_no_evolution) then
         if (iflag_SGS_induction .ne. id_SGS_none) then
           icomp_sgs%i_induction = i
           ifld_sgs%i_induction =  j
           wk_sgs%name(j) = fhd_SGS_induction
           sgs_coefs%num_comps(j) = 3
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
         end if
       end if
       if (evo_magne%iflag_scheme .gt. id_no_evolution) then
         if (iflag_SGS_induction .ne. id_SGS_none) then
           icomp_sgs%i_induction = i
           ifld_sgs%i_induction =  j
           wk_sgs%name(j) = fhd_SGS_induction
           sgs_coefs%num_comps(j) = 3
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
         end if
       end if
!
       if (evo_comp%iflag_scheme .gt. id_no_evolution) then
         if (iflag_SGS_comp_flux .ne. id_SGS_none) then
           icomp_sgs%i_comp_flux = i
           ifld_sgs%i_comp_flux =  j
           wk_sgs%name(j) = fhd_SGS_c_flux
           sgs_coefs%num_comps(j) = 3
           i = i + sgs_coefs%num_comps(j)
           j = j + 1
         end if
       end if
!
       sgs_coefs%istack_comps(0) = 0
       do i = 1, sgs_coefs%num_field
         sgs_coefs%istack_comps(i) = sgs_coefs%istack_comps(i-1)        &
     &                              + sgs_coefs%num_comps(i)
       end do
!
      end subroutine set_sgs_addresses
!
!  ------------------------------------------------------------------
!
      subroutine set_SGS_ele_fld_addresses(iphys_elediff)
!
      use m_control_parameter
      use t_material_property
      use t_SGS_model_coefs
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
     &     .and. evo_magne%iflag_scheme .gt. id_no_evolution) then
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
     &     .and. evo_magne%iflag_scheme .gt. id_no_evolution) then
         iphys_elediff%i_magne = i
         i = i + 9
        end if
      end if
!
      end subroutine set_SGS_ele_fld_addresses
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
      subroutine check_sgs_addresses                                    &
     &         (ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs)
!
      use calypso_mpi
!
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_model_coefs
!
!
      type(SGS_terms_address), intent(inout) :: ifld_sgs, icomp_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
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
      end if
!
      end subroutine check_sgs_addresses
!
! -------------------------------------------------------------------
!
      end module count_sgs_components
      