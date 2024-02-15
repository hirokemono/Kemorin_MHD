!>@file   init_sgs_diff_coefs.f90
!!        module init_sgs_diff_coefs
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief initialize model coefficients for commutation
!!
!!@verbatim
!!      subroutine define_sgs_diff_coefs(numele, SGS_param, cmt_param,  &
!!     &          layer_tbl, MHD_prop, wk_diff, Csims_FEM_MHD)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!@end verbatim
!
      module init_sgs_diff_coefs
!
      use m_precision
      use m_machine_parameter
      use t_SGS_control_parameter
      use t_control_parameter
      use t_physical_property
      use t_base_field_labels
      use t_SGS_term_labels
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
      subroutine define_sgs_diff_coefs(numele, SGS_param, cmt_param,    &
     &          layer_tbl, MHD_prop, wk_diff, Csims_FEM_MHD)
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_FEM_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: numele
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!
      integer(kind = kint) :: ntot_diff_comp
!
!
      call count_sgs_diff_coefs(SGS_param, cmt_param,                   &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    ntot_diff_comp, Csims_FEM_MHD%diff_coefs)
      call alloc_sgs_coefs_layer(layer_tbl%e_grp%num_grp,               &
     &    Csims_FEM_MHD%diff_coefs%num_field, ntot_diff_comp, wk_diff)
!
      call alloc_SGS_num_coefs(Csims_FEM_MHD%diff_coefs)
      call alloc_SGS_coefs(numele, Csims_FEM_MHD%diff_coefs)
!
      call set_sgs_diff_addresses(SGS_param, cmt_param,                 &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    Csims_FEM_MHD%iak_diff_sgs,                                   &
     &    Csims_FEM_MHD%icomp_diff_base, Csims_FEM_MHD%icomp_diff_sgs,  &
     &    wk_diff, Csims_FEM_MHD%diff_coefs)
      Csims_FEM_MHD%diff_coefs%ntot_comp                                &
     &      = Csims_FEM_MHD%diff_coefs%num_field
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_velo%iak_diff .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_velo)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_velo)
      end if
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_magne%iak_diff .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_magne)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_magne)
      end if
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_temp%iak_diff .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_temp)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_temp)
      end if
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_light%iak_diff .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_light)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_light)
      end if
!
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_SGS_uxb%iak_diff .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_uxb)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_uxb)
      end if
!
!
      if(iflag_debug .gt. 0) then
        call check_sgs_diff_addresses(Csims_FEM_MHD%iak_diff_sgs,       &
     &     Csims_FEM_MHD%icomp_diff_base, Csims_FEM_MHD%icomp_diff_sgs, &
     &     wk_diff, Csims_FEM_MHD%diff_coefs)
      end if
!
!
      end subroutine define_sgs_diff_coefs
!
!  ------------------------------------------------------------------
!
      subroutine count_sgs_diff_coefs(SGS_param, cmt_param,             &
     &          fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          ntot_diff_comp, diff_coefs)
!
      use calypso_mpi
!
      use t_base_field_labels
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_model_coefs
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
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
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
          if (SGS_param%SGS_heat%iflag_commute_flux                     &
     &       .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 3
          end if
        end if
      end if
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%SGS_momentum%iflag_SGS_flux                        &
     &        .ne. id_SGS_none) then
          if(SGS_param%SGS_momentum%iflag_commute_flux                  &
     &      .eq. id_SGS_commute_ON) then
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
      if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          if(cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 9
          end if
        end if
      end if
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
          if (SGS_param%SGS_light%iflag_commute_flux                    &
     &      .eq. id_SGS_commute_ON) then
            diff_coefs%num_field = diff_coefs%num_field + 1
            ntot_diff_comp = ntot_diff_comp + 3
          end if
        end if
      end if
!
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &      .and. SGS_param%SGS_heat%iflag_commute_field                &
     &           .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 3
        end if
      end if
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &      .and. SGS_param%SGS_light%iflag_commute_field               &
     &           .eq. id_SGS_commute_ON) then
          diff_coefs%num_field = diff_coefs%num_field + 1
          ntot_diff_comp = ntot_diff_comp + 3
        end if
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &       .and. SGS_param%SGS_momentum%iflag_commute_field           &
     &            .eq. id_SGS_commute_ON) then
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
      if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
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
      subroutine set_sgs_diff_addresses(SGS_param, cmt_param,           &
     &          fl_prop, cd_prop, ht_prop, cp_prop, iak_diff_sgs,       &
     &          icomp_diff_base, icomp_diff_sgs, wk_diff, diff_coefs)
!
      use calypso_mpi
      use t_base_field_labels
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_term_labels
      use t_SGS_model_coefs
!
      use m_base_field_labels
      use m_SGS_term_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
!
      type(SGS_term_address), intent(inout) :: iak_diff_sgs
      type(base_field_address), intent(inout) :: icomp_diff_base
      type(SGS_term_address), intent(inout) :: icomp_diff_sgs
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer(kind = kint) :: id, jd
!
!
       id = 1
       jd = 1
       if (ht_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
           if (SGS_param%SGS_heat%iflag_commute_flux                    &
     &       .eq. id_SGS_commute_ON) then
             icomp_diff_sgs%i_SGS_h_flux = id
             iak_diff_sgs%i_SGS_h_flux =  jd
             wk_diff%name(jd) = SGS_heat_flux%name
             diff_coefs%num_comps(jd) = 1
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
       end if
!
       if(fl_prop%iflag_scheme .gt. id_no_evolution) then
         if(SGS_param%SGS_momentum%iflag_SGS_flux                       &
     &     .ne. id_SGS_none) then
           if(SGS_param%SGS_momentum%iflag_commute_flux                 &
     &      .eq. id_SGS_commute_ON) then
             icomp_diff_sgs%i_SGS_m_flux = id
             iak_diff_sgs%i_SGS_m_flux = jd
             wk_diff%name(jd) = SGS_momentum_flux%name
             diff_coefs%num_comps(jd) = 1
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
!
         if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
           if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
             icomp_diff_sgs%i_SGS_Lorentz = id
             iak_diff_sgs%i_SGS_Lorentz = jd
             wk_diff%name(jd) = SGS_Lorentz%name
             diff_coefs%num_comps(jd) = 1
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
       end if
!
       if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
           if (cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
             icomp_diff_sgs%i_SGS_induction = id
             wk_diff%name(jd) = SGS_induction%name
             diff_coefs%Cdiff_SGS_uxb%iak_diff = jd
             diff_coefs%num_comps(jd) = 1
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
       end if
!
       if (cp_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
           if(SGS_param%SGS_light%iflag_commute_flux                    &
     &        .eq. id_SGS_commute_ON) then
             icomp_diff_sgs%i_SGS_c_flux = id
             iak_diff_sgs%i_SGS_c_flux =  jd
             wk_diff%name(jd) = SGS_composit_flux%name
             diff_coefs%num_comps(jd) = 1
             id = id + diff_coefs%num_comps(jd)
             jd = jd + 1
           end if
         end if
       end if
!
!
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. SGS_param%SGS_heat%iflag_commute_field               &
     &           .eq. id_SGS_commute_ON) then
            icomp_diff_base%i_temp = id
            wk_diff%name(jd) = temperature%name
            diff_coefs%Cdiff_temp%iak_diff = jd
            diff_coefs%num_comps(jd) = 1
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. SGS_param%SGS_light%iflag_commute_field              &
     &           .eq. id_SGS_commute_ON) then
            icomp_diff_base%i_light = id
            wk_diff%name(jd) = composition%name
            diff_coefs%Cdiff_light%iak_diff = jd
            diff_coefs%num_comps(jd) = 1
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. SGS_param%SGS_momentum%iflag_commute_field           &
     &           .eq. id_SGS_commute_ON) then
            icomp_diff_base%i_velo = id
            wk_diff%name(jd) = velocity%name
            diff_coefs%Cdiff_velo%iak_diff = jd
            diff_coefs%num_comps(jd) = 1
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      end if
!
      diff_coefs%Cdiff_magne%iak_diff = 0
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
            icomp_diff_base%i_magne = id
            wk_diff%name(jd) = magnetic_field%name
            diff_coefs%Cdiff_magne%iak_diff = jd
            diff_coefs%num_comps(jd) = 1
            id = id + diff_coefs%num_comps(jd)
            jd = jd + 1
        end if
      else if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
            icomp_diff_base%i_magne = id
            diff_coefs%Cdiff_magne%iak_diff = jd
            wk_diff%name(jd) = magnetic_field%name
            diff_coefs%num_comps(jd) = 1
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
      subroutine check_sgs_diff_addresses(iak_diff_sgs,                 &
     &          icomp_diff_base, icomp_diff_sgs, wk_diff, diff_coefs)
!
      use calypso_mpi
!
      use t_base_field_labels
      use t_SGS_term_labels
      use t_ele_info_4_dynamic
      use t_SGS_model_coefs
!
!
      type(SGS_term_address), intent(in) :: iak_diff_sgs
      type(base_field_address), intent(in) :: icomp_diff_base
      type(SGS_term_address), intent(in) :: icomp_diff_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
        write(*,*) 'wk_diff%ntot_comp', wk_diff%ntot_comp
        write(*,*) 'diff_coefs%num_field', diff_coefs%num_field
!
        if(iak_diff_sgs%i_SGS_h_flux .gt. 0) then
          write(*,*) 'iak_diff_hf',                                     &
     &        iak_diff_sgs%i_SGS_h_flux, icomp_diff_sgs%i_SGS_h_flux,   &
     &        diff_coefs%num_comps(iak_diff_sgs%i_SGS_h_flux),          &
     &        trim(wk_diff%name(iak_diff_sgs%i_SGS_h_flux))
        end if
        if(iak_diff_sgs%i_SGS_m_flux .gt. 0) then
          write(*,*) 'iak_diff_mf',                                     &
     &        iak_diff_sgs%i_SGS_m_flux, icomp_diff_sgs%i_SGS_m_flux,   &
     &        diff_coefs%num_comps(iak_diff_sgs%i_SGS_m_flux),          &
     &        trim(wk_diff%name(iak_diff_sgs%i_SGS_m_flux))
        end if
        if(iak_diff_sgs%i_SGS_Lorentz .gt. 0) then
          write(*,*) 'iak_diff_lor',                                    &
     &        iak_diff_sgs%i_SGS_Lorentz, icomp_diff_sgs%i_SGS_Lorentz, &
     &        diff_coefs%num_comps(iak_diff_sgs%i_SGS_Lorentz),         &
     &        trim(wk_diff%name(iak_diff_sgs%i_SGS_Lorentz))
        end if
        if(diff_coefs%Cdiff_SGS_uxb%iak_diff .gt. 0) then
          write(*,*) 'iak_diff_uxb',                                    &
     &        diff_coefs%Cdiff_SGS_uxb%iak_diff,                        &
     &        icomp_diff_sgs%i_SGS_induction,                           &
     &        diff_coefs%Cdiff_SGS_uxb%num_comp,                        &
     &        trim(wk_diff%name(diff_coefs%Cdiff_SGS_uxb%iak_diff))
        end if
!
        if(diff_coefs%Cdiff_temp%iak_diff .gt. 0) then
          write(*,*) 'iak_diff_t',                                      &
     &        diff_coefs%Cdiff_temp%iak_diff, icomp_diff_base%i_temp,   &
     &        diff_coefs%Cdiff_temp%num_comp,                           &
     &        trim(wk_diff%name(diff_coefs%Cdiff_temp%iak_diff))
        end if
        if(diff_coefs%Cdiff_velo%iak_diff .gt. 0) then
          write(*,*) 'iak_diff_v',                                      &
     &        diff_coefs%Cdiff_velo%iak_diff, icomp_diff_base%i_velo,   &
     &        diff_coefs%Cdiff_velo%num_comp,                           &
     &        trim(wk_diff%name(diff_coefs%Cdiff_velo%iak_diff))
        end if
        if(diff_coefs%Cdiff_magne%iak_diff .gt. 0) then
          write(*,*) 'iak_diff_b',                                      &
     &        diff_coefs%Cdiff_magne%iak_diff,                          &
     &        icomp_diff_base%i_magne,                                  &
     &        diff_coefs%Cdiff_magne%num_comp,                          &
     &        trim(wk_diff%name(diff_coefs%Cdiff_magne%iak_diff))
        end if
        if(diff_coefs%Cdiff_light%iak_diff .gt. 0) then
          write(*,*) 'iak_diff_c',                                      &
     &        diff_coefs%Cdiff_light%iak_diff, icomp_diff_base%i_light, &
     &        diff_coefs%Cdiff_light%num_comp,                          &
     &        trim(wk_diff%name(diff_coefs%Cdiff_light%iak_diff))
        end if
!
      end subroutine check_sgs_diff_addresses
!
! -------------------------------------------------------------------
!
      end module init_sgs_diff_coefs
