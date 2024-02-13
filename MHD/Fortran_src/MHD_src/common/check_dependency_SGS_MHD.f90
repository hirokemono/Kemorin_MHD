!>@file   check_dependency_SGS_MHD.f90
!!@brief  module check_dependency_SGS_MHD
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Check dependecy of field list fro MHD dynamo
!!
!!@verbatim
!!      subroutine set_sph_SGS_MHD_spectr_data                          &
!!     &         (SGS_par, MHD_prop, sph, rj_fld, ipol, ipol_LES)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(inout) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(SGS_model_addresses), intent(inout) :: ipol_LES
!!      subroutine check_filter_force_dependency                        &
!!     &         (fl_prop, iphys_fil, fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(phys_data), intent(in) :: fld
!!@endverbatim
!
      module check_dependency_SGS_MHD
!
      use m_precision
      use m_error_IDs
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_phys_address
      use t_phys_data
      use t_SGS_model_addresses
      use t_base_force_labels
!
      use check_dependency_for_MHD
      use t_SGS_term_labels
!
      implicit none
!
      private :: check_dependence_4_SPH_SGS
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_SGS_MHD_spectr_data                            &
     &         (SGS_par, MHD_prop, sph, rj_fld, ipol, ipol_LES)
!
      use t_spheric_parameter
      use set_field_data_w_SGS
      use check_dependency_for_MHD
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_grids), intent(in) :: sph
!
      type(phys_data), intent(inout) :: rj_fld
      type(phys_address), intent(inout) :: ipol
      type(SGS_model_addresses), intent(inout) :: ipol_LES
!
!
      call init_field_data_w_SGS                                        &
     &   (sph%sph_rj%nnod_rj, rj_fld, ipol, ipol_LES)
!
      call check_field_dependencies                                     &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, ipol%base, rj_fld)
      call check_filter_force_dependency                                &
     &   (MHD_prop%fl_prop, ipol_LES%filter_fld, rj_fld)
      call check_dependence_SPH_evo(MHD_prop%fl_prop, ipol, rj_fld)
!
      call check_dependence_4_SPH_SGS(SGS_par%model_p,                  &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    ipol_LES%filter_fld, ipol_LES%SGS_term, rj_fld)
!
      end subroutine set_sph_SGS_MHD_spectr_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_filter_force_dependency                          &
     &         (fl_prop, iphys_fil, fld)
!
      type(fluid_property), intent(in) :: fl_prop
      type(base_field_address), intent(in) :: iphys_fil
      type(phys_data), intent(in) :: fld
!
      character(len=kchara) :: msg
!
!
!   check dependencies for time evolution
      if ( fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (fl_prop%iflag_4_filter_gravity) then
          msg = 'Filtered thermal buoyancy needs'
          call check_missing_field_w_msg(fld, msg, iphys_fil%i_temp)
        end if
!
        if (fl_prop%iflag_4_filter_comp_buo) then
          msg = 'Filtered compositional buoyancy needs'
          call check_missing_field_w_msg(fld, msg, iphys_fil%i_light)
        end if
      end if
!
      end subroutine check_filter_force_dependency
!
! -----------------------------------------------------------------------
!
      subroutine check_dependence_4_SPH_SGS(SGS_param,                  &
     &          fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          ipol_fil, ippol_SGS, rj_fld)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(base_field_address), intent(in) :: ipol_fil
      type(SGS_term_address), intent(in) :: ippol_SGS
      type(phys_data), intent(in) :: rj_fld
!
      character(len=kchara) :: msg
!
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none) then
          msg = 'solving SGS momentum flux needs'
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ippol_SGS%i_SGS_inertia)
        end if
!
        if(SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
          msg = 'solving SGS lorentz term needs'
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ippol_SGS%i_SGS_Lorentz)
        end if
      end if
!
!
      if ( ht_prop%iflag_scheme .gt. id_no_evolution) then
        if ( SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
          msg = 'solving SGS heat flux needs'
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ippol_SGS%i_SGS_h_flux)
        end if
      end if
!
!
      if ( cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          msg = 'solving SGS magnetic induction needs'
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ippol_SGS%i_SGS_induction)
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ippol_SGS%i_SGS_vp_induct)
        end if
      end if
!
!
      if ( cp_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
          msg = 'solving SGS compsition flux needs'
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ippol_SGS%i_SGS_c_flux)
        end if
      end if
!
!
      if ( fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%SGS_momentum%iflag_SGS_flux .eq. id_SGS_similarity &
     &   .and. SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          msg = 'SGS momentum flux needs'
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ipol_fil%i_velo)
        end if
!
        if    (SGS_param%iflag_SGS_lorentz .eq. id_SGS_similarity       &
     &   .and. SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          msg = 'SGS Lorentz term needs'
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ipol_fil%i_magne)
        end if
      end if
!
      if(SGS_param%iflag_SGS_gravity .gt. id_SGS_none) then
        if(fl_prop%iflag_4_gravity                                      &
     &     .and. (fl_prop%iflag_4_composit_buo .eqv. .FALSE.)) then
          call calypso_MPI_abort(ierr_fld,                              &
     &       'set one of buoyancy sources')
        end if
        if(fl_prop%iflag_4_gravity) then
          if(SGS_param%SGS_momentum%iflag_SGS_flux.eq.id_SGS_none       &
     &      .or. SGS_param%SGS_heat%iflag_SGS_flux.eq.id_SGS_none) then
            call calypso_MPI_abort(ierr_fld,                            &
     &          'Turn on SGS momentum flux and heat flux')
          end if
        end if
        if(fl_prop%iflag_4_composit_buo) then
          if(SGS_param%SGS_momentum%iflag_SGS_flux.eq.id_SGS_none       &
     &     .or. SGS_param%SGS_light%iflag_SGS_flux.eq.id_SGS_none) then
              call calypso_MPI_abort(ierr_fld,                          &
     &          'Turn on SGS momentum flux and composition flux')
          end if
        end if
      end if
!
      if ( ht_prop%iflag_scheme .gt. id_no_evolution) then
        if    (SGS_param%SGS_heat%iflag_SGS_flux .eq. id_SGS_similarity &
     &   .and. SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          msg = 'SGS heat flux needs'
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ipol_fil%i_temp)
        end if
      end if
!
!
      if ( cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if    (SGS_param%iflag_SGS_uxb .eq. id_SGS_similarity           &
     &   .and. SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          msg = 'SGS induction needs'
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ipol_fil%i_velo)
          call check_missing_field_w_msg                                &
     &       (rj_fld, msg, ipol_fil%i_magne)
        end if
      end if
!
      end subroutine check_dependence_4_SPH_SGS
!
! -----------------------------------------------------------------------
!
      end module check_dependency_SGS_MHD
