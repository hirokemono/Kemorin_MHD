!>@file   set_control_FEM_SGS.f90
!!@brief  module set_control_FEM_SGS
!!
!!@author H. Matsui
!!@date Programmed in 2003
!!@n    modified in Aug., 2007
!!@n    modified in Nov., 2009
!
!> @brief set parameters for SGS model
!!        from control data
!!
!!@verbatim
!!      subroutine s_set_control_FEM_SGS                                &
!!     &         (ffile_ctl, sgs_ctl, elayer_ctl, SGS_param)
!!        type(filter_file_control), intent(in) :: ffile_ctl
!!        type(SGS_model_control), intent(in) :: sgs_ctl
!!        type(layering_control), intent(in) :: elayer_ctl
!!        type(SGS_model_control_params), intent(inout) :: SGS_param
!!@endverbatim
!
      module set_control_FEM_SGS
!
      use m_precision
      use m_error_IDs
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      private :: set_Csim_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_FEM_SGS                                  &
     &         (ffile_ctl, sgs_ctl, elayer_ctl, SGS_param)
!
      use m_geometry_constants
      use m_file_format_switch
      use m_phys_labels
      use m_filter_file_names
      use t_ctl_data_SGS_model
      use t_ctl_data_filter_files
      use t_ctl_data_ele_layering
      use t_SGS_control_parameter
      use set_control_ele_layering
!
      type(filter_file_control), intent(in) :: ffile_ctl
      type(SGS_model_control), intent(in) :: sgs_ctl
      type(layering_control), intent(in) :: elayer_ctl
      type(SGS_model_control_params), intent(inout) :: SGS_param
!
      character(len=kchara) :: tmpchara
!
!
      SGS_param%ifilter_final = ifilter_2delta
!
!
      if (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (sgs_ctl%SGS_negative_clip_ctl%iflag .eq. 0) then
          e_message                                                     &
     &      = 'Set cliping method for model coefficient'
          call calypso_MPI_abort(ierr_SGS, e_message)
        else
          tmpchara = sgs_ctl%SGS_negative_clip_ctl%charavalue
!
          if (cmp_no_case(tmpchara, 'none'))                            &
     &      SGS_param%iflag_nagetive_clip = id_SGS_NO_CLIP
          if (cmp_no_case(tmpchara, 'zero'))                            &
     &      SGS_param%iflag_nagetive_clip = id_SGS_ZERO_CLIP
          if (cmp_no_case(tmpchara, 'keep'))                            &
     &      SGS_param%iflag_nagetive_clip = id_SGS_KEEP_PREVIOUS
        end if
!
!
        SGS_param%iflag_Csim_marging = id_SGS_DIR_LSQ
        if (sgs_ctl%SGS_marging_ctl%iflag .gt. 0) then
          tmpchara = sgs_ctl%SGS_marging_ctl%charavalue
!
          if      (cmp_no_case(tmpchara, 'lsq_over_directions')         &
     &        .or. cmp_no_case(tmpchara, 'lsq')) then
             SGS_param%iflag_Csim_marging = id_SGS_DIR_LSQ
          else if (cmp_no_case(tmpchara, 'average_over_directions')     &
     &        .or. cmp_no_case(tmpchara, 'average')) then
             SGS_param%iflag_Csim_marging = id_SGS_DIR_AVERAGE
          else if (cmp_no_case(tmpchara, 'weighting_by_correlation')    &
     &        .or. cmp_no_case(tmpchara, 'weighting')) then
             SGS_param%iflag_Csim_marging = id_SGS_DIR_CORRELATE
          end if
        end if
!
        SGS_param%min_step_dynamic = 1
        if (sgs_ctl%min_step_dynamic_ctl%iflag .gt. 0) then
          SGS_param%min_step_dynamic                                    &
     &      = sgs_ctl%min_step_dynamic_ctl%intvalue
        end if
!
        SGS_param%max_step_dynamic = 50
        if (sgs_ctl%max_step_dynamic_ctl%iflag .gt. 0) then
          SGS_param%max_step_dynamic                                    &
     &      = sgs_ctl%max_step_dynamic_ctl%intvalue
        end if
!
        SGS_param%extend_SGS_dt                                         &
     &      = sgs_ctl%delta_to_shrink_dynamic_ctl%realvalue
        SGS_param%extend_SGS_dt                                         &
     &      = sgs_ctl%delta_to_extend_dynamic_ctl%realvalue
      end if
!
!
      SGS_param%iflag_parterbuation = id_turn_OFF
      if(sgs_ctl%SGS_perturbation_ctl%iflag .gt. 0) then
          tmpchara = sgs_ctl%SGS_perturbation_ctl%charavalue
          if (cmp_no_case(tmpchara, 'reference')                        &
     &       ) SGS_param%iflag_parterbuation = id_SGS_REFERENCE
          if (cmp_no_case(tmpchara, 'average')                          &
     &       ) SGS_param%iflag_parterbuation = id_SGS_REF_AVERAGE
      end if
!
!
      SGS_param%itype_Csym                                              &
     &      = set_Csim_type(id_CSIM_FIELD,                              &
     &                      sgs_ctl%SGS_model_coef_type_ctl)
!
      SGS_param%itype_Csym_h_flux                                       &
     &      = set_Csim_type(SGS_param%itype_Csym,                       &
     &                      sgs_ctl%heat_flux_csim_type_ctl)
      SGS_param%itype_Csym_c_flux                                       &
     &      = set_Csim_type(SGS_param%itype_Csym,                       &
     &                      sgs_ctl%comp_flux_csim_type_ctl)
      SGS_param%itype_Csym_m_flux                                       &
     &      = set_Csim_type(SGS_param%itype_Csym,                       &
     &                      sgs_ctl%mom_flux_csim_type_ctl)
      SGS_param%itype_Csym_maxwell                                      &
     &      = set_Csim_type(SGS_param%itype_Csym,                       &
     &                      sgs_ctl%maxwell_csim_type_ctl)
      SGS_param%itype_Csym_uxb                                          &
     &      = set_Csim_type(SGS_param%itype_Csym,                       &
     &                      sgs_ctl%uxb_csim_type_ctl)
!
      SGS_param%icoord_Csim = 0
      if(sgs_ctl%SGS_model_coef_coord_ctl%iflag .gt. 0) then
          tmpchara = sgs_ctl%SGS_model_coef_coord_ctl%charavalue
          if(   cmp_no_case(tmpchara, 'spherical')                      &
     &     .or. cmp_no_case(tmpchara, 'sph')) then
             SGS_param%icoord_Csim = iflag_spherical
          end if
          if(   cmp_no_case(tmpchara, 'cylindrical')                    &
     &     .or. cmp_no_case(tmpchara, 'spz')) then
            SGS_param%icoord_Csim = iflag_cylindrical
          end if
      end if
!
      if (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        call s_set_control_ele_layering(elayer_ctl)
      end if
!
      if (SGS_param%iflag_SGS .eq. id_SGS_NL_grad) then
        if (ffile_ctl%filter_elen_head_ctl%iflag .eq. 1) then
          filter_elen_head = ffile_ctl%filter_elen_head_ctl%charavalue
        end if
!
        ifmt_filter_elen                                                &
     &     = choose_file_format(ffile_ctl%filter_elen_format)
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'filter_elen_head: ', trim(filter_elen_head)
          write(*,*) 'filter_elen_format: ', ifmt_filter_elen
        end if
      end if
!
      if (SGS_param%iflag_SGS .ne. id_SGS_none) then
        if (iflag_debug .gt. 0)  then
          write(*,*) 'itype_SGS_model_coef: ',  SGS_param%itype_Csym
          write(*,*) 'icoord_SGS_model_coef: ', SGS_param%icoord_Csim
!
          write(*,*) 'SGS_hf_factor:     ', SGS_param%SGS_hf_factor
          write(*,*) 'SGS_mf_factor:     ', SGS_param%SGS_mf_factor
          write(*,*) 'SGS_mawell_factor: ',                             &
     &               SGS_param%SGS_mawell_factor
          write(*,*) 'SGS_uxb_factor:    ', SGS_param%SGS_uxb_factor
          write(*,*) 'SGS_cf_factor:     ', SGS_param%SGS_cf_factor
!
          write(*,*) 'itype_SGS_h_flux_coef:  ',                        &
     &              SGS_param%itype_Csym_h_flux
          write(*,*) 'itype_SGS_c_flux_coef:  ',                        &
     &              SGS_param%itype_Csym_c_flux
          write(*,*) 'itype_SGS_m_flux_coef:  ',                        &
     &              SGS_param%itype_Csym_m_flux
          write(*,*) 'itype_SGS_maxwell_coef: ',                        &
     &              SGS_param%itype_Csym_maxwell
          write(*,*) 'itype_SGS_uxb_coef:     ',                        &
     &              SGS_param%itype_Csym_uxb
        end if
      end if
!
      end subroutine s_set_control_FEM_SGS
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function set_Csim_type(i_default,            &
     &                                            csim_type_ctl)
!
      use t_control_elements
      use t_SGS_control_parameter
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: i_default
      type(read_character_item), intent(in) :: csim_type_ctl
!
      set_Csim_type = i_default
      if(csim_type_ctl%iflag .eq. 0) return 
      if(cmp_no_case(csim_type_ctl%charavalue, 'components'))           &
     &        set_Csim_type = id_CSIM_COMPONENT
!
      end function set_Csim_type
!
! -----------------------------------------------------------------------
!
      end module set_control_FEM_SGS
