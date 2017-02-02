!>@file   set_control_4_SGS.f90
!!@brief  module set_control_4_SGS
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
!!      subroutine set_control_SGS_model(sgs_ctl)
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!      subroutine set_control_SPH_SGS                                  &
!!     &         (num_sph_filter_ctl, sph_filter_ctl, sph_filters)
!!        type(sph_filter_ctl_type), intent(in) :: sph_filter_ctl(1)
!!        type(sph_filters_type), intent(inout) :: sph_filters(1)
!!      subroutine set_control_FEM_SGS(ffile_ctl, sgs_ctl, elayer_ctl)
!!        type(filter_file_control), intent(in) :: ffile_ctl
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!        type(layering_control), intent(inout) :: elayer_ctl
!!@endverbatim
!
      module set_control_4_SGS
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
      subroutine set_control_SPH_SGS                                    &
     &         (num_sph_filter_ctl, sph_filter_ctl, sph_filters)
!
      use m_control_parameter
      use t_ctl_data_SGS_filter
      use t_sph_filtering_data
!
      integer(kind = kint), intent(in) :: num_sph_filter_ctl
      type(sph_filter_ctl_type), intent(in) :: sph_filter_ctl(1)
      type(sph_filters_type), intent(inout) :: sph_filters(1)
!
!
      if(SGS_param1%iflag_SGS .eq. id_SGS_none) return
      if(num_sph_filter_ctl .le. 0) then
        call calypso_mpi_abort(1, 'Set filter configrations')
      end if
!
      if(sph_filter_ctl(1)%maximum_moments_ctl%iflag .gt. 0) then
        sph_filters(1)%r_moments%num_momentum                           &
     &     = sph_filter_ctl(1)%maximum_moments_ctl%intvalue
        sph_filters(1)%sph_moments%num_momentum                         &
     &     = sph_filter_ctl(1)%maximum_moments_ctl%intvalue
      end if
!
      if(sph_filter_ctl(1)%radial_filter_width_ctl%iflag .gt. 0) then
        sph_filters(1)%width                                            &
     &     = sph_filter_ctl(1)%radial_filter_width_ctl%realvalue
      end if
!
      if(sph_filter_ctl(1)%sphere_filter_width_ctl%iflag .gt. 0) then
        sph_filters(1)%sph_filter%f_width                               &
     &     = sph_filter_ctl(1)%sphere_filter_width_ctl%realvalue
      end if
!
!      if(sph_filter_ctl(2)%maximum_moments_ctl%iflag .gt. 0) then
!        sph_filters(2)%r_moments%num_momentum                          &
!     &     = sph_filter_ctl(2)%maximum_moments_ctl%intvalue
!        sph_filters(2)%sph_moments%num_momentum                        &
!     &     = sph_filter_ctl(2)%maximum_moments_ctl%intvalue
!
!        sph_filters(3)%r_moments%num_momentum                          &
!     &     = sph_filter_ctl(2)%maximum_moments_ctl%intvalue
!        sph_filters(3)%sph_moments%num_momentum                        &
!     &     = sph_filter_ctl(2)%maximum_moments_ctl%intvalue
!      end if
!
!      if(sph_filter_ctl(2)%radial_filter_width_ctl%iflag .gt. 0) then
!        sph_filters(2)%width                                           &
!     &     = sph_filter_ctl(2)%radial_filter_width_ctl%realvalue
!        sph_filters(3)%width                                           &
!     &     = two*sph_filter_ctl(2)%radial_filter_width_ctl%realvalue
!      end if
!
!      if(sph_filter_ctl(2)%sphere_filter_width_ctl%iflag .gt. 0) then
!        sph_filters(2)%sph_filter%f_width                              &
!     &     = sph_filter_ctl(2)%sphere_filter_width_ctl%realvalue
!        sph_filters(3)%sph_filter%f_width                              &
!     &     = itwo * sph_filter_ctl(2)%sphere_filter_width_ctl%intvalue
!      end if
!
      end subroutine set_control_SPH_SGS
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SGS_model(sgs_ctl)
!
      use m_geometry_constants
      use m_phys_labels
      use m_control_parameter
      use m_SGS_control_parameter
      use t_ctl_data_SGS_model
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
!   set control parameters for SGS model
!
       SGS_param1%iflag_SGS =      id_SGS_none
       SGS_param1%iflag_dynamic =  id_SGS_DYNAMIC_OFF
       filter_param1%iflag_SGS_filter = id_SGS_NO_FILTERING
!
      if (sgs_ctl%SGS_model_name_ctl%iflag .gt. 0) then
        tmpchara = sgs_ctl%SGS_model_name_ctl%charavalue
!
        if      (no_flag(tmpchara)) then
          SGS_param1%iflag_SGS =     id_SGS_none
          SGS_param1%iflag_dynamic = id_SGS_DYNAMIC_OFF
!
        else if (cmp_no_case(tmpchara, 'gradient')) then
          SGS_param1%iflag_SGS =     id_SGS_NL_grad
          SGS_param1%iflag_dynamic = id_SGS_DYNAMIC_OFF
!
        else if (cmp_no_case(tmpchara, 'similarity')) then
          SGS_param1%iflag_SGS =     id_SGS_similarity
          SGS_param1%iflag_dynamic = id_SGS_DYNAMIC_OFF
!
        else if (cmp_no_case(tmpchara, 'dynamic')) then
          SGS_param1%iflag_SGS =     id_SGS_NL_grad
          SGS_param1%iflag_dynamic = id_SGS_DYNAMIC_ON
!
        else if (cmp_no_case(tmpchara, 'dynamic_similarity')            &
     &      .or. cmp_no_case(tmpchara, 'dynamic_simi')) then
          SGS_param1%iflag_SGS =     id_SGS_similarity
          SGS_param1%iflag_dynamic = id_SGS_DYNAMIC_ON
!
        else if (cmp_no_case(tmpchara,'diffusion')) then
          SGS_param1%iflag_SGS =     id_SGS_diffusion
          SGS_param1%iflag_dynamic = id_SGS_DYNAMIC_ON
        end if
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'iflag_SGS_model',   SGS_param1%iflag_SGS
        write(*,*) 'iflag_dynamic_SGS', SGS_param1%iflag_dynamic
      end if
!
!
!  set applied terms
!
      SGS_param1%iflag_SGS_h_flux =  id_SGS_none
      SGS_param1%iflag_SGS_c_flux =  id_SGS_none
      SGS_param1%iflag_SGS_m_flux =  id_SGS_none
      SGS_param1%iflag_SGS_lorentz = id_SGS_none
      SGS_param1%iflag_SGS_uxb =     id_SGS_none
      SGS_param1%iflag_SGS_gravity = id_SGS_none
!
      if (SGS_param1%iflag_SGS .ne. id_SGS_none) then
        if (sgs_ctl%SGS_terms_ctl%icou .eq. izero) then
            e_message = 'Set equations to apply SGS model'
            call calypso_MPI_abort(ierr_SGS, e_message)
        else
!
          do i = 1, sgs_ctl%SGS_terms_ctl%num
            tmpchara = sgs_ctl%SGS_terms_ctl%c_tbl(i)
            if(     tmpchara .eq. thd_heat_flux) then
              SGS_param1%iflag_SGS_h_flux =  SGS_param1%iflag_SGS
            else if(tmpchara .eq. thd_advection) then
              SGS_param1%iflag_SGS_m_flux =  SGS_param1%iflag_SGS
            else if(tmpchara .eq. thd_lorentz) then
              SGS_param1%iflag_SGS_lorentz = SGS_param1%iflag_SGS
            else if(tmpchara .eq. thd_induction) then
              SGS_param1%iflag_SGS_uxb =     SGS_param1%iflag_SGS
            else if(tmpchara .eq. thd_gravity) then
              SGS_param1%iflag_SGS_gravity = SGS_param1%iflag_SGS
            else if(tmpchara .eq. thd_comp_flux) then
              SGS_param1%iflag_SGS_c_flux =  SGS_param1%iflag_SGS
            end if
          end do
!
          call dealloc_control_array_chara(sgs_ctl%SGS_terms_ctl)
        end if
      end if
!
      if (SGS_param1%iflag_SGS .ne. id_SGS_none) then
        if (iflag_debug .gt. 0)  then
          write(*,*) 'iflag_SGS_heat:         ',                        &
     &              SGS_param1%iflag_SGS_h_flux
          write(*,*) 'iflag_SGS_comp_flux:    ',                        &
     &              SGS_param1%iflag_SGS_c_flux
          write(*,*) 'iflag_SGS_inertia:      ',                        &
     &              SGS_param1%iflag_SGS_m_flux
          write(*,*) 'iflag_SGS_lorentz:      ',                        &
     &              SGS_param1%iflag_SGS_lorentz
          write(*,*) 'iflag_SGS_induction:    ',                        &
     &              SGS_param1%iflag_SGS_uxb
          write(*,*) 'iflag_SGS_gravity:      ',                        &
     &              SGS_param1%iflag_SGS_gravity
        end if
      end if
!
      call set_control_SGS_commute(sgs_ctl, cmt_param1)
!
      end subroutine set_control_SGS_model
!
! -----------------------------------------------------------------------
!
      subroutine set_control_FEM_SGS(ffile_ctl, sgs_ctl, elayer_ctl)
!
      use m_geometry_constants
      use m_file_format_switch
      use m_phys_labels
      use m_control_parameter
      use m_filter_file_names
      use t_ctl_data_SGS_model
      use t_ctl_data_filter_files
      use t_ctl_data_ele_layering
      use sgs_ini_model_coefs_IO
      use set_control_ele_layering
!
      type(filter_file_control), intent(in) :: ffile_ctl
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(layering_control), intent(inout) :: elayer_ctl
!
      character(len=kchara) :: tmpchara
!
!
      SGS_param1%ifilter_final = ifilter_2delta
!
!
      if (SGS_param1%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (sgs_ctl%SGS_negative_clip_ctl%iflag .eq. 0) then
          e_message                                                     &
     &      = 'Set cliping method for model coefficient'
          call calypso_MPI_abort(ierr_SGS, e_message)
        else
          tmpchara = sgs_ctl%SGS_negative_clip_ctl%charavalue
!
          if (cmp_no_case(tmpchara, 'none'))                            &
     &      SGS_param1%iflag_nagetive_clip = id_SGS_NO_CLIP
          if (cmp_no_case(tmpchara, 'zero'))                            &
     &      SGS_param1%iflag_nagetive_clip = id_SGS_ZERO_CLIP
          if (cmp_no_case(tmpchara, 'keep'))                            &
     &      SGS_param1%iflag_nagetive_clip = id_SGS_KEEP_PREVIOUS
        end if
!
!
        SGS_param1%clipping_limit                                       &
     &        = set_fixed_Csim(zero, sgs_ctl%clipping_limit_ctl)
!
        SGS_param1%SGS_hf_factor                                        &
     &        = set_fixed_Csim(one, sgs_ctl%SGS_hf_factor_ctl)
        SGS_param1%SGS_mf_factor                                        &
     &        = set_fixed_Csim(one, sgs_ctl%SGS_mf_factor_ctl)
        SGS_param1%SGS_mawell_factor                                    &
     &        = set_fixed_Csim(one, sgs_ctl%SGS_mxwl_factor_ctl)
        SGS_param1%SGS_uxb_factor                                       &
     &        = set_fixed_Csim(one, sgs_ctl%SGS_uxb_factor_ctl)
        SGS_param1%SGS_cf_factor                                        &
     &        = set_fixed_Csim(one, sgs_ctl%SGS_cf_factor_ctl)
!
!
        SGS_param1%iflag_Csim_marging = id_SGS_DIR_LSQ
        if (sgs_ctl%SGS_marging_ctl%iflag .gt. 0) then
          tmpchara = sgs_ctl%SGS_marging_ctl%charavalue
!
          if      (cmp_no_case(tmpchara, 'lsq_over_directions')         &
     &        .or. cmp_no_case(tmpchara, 'lsq')) then
             SGS_param1%iflag_Csim_marging = id_SGS_DIR_LSQ
          else if (cmp_no_case(tmpchara, 'average_over_directions')     &
     &        .or. cmp_no_case(tmpchara, 'average')) then
             SGS_param1%iflag_Csim_marging = id_SGS_DIR_AVERAGE
          else if (cmp_no_case(tmpchara, 'weighting_by_correlation')    &
     &        .or. cmp_no_case(tmpchara, 'weighting')) then
             SGS_param1%iflag_Csim_marging = id_SGS_DIR_CORRELATE
          end if
        end if
!
        SGS_param1%min_step_dynamic = 1
        if (sgs_ctl%min_step_dynamic_ctl%iflag .gt. 0) then
          SGS_param1%min_step_dynamic                                   &
     &      = sgs_ctl%min_step_dynamic_ctl%intvalue
        end if
!
        SGS_param1%max_step_dynamic = 50
        if (sgs_ctl%max_step_dynamic_ctl%iflag .gt. 0) then
          SGS_param1%max_step_dynamic                                   &
     &      = sgs_ctl%max_step_dynamic_ctl%intvalue
        end if
!
        SGS_param1%extend_SGS_dt                                        &
     &      = sgs_ctl%delta_to_shrink_dynamic_ctl%realvalue
        SGS_param1%extend_SGS_dt                                        &
     &      = sgs_ctl%delta_to_extend_dynamic_ctl%realvalue
      end if
!
!
      SGS_param1%iflag_parterbuation = id_turn_OFF
      if(sgs_ctl%SGS_perturbation_ctl%iflag .gt. 0) then
          tmpchara = sgs_ctl%SGS_perturbation_ctl%charavalue
          if (cmp_no_case(tmpchara, 'reference')                        &
     &       ) SGS_param1%iflag_parterbuation = id_SGS_REFERENCE
          if (cmp_no_case(tmpchara, 'average')                          &
     &       ) SGS_param1%iflag_parterbuation = id_SGS_REF_AVERAGE
      end if
!
!
      SGS_param1%itype_Csym                                             &
     &      = set_Csim_type(id_CSIM_FIELD,                              &
     &                      sgs_ctl%SGS_model_coef_type_ctl)
!
      SGS_param1%itype_Csym_h_flux                                      &
     &      = set_Csim_type(SGS_param1%itype_Csym,                      &
     &                      sgs_ctl%heat_flux_csim_type_ctl)
      SGS_param1%itype_Csym_c_flux                                      &
     &      = set_Csim_type(SGS_param1%itype_Csym,                      &
     &                      sgs_ctl%comp_flux_csim_type_ctl)
      SGS_param1%itype_Csym_m_flux                                      &
     &      = set_Csim_type(SGS_param1%itype_Csym,                      &
     &                      sgs_ctl%mom_flux_csim_type_ctl)
      SGS_param1%itype_Csym_maxwell                                     &
     &      = set_Csim_type(SGS_param1%itype_Csym,                      &
     &                      sgs_ctl%maxwell_csim_type_ctl)
      SGS_param1%itype_Csym_uxb                                         &
     &      = set_Csim_type(SGS_param1%itype_Csym,                      &
     &                      sgs_ctl%uxb_csim_type_ctl)
!
      SGS_param1%icoord_Csim = 0
      if(sgs_ctl%SGS_model_coef_coord_ctl%iflag .gt. 0) then
          tmpchara = sgs_ctl%SGS_model_coef_coord_ctl%charavalue
          if(   cmp_no_case(tmpchara, 'spherical')                      &
     &     .or. cmp_no_case(tmpchara, 'sph')) then
             SGS_param1%icoord_Csim = iflag_spherical
          end if
          if(   cmp_no_case(tmpchara, 'cylindrical')                    &
     &     .or. cmp_no_case(tmpchara, 'spz')) then
            SGS_param1%icoord_Csim = iflag_cylindrical
          end if
      end if
!
!
      if (SGS_param1%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        call s_set_control_ele_layering(elayer_ctl)
      end if
!
      if (SGS_param1%iflag_SGS .eq. id_SGS_NL_grad) then
        if (ffile_ctl%filter_elen_head_ctl%iflag .eq. 1) then
          filter_elen_head = ffile_ctl%filter_elen_head_ctl%charavalue
        end if
!
        call choose_file_format                                         &
     &     (ffile_ctl%filter_elen_format, ifmt_filter_elen)
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'filter_elen_head: ', trim(filter_elen_head)
          write(*,*) 'filter_elen_format: ', ifmt_filter_elen
        end if
      end if
!
      if (SGS_param1%iflag_SGS .ne. id_SGS_none) then
        if (iflag_debug .gt. 0)  then
          write(*,*) 'itype_SGS_model_coef: ',  SGS_param1%itype_Csym
          write(*,*) 'icoord_SGS_model_coef: ', SGS_param1%icoord_Csim
!
          write(*,*) 'SGS_hf_factor:     ', SGS_param1%SGS_hf_factor
          write(*,*) 'SGS_mf_factor:     ', SGS_param1%SGS_mf_factor
          write(*,*) 'SGS_mawell_factor: ',                             &
     &               SGS_param1%SGS_mawell_factor
          write(*,*) 'SGS_uxb_factor:    ', SGS_param1%SGS_uxb_factor
          write(*,*) 'SGS_cf_factor:     ', SGS_param1%SGS_cf_factor
!
          write(*,*) 'itype_SGS_h_flux_coef:  ',                        &
     &              SGS_param1%itype_Csym_h_flux
          write(*,*) 'itype_SGS_c_flux_coef:  ',                        &
     &              SGS_param1%itype_Csym_c_flux
          write(*,*) 'itype_SGS_m_flux_coef:  ',                        &
     &              SGS_param1%itype_Csym_m_flux
          write(*,*) 'itype_SGS_maxwell_coef: ',                        &
     &              SGS_param1%itype_Csym_maxwell
          write(*,*) 'itype_SGS_uxb_coef:     ',                        &
     &              SGS_param1%itype_Csym_uxb
        end if
      end if
!
      if (SGS_param1%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        iflag_rst_sgs_coef_code                                         &
     &          = ffile_ctl%model_coef_ini_head_ctl%iflag
        if(iflag_rst_sgs_coef_code .gt. 0) then
          rst_sgs_coef_head                                             &
     &          = ffile_ctl%model_coef_ini_head_ctl%charavalue
        end if
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'rst_sgs_coef_head: ',   trim(rst_sgs_coef_head)
        end if
      end if
!
      end subroutine set_control_FEM_SGS
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SGS_commute(sgs_ctl, cmt_param)
!
      use m_phys_labels
      use m_control_parameter
      use t_ctl_data_SGS_model
      use t_SGS_control_parameter
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(commutation_control_params), intent(inout) :: cmt_param
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
      if (SGS_param1%iflag_SGS .eq. id_SGS_none) return
!
      if (sgs_ctl%commutate_fld_ctl%icou .gt. 0) then
        do i = 1, sgs_ctl%commutate_fld_ctl%num
          tmpchara = sgs_ctl%commutate_fld_ctl%c_tbl(i)
          if(     tmpchara .eq. fhd_temp) then
            cmt_param%iflag_c_temp =      id_SGS_commute_ON
          else if(tmpchara .eq. fhd_velo) then
            cmt_param%iflag_c_velo =      id_SGS_commute_ON
          else if(tmpchara .eq. fhd_magne) then
            cmt_param%iflag_c_magne =     id_SGS_commute_ON
          else if(tmpchara .eq. fhd_vecp) then
            cmt_param%iflag_c_magne =     id_SGS_commute_ON
          else if(tmpchara .eq. fhd_light) then
            cmt_param%iflag_c_cf =    id_SGS_commute_ON
!
          else if(tmpchara .eq. thd_heat_flux) then
            cmt_param%iflag_c_hf =   id_SGS_commute_ON
          else if(tmpchara .eq. thd_advection) then
            cmt_param%iflag_c_mf =   id_SGS_commute_ON
          else if(tmpchara .eq. thd_lorentz) then
            cmt_param%iflag_c_lorentz = id_SGS_commute_ON
          else if(tmpchara .eq. thd_induction) then
            cmt_param%iflag_c_uxb = id_SGS_commute_ON
          else if(tmpchara .eq. thd_comp_flux) then
            cmt_param%iflag_c_light =  id_SGS_commute_ON
          end if
        end do
!
        call dealloc_control_array_chara(sgs_ctl%commutate_fld_ctl)
!
        cmt_param%iflag_c_linear                                        &
     &        =  cmt_param%iflag_c_temp +  cmt_param%iflag_c_velo       &
     &         + cmt_param%iflag_c_magne + cmt_param%iflag_c_light
        cmt_param%iflag_c_nonlinars                                     &
     &        =  cmt_param%iflag_c_hf                                   &
     &         + cmt_param%iflag_c_mf + cmt_param%iflag_c_lorentz       &
     &         + cmt_param%iflag_c_uxb + cmt_param%iflag_c_cf
        cmt_param%iflag_commute = cmt_param%iflag_c_linear              &
     &                              + cmt_param%iflag_c_nonlinars
      end if
!
      if (SGS_param1%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        cmt_param%iset_DIFF_coefs = 0
        if (sgs_ctl%DIFF_model_coef_ctl%iflag .ne. 0) then
          tmpchara = sgs_ctl%DIFF_model_coef_ctl%charavalue
!
          if (cmp_no_case(tmpchara, 'whole_domain')) then
            cmt_param%iset_DIFF_coefs = 0
          else if (cmp_no_case(tmpchara, 'layerd')) then
            cmt_param%iset_DIFF_coefs = 1
          end if
        end if
      end if
!
      if (iflag_debug .gt. 0)  then
        write(*,*) 'iflag_commute_temp:     ', cmt_param%iflag_c_temp
        write(*,*) 'iflag_commute_velo:     ', cmt_param%iflag_c_velo
        write(*,*) 'iflag_commute_magne:    ',                          &
     &              cmt_param%iflag_c_magne
        write(*,*) 'iflag_commute_composit: ',                          &
     &              cmt_param%iflag_c_light
        write(*,*) 'iflag_commute_heat:     ', cmt_param%iflag_c_hf
        write(*,*) 'iflag_commute_inertia:  ', cmt_param%iflag_c_mf
        write(*,*) 'iflag_commute_lorentz:  ',                          &
     &              cmt_param%iflag_c_lorentz
        write(*,*) 'iflag_commute_induction:', cmt_param%iflag_c_uxb
        write(*,*) 'iflag_commute_c_flux:   ', cmt_param%iflag_c_cf
!
        write(*,*) 'iset_DIFF_coefs:   ', cmt_param%iset_DIFF_coefs
      end if
!
      end subroutine set_control_SGS_commute
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
      real(kind = kreal) function set_fixed_Csim                        &
     &                          (default, csim_fact_ctl)
!
      use t_control_elements
!
      real(kind = kreal), intent(in) :: default
      type(read_real_item), intent(in) :: csim_fact_ctl
!
      set_fixed_Csim = default
!
      if(csim_fact_ctl%iflag .eq. 0) return
      set_fixed_Csim = csim_fact_ctl%realvalue
!
      end function set_fixed_Csim
!
! -----------------------------------------------------------------------
!
      end module set_control_4_SGS
