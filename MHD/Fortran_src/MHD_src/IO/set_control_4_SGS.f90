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
!!     subroutine s_set_control_4_SGS
!!@endverbatim
!
      module set_control_4_SGS
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_SGS
!
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
      use m_file_format_switch
      use m_phys_labels
      use m_control_parameter
      use m_ctl_data_SGS_model
      use m_filter_file_names
      use m_ctl_data_filter_files
      use sgs_ini_model_coefs_IO
      use set_control_ele_layering
!
      integer(kind = kint) :: i
!
!
!   set control parameters for SGS model
!
       iflag_SGS_model =      id_SGS_none
       iflag_dynamic_SGS =    id_SGS_DYNAMIC_OFF
       iflag_SGS_filter =     0
!
      if (i_SGS_model .eq. 0) then
        iflag_SGS_model =   id_SGS_none
        iflag_dynamic_SGS = id_SGS_DYNAMIC_OFF
      else
        if      (SGS_model_name_ctl .eq. 'no'                           &
     &      .or. SGS_model_name_ctl .eq. 'No'                           &
     &      .or. SGS_model_name_ctl .eq. 'NO' ) then
          iflag_SGS_model =   id_SGS_none
          iflag_dynamic_SGS = id_SGS_DYNAMIC_OFF
!
        else if (SGS_model_name_ctl .eq. 'gradient'                     &
     &      .or. SGS_model_name_ctl .eq. 'Gradient'                     &
     &      .or. SGS_model_name_ctl .eq. 'GRADIENT' ) then
          iflag_SGS_model =   id_SGS_NL_grad
          iflag_dynamic_SGS = id_SGS_DYNAMIC_OFF
!
        else if (SGS_model_name_ctl .eq. 'similarity'                   &
     &      .or. SGS_model_name_ctl .eq. 'Similarity'                   &
     &      .or. SGS_model_name_ctl .eq. 'SIMILARITY' ) then
          iflag_SGS_model =   id_SGS_similarity
          iflag_dynamic_SGS = id_SGS_DYNAMIC_OFF
!
        else if (SGS_model_name_ctl .eq. 'dynamic'                      &
     &      .or. SGS_model_name_ctl .eq. 'Dynamic'                      &
     &      .or. SGS_model_name_ctl .eq. 'DYNAMIC' ) then
          iflag_SGS_model =   id_SGS_NL_grad
          iflag_dynamic_SGS = id_SGS_DYNAMIC_ON
!
        else if (SGS_model_name_ctl .eq. 'dynamic_similarity'           &
     &      .or. SGS_model_name_ctl .eq. 'Dynamic_similarity'           &
     &      .or. SGS_model_name_ctl .eq. 'DYNAMIC_SIMILARITY'           &
     &      .or. SGS_model_name_ctl .eq. 'dynamic_simi'                 &
     &      .or. SGS_model_name_ctl .eq. 'Dynamic_simi'                 &
     &      .or. SGS_model_name_ctl .eq. 'DYNAMIC_SIMI'  ) then
          iflag_SGS_model =   id_SGS_similarity
          iflag_dynamic_SGS = id_SGS_DYNAMIC_ON
!
        else if (SGS_model_name_ctl .eq. 'diffusion'                    &
     &      .or. SGS_model_name_ctl .eq. 'Diffusino'                    &
     &      .or. SGS_model_name_ctl .eq. 'DIFFUSION' ) then
          iflag_SGS_model =   id_SGS_diffusion
          iflag_dynamic_SGS = id_SGS_DYNAMIC_ON
        end if
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'iflag_SGS_model',   iflag_SGS_model
        write(*,*) 'iflag_dynamic_SGS', iflag_dynamic_SGS
      end if
!
!
      n_filter_final = 1
      n_second = 1
      n_quad = 2
!
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        iset_DIFF_model_coefs = 0
        if (i_DIFF_coefs.ne.0) then
          if (  DIFF_model_coef_ctl.eq.'whole_domain'                   &
           .or. DIFF_model_coef_ctl.eq.'Whole_domain'                   &
     &     .or. DIFF_model_coef_ctl.eq.'WHOLE_DOMAIN'                   &
     &       )               iset_DIFF_model_coefs = 0
          if (  DIFF_model_coef_ctl.eq.'layerd'                         &
     &     .or. DIFF_model_coef_ctl.eq.'Layerd'                         &
     &     .or. DIFF_model_coef_ctl.eq.'LAYERD'                         &
     &       )               iset_DIFF_model_coefs = 1
        end if
!
!
        if (i_SGS_clips.eq.0) then
          e_message                                                     &
     &      = 'Set cliping method for model coefficient'
          call parallel_abort(90, e_message)
        else
!
          if (  SGS_negative_clip_ctl.eq.'none'                         &
     &     .or. SGS_negative_clip_ctl.eq.'None'                         &
     &     .or. SGS_negative_clip_ctl.eq.'NONE'                         &
     &       )               iset_SGS_nagetive_clip = 0
          if (  SGS_negative_clip_ctl.eq.'zero'                         &
     &     .or. SGS_negative_clip_ctl.eq.'Zero'                         &
     &     .or. SGS_negative_clip_ctl.eq.'ZERO'                         &
     &       )               iset_SGS_nagetive_clip = 1
          if (  SGS_negative_clip_ctl.eq.'keep'                         &
     &     .or. SGS_negative_clip_ctl.eq.'Keep'                         &
     &     .or. SGS_negative_clip_ctl.eq.'KEEP'                         &
     &       )               iset_SGS_nagetive_clip = 2
        end if
!
!
        if (i_SGS_clip_limit .gt. 0) then
          SGS_clipping_limit = clipping_limit_ctl
        else
          SGS_clipping_limit = 0.0d0
        end if
!
        if (i_SGS_hf_factor .gt. 0) then
          SGS_hf_factor = SGS_hf_factor_ctl
        else
          SGS_hf_factor = 1.0d0
        end if
!
        if (i_SGS_mf_factor .gt. 0) then
          SGS_mf_factor = SGS_mf_factor_ctl
        else
          SGS_mf_factor = 1.0d0
        end if
!
        if (i_SGS_mxwl_factor .gt. 0) then
          SGS_mawell_factor = SGS_mxwl_factor_ctl
        else
          SGS_mawell_factor = 1.0d0
        end if
!
        if (i_SGS_uxb_factor .gt. 0) then
          SGS_uxb_factor = SGS_uxb_factor_ctl
        else
          SGS_uxb_factor = 1.0d0
        end if
!
!
        if (i_SGS_marging.eq.0) then
          iset_SGS_coef_marging = 0
        else
!
          if (  SGS_marging_ctl.eq.'lsq_over_directions'                &
     &     .or. SGS_marging_ctl.eq.'Lsq_over_directions'                &
     &     .or. SGS_marging_ctl.eq.'LSQ_OVER_DIRECTIONS'                &
     &     .or. SGS_marging_ctl.eq.'lsq'                                &
     &     .or. SGS_marging_ctl.eq.'Lsq'                                &
     &     .or. SGS_marging_ctl.eq.'LSQ'                                &
     &       )               iset_SGS_coef_marging = 0
          if (  SGS_marging_ctl.eq.'average_over_directions'            &
     &     .or. SGS_marging_ctl.eq.'Average_over_directions'            &
     &     .or. SGS_marging_ctl.eq.'AVERAGE_OVER_DIRECTIONS'            &
     &     .or. SGS_marging_ctl.eq.'average'                            &
     &     .or. SGS_marging_ctl.eq.'Average'                            &
     &     .or. SGS_marging_ctl.eq.'AVERAGE'                            &
     &       )               iset_SGS_coef_marging = 1
          if (  SGS_marging_ctl.eq.'weighting_by_correlation'           &
     &     .or. SGS_marging_ctl.eq.'Weighting_by_correlation'           &
     &     .or. SGS_marging_ctl.eq.'WEIGHTING_BY_CORRELATION'           &
     &     .or. SGS_marging_ctl.eq.'weighting'                          &
     &     .or. SGS_marging_ctl.eq.'Weighting'                          &
     &     .or. SGS_marging_ctl.eq.'WEIGHTING'                          &
     &       )               iset_SGS_coef_marging = 2
!
        end if
!
        if (i_min_step_dynamic.gt.0) then
          min_step_dynamic = min_step_dynamic_ctl
        else
          min_step_dynamic = 1
        end if
!
        if (i_max_step_dynamic.gt.0) then
          max_step_dynamic = max_step_dynamic_ctl
        else
          max_step_dynamic = 50
        end if
!
        delta_to_shrink_dynamic = delta_to_shrink_dynamic_ctl
        delta_to_extend_dynamic = delta_to_extend_dynamic_ctl
      end if
!
!
!  set applied terms
!
      iflag_SGS_heat      = id_SGS_none
      iflag_SGS_inertia   = id_SGS_none
      iflag_SGS_lorentz   = id_SGS_none
      iflag_SGS_induction = id_SGS_none
      iflag_SGS_gravity =   id_SGS_none
!
      if (iflag_SGS_model .ne. id_SGS_none) then
        if (i_n_SGS_terms.eq.0) then
            e_message = 'Set equations to apply SGS model'
            call parallel_abort(90, e_message)
        else
!
          do i = 1, num_SGS_term_ctl
            if ( SGS_term_name_ctl(i) .eq. thd_heat_flux) then
              iflag_SGS_heat =      iflag_SGS_model
            else if ( SGS_term_name_ctl(i) .eq. thd_advection) then
              iflag_SGS_inertia =   iflag_SGS_model
            else if ( SGS_term_name_ctl(i) .eq. thd_lorentz) then
              iflag_SGS_lorentz =   iflag_SGS_model
            else if ( SGS_term_name_ctl(i) .eq. thd_induction) then
              iflag_SGS_induction = iflag_SGS_model
            else if ( SGS_term_name_ctl(i) .eq. thd_gravity) then
              iflag_SGS_gravity = iflag_SGS_model
            end if
          end do
        end if
!
!
        if (i_commutation_fld .gt. 0) then
          do i = 1, num_commutation_fld_ctl
            if ( commutation_fld_ctl(i) .eq. fhd_temp) then
              iflag_commute_temp =      id_SGS_commute_ON
            else if ( commutation_fld_ctl(i) .eq. fhd_velo) then
              iflag_commute_velo =      id_SGS_commute_ON
            else if ( commutation_fld_ctl(i) .eq. fhd_magne) then
              iflag_commute_magne =     id_SGS_commute_ON
            else if ( commutation_fld_ctl(i) .eq. fhd_vecp) then
              iflag_commute_magne =     id_SGS_commute_ON
            else if ( commutation_fld_ctl(i) .eq. fhd_light) then
              iflag_commute_c_flux =    id_SGS_commute_ON
!
            else if ( commutation_fld_ctl(i) .eq. thd_heat_flux) then
              iflag_commute_heat =      id_SGS_commute_ON
            else if ( commutation_fld_ctl(i) .eq. thd_advection) then
              iflag_commute_inertia =   id_SGS_commute_ON
            else if ( commutation_fld_ctl(i) .eq. thd_lorentz) then
              iflag_commute_lorentz =   id_SGS_commute_ON
            else if ( commutation_fld_ctl(i) .eq. thd_induction) then
              iflag_commute_induction = id_SGS_commute_ON
            else if ( commutation_fld_ctl(i) .eq. thd_comp_flux) then
              iflag_commute_composit =  id_SGS_commute_ON
            end if
          end do
!
          iflag_commute_linear                                          &
     &        =  iflag_commute_temp +  iflag_commute_velo               &
     &         + iflag_commute_magne + iflag_commute_composit
          iflag_commute_nonlinar                                        &
     &        =  iflag_commute_heat +    iflag_commute_inertia          &
     &         + iflag_commute_lorentz + iflag_commute_induction        &
     &         + iflag_commute_c_flux
          iflag_commute_correction                                      &
     &        =  iflag_commute_linear + iflag_commute_nonlinar
        end if
!
      end if
!
!
!
      iflag_SGS_parterbuation = 0
      if(i_SGS_perturbation_ctl .gt. 0) then
          if (  SGS_perturbation_ctl.eq.'reference'                     &
     &     .or. SGS_perturbation_ctl.eq.'Reference'                     &
     &     .or. SGS_perturbation_ctl.eq.'REFERENCE'                     &
     &       )               iflag_SGS_parterbuation = 1
          if (  SGS_perturbation_ctl.eq.'average'                       &
     &     .or. SGS_perturbation_ctl.eq.'Average'                       &
     &     .or. SGS_perturbation_ctl.eq.'AVERAGE'                       &
     &       )               iflag_SGS_parterbuation = 2
      end if
!
!
      if(i_hf_csim_type_ctl .gt. 0) then
          if (  heat_flux_csim_type_ctl.eq.'components'                 &
     &     .or. heat_flux_csim_type_ctl.eq.'Components'                 &
     &     .or. heat_flux_csim_type_ctl.eq.'COMPONENTS'                 &
     &       ) itype_SGS_h_flux_coef = 1
      end if
!
      if(i_mf_csim_type_ctl .gt. 0) then
          if (  mom_flux_csim_type_ctl.eq.'components'                  &
     &     .or. mom_flux_csim_type_ctl.eq.'Components'                  &
     &     .or. mom_flux_csim_type_ctl.eq.'COMPONENTS'                  &
     &       ) itype_SGS_m_flux_coef = 1
      end if
!
      if(i_mxwl_csim_type_ctl .gt. 0) then
          if (  maxwell_csim_type_ctl.eq.'components'                   &
     &     .or. maxwell_csim_type_ctl.eq.'Components'                   &
     &     .or. maxwell_csim_type_ctl.eq.'COMPONENTS'                   &
     &       ) itype_SGS_maxwell_coef = 1
      end if
!
      if(i_uxb_csim_type_ctl .gt. 0) then
          if (  uxb_csim_type_ctl.eq.'components'                       &
     &     .or. uxb_csim_type_ctl.eq.'Components'                       &
     &     .or. uxb_csim_type_ctl.eq.'COMPONENTS'                       &
     &       ) itype_SGS_uxb_coef = 1
      end if
!
      itype_SGS_model_coef = 0
      if(i_model_coef_type_ctl .gt. 0) then
          if (  SGS_model_coef_type_ctl.eq.'components'                 &
     &     .or. SGS_model_coef_type_ctl.eq.'Components'                 &
     &     .or. SGS_model_coef_type_ctl.eq.'COMPONENTS'                 &
     &       ) then
          itype_SGS_model_coef = 1
!
          itype_SGS_h_flux_coef =   1
          itype_SGS_m_flux_coef =   1
          itype_SGS_maxwell_coef =  1
          itype_SGS_uxb_coef =      1
        end if
      end if
!
      icoord_SGS_model_coef = 0
      if(i_model_coef_coord_ctl .gt. 0) then
          if (  SGS_model_coef_coord_ctl.eq.'spherical'                 &
     &     .or. SGS_model_coef_coord_ctl.eq.'Spherical'                 &
     &     .or. SGS_model_coef_coord_ctl.eq.'SPHERICAL'                 &
     &     .or. SGS_model_coef_coord_ctl.eq.'sph'                       &
     &     .or. SGS_model_coef_coord_ctl.eq.'SPH'                       &
     &       )               icoord_SGS_model_coef = 1
          if (  SGS_model_coef_coord_ctl.eq.'cylindrical'               &
     &     .or. SGS_model_coef_coord_ctl.eq.'Cylindrical'               &
     &     .or. SGS_model_coef_coord_ctl.eq.'CYLINDRICAL'               &
     &     .or. SGS_model_coef_coord_ctl.eq.'spz'                       &
     &     .or. SGS_model_coef_coord_ctl.eq.'SPZ'                       &
     &       )               icoord_SGS_model_coef = 2
      end if
!
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        call s_set_control_ele_layering
      end if
!
      if (iflag_SGS_model .eq. id_SGS_NL_grad) then
        if (i_filter_elen_head_ctl .eq. 1) then
          filter_elen_head = filter_elen_head_ctl
        else
          filter_elen_head = filter_elen_def_hd
        end if
!
        call choose_file_format(filter_elen_format, i_filter_elen_fmt,  &
     &      ifmt_filter_elen)
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'filter_elen_head: ', trim(filter_elen_head)
          write(*,*) 'filter_elen_format: ', ifmt_filter_elen
        end if
      end if
!
      if (iflag_SGS_model .ne. id_SGS_none) then
        if (iflag_debug .gt. 0)  then
          write(*,*) 'iflag_SGS_heat:         ',iflag_SGS_heat
          write(*,*) 'iflag_SGS_inertia:      ',iflag_SGS_inertia
          write(*,*) 'iflag_SGS_lorentz:      ',iflag_SGS_lorentz
          write(*,*) 'iflag_SGS_induction:    ',iflag_SGS_induction
          write(*,*) 'iflag_SGS_gravity:      ',iflag_SGS_gravity
!
          write(*,*) 'iflag_commute_temp:     ',iflag_commute_temp
          write(*,*) 'iflag_commute_velo:     ',iflag_commute_velo
          write(*,*) 'iflag_commute_magne:    ',iflag_commute_magne
          write(*,*) 'iflag_commute_c_flux:   ',iflag_commute_c_flux
          write(*,*) 'iflag_commute_heat:     ',iflag_commute_heat
          write(*,*) 'iflag_commute_inertia:  ',iflag_commute_inertia
          write(*,*) 'iflag_commute_lorentz:  ',iflag_commute_lorentz
          write(*,*) 'iflag_commute_induction:',iflag_commute_induction
          write(*,*) 'iflag_commute_composit: ',iflag_commute_composit
!
          write(*,*) 'itype_SGS_model_coef: ',  itype_SGS_model_coef
          write(*,*) 'icoord_SGS_model_coef: ', icoord_SGS_model_coef
!
          write(*,*) 'SGS_hf_factor:     ', SGS_hf_factor
          write(*,*) 'SGS_mf_factor:     ', SGS_mf_factor
          write(*,*) 'SGS_mawell_factor: ', SGS_mawell_factor
          write(*,*) 'SGS_uxb_factor:    ', SGS_uxb_factor
!
          write(*,*) 'itype_SGS_h_flux_coef:  ', itype_SGS_h_flux_coef
          write(*,*) 'itype_SGS_m_flux_coef:  ', itype_SGS_m_flux_coef
          write(*,*) 'itype_SGS_maxwell_coef: ', itype_SGS_maxwell_coef
          write(*,*) 'itype_SGS_uxb_coef:     ', itype_SGS_uxb_coef
        end if
      end if
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        iflag_rst_sgs_coef_code = i_model_coef_ini_head
        if(iflag_rst_sgs_coef_code .gt. 0) then
          rst_sgs_coef_head = model_coef_ini_head_ctl
        end if
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'rst_sgs_coef_head: ',   trim(rst_sgs_coef_head)
        end if
      end if
!
      end subroutine s_set_control_4_SGS
!
! -----------------------------------------------------------------------
!
      end module set_control_4_SGS
