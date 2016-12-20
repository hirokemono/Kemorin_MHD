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
!!      subroutine set_control_SGS_model
!!      subroutine set_control_SPH_SGS(sph_filters)
!!      subroutine set_control_FEM_SGS
!!@endverbatim
!
      module set_control_4_SGS
!
      use m_precision
      use m_error_IDs
      use m_constants
      use calypso_mpi
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SPH_SGS(sph_filters)
!
      use m_control_parameter
      use m_ctl_data_SGS_model
      use t_sph_filtering_data
!
      type(sph_filters_type), intent(inout) :: sph_filters(1)
!
!
      if(iflag_SGS_model .eq. 0) return
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
      call deallocate_sph_filter_ctl
!
      end subroutine set_control_SPH_SGS
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SGS_model
!
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_labels
      use m_control_parameter
      use m_ctl_data_SGS_model
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
!   set control parameters for SGS model
!
       iflag_SGS_model =      id_SGS_none
       iflag_dynamic_SGS =    id_SGS_DYNAMIC_OFF
       iflag_SGS_filter =     id_SGS_NO_FILTERING
!
      if (SGS_model_name_ctl%iflag .eq. 0) then
        iflag_SGS_model =   id_SGS_none
        iflag_dynamic_SGS = id_SGS_DYNAMIC_OFF
      else
        tmpchara = SGS_model_name_ctl%charavalue
!
        if      (no_flag(tmpchara)) then
          iflag_SGS_model =   id_SGS_none
          iflag_dynamic_SGS = id_SGS_DYNAMIC_OFF
!
        else if (cmp_no_case(tmpchara, 'gradient')) then
          iflag_SGS_model =   id_SGS_NL_grad
          iflag_dynamic_SGS = id_SGS_DYNAMIC_OFF
!
        else if (cmp_no_case(tmpchara, 'similarity')) then
          iflag_SGS_model =   id_SGS_similarity
          iflag_dynamic_SGS = id_SGS_DYNAMIC_OFF
!
        else if (cmp_no_case(tmpchara, 'dynamic')) then
          iflag_SGS_model =   id_SGS_NL_grad
          iflag_dynamic_SGS = id_SGS_DYNAMIC_ON
!
        else if (cmp_no_case(tmpchara, 'dynamic_similarity')            &
     &      .or. cmp_no_case(tmpchara, 'dynamic_simi')) then
          iflag_SGS_model =   id_SGS_similarity
          iflag_dynamic_SGS = id_SGS_DYNAMIC_ON
!
        else if (cmp_no_case(tmpchara,'diffusion')) then
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
!  set applied terms
!
      iflag_SGS_heat      = id_SGS_none
      iflag_SGS_comp_flux = id_SGS_none
      iflag_SGS_inertia   = id_SGS_none
      iflag_SGS_lorentz   = id_SGS_none
      iflag_SGS_induction = id_SGS_none
      iflag_SGS_gravity =   id_SGS_none
!
      if (iflag_SGS_model .ne. id_SGS_none) then
        if (SGS_terms_ctl%icou .eq. izero) then
            e_message = 'Set equations to apply SGS model'
            call calypso_MPI_abort(ierr_SGS, e_message)
        else
!
          do i = 1, SGS_terms_ctl%num
            if(     SGS_terms_ctl%c_tbl(i) .eq. thd_heat_flux) then
              iflag_SGS_heat =      iflag_SGS_model
            else if(SGS_terms_ctl%c_tbl(i) .eq. thd_advection) then
              iflag_SGS_inertia =   iflag_SGS_model
            else if(SGS_terms_ctl%c_tbl(i) .eq. thd_lorentz) then
              iflag_SGS_lorentz =   iflag_SGS_model
            else if(SGS_terms_ctl%c_tbl(i) .eq. thd_induction) then
              iflag_SGS_induction = iflag_SGS_model
            else if(SGS_terms_ctl%c_tbl(i) .eq. thd_gravity) then
              iflag_SGS_gravity = iflag_SGS_model
            end if
          end do
!
          call dealloc_control_array_chara(SGS_terms_ctl)
        end if
!
!
        if (commutate_fld_ctl%icou .gt. 0) then
          do i = 1, commutate_fld_ctl%num
            if(     commutate_fld_ctl%c_tbl(i) .eq. fhd_temp) then
              iflag_commute_temp =      id_SGS_commute_ON
            else if(commutate_fld_ctl%c_tbl(i) .eq. fhd_velo) then
              iflag_commute_velo =      id_SGS_commute_ON
            else if(commutate_fld_ctl%c_tbl(i) .eq. fhd_magne) then
              iflag_commute_magne =     id_SGS_commute_ON
            else if(commutate_fld_ctl%c_tbl(i) .eq. fhd_vecp) then
              iflag_commute_magne =     id_SGS_commute_ON
            else if(commutate_fld_ctl%c_tbl(i) .eq. fhd_light) then
              iflag_commute_c_flux =    id_SGS_commute_ON
!
            else if(commutate_fld_ctl%c_tbl(i) .eq. thd_heat_flux) then
              iflag_commute_heat =      id_SGS_commute_ON
            else if(commutate_fld_ctl%c_tbl(i) .eq. thd_advection) then
              iflag_commute_inertia =   id_SGS_commute_ON
            else if(commutate_fld_ctl%c_tbl(i) .eq. thd_lorentz) then
              iflag_commute_lorentz =   id_SGS_commute_ON
            else if(commutate_fld_ctl%c_tbl(i) .eq. thd_induction) then
              iflag_commute_induction = id_SGS_commute_ON
            else if(commutate_fld_ctl%c_tbl(i) .eq. thd_comp_flux) then
              iflag_commute_composit =  id_SGS_commute_ON
            end if
          end do
!
          call dealloc_control_array_chara(commutate_fld_ctl)
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
        end if
      end if
!
      end subroutine set_control_SGS_model
!
! -----------------------------------------------------------------------
!
      subroutine set_control_FEM_SGS
!
      use m_machine_parameter
      use m_geometry_constants
      use m_file_format_switch
      use m_phys_labels
      use m_control_parameter
      use m_ctl_data_SGS_model
      use m_filter_file_names
      use m_ctl_data_filter_files
      use sgs_ini_model_coefs_IO
      use set_control_ele_layering
!
      character(len=kchara) :: tmpchara
!
!
      ifilter_final = ifilter_2delta
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        iset_DIFF_model_coefs = 0
        if (DIFF_model_coef_ctl%iflag .ne. 0) then
          tmpchara = DIFF_model_coef_ctl%charavalue
!
          if (cmp_no_case(tmpchara, 'whole_domain')) then
            iset_DIFF_model_coefs = 0
          else if (cmp_no_case(tmpchara, 'layerd')) then
            iset_DIFF_model_coefs = 1
          end if
        end if
!
!
        if (SGS_negative_clip_ctl%iflag .eq. 0) then
          e_message                                                     &
     &      = 'Set cliping method for model coefficient'
          call calypso_MPI_abort(ierr_SGS, e_message)
        else
          tmpchara = SGS_negative_clip_ctl%charavalue
!
          if (cmp_no_case(tmpchara, 'none')) iset_SGS_nagetive_clip = 0
          if (cmp_no_case(tmpchara, 'zero')) iset_SGS_nagetive_clip = 1
          if (cmp_no_case(tmpchara, 'keep')) iset_SGS_nagetive_clip = 2
        end if
!
!
        if (clipping_limit_ctl%iflag .gt. 0) then
          SGS_clipping_limit = clipping_limit_ctl%realvalue
        else
          SGS_clipping_limit = 0.0d0
        end if
!
        if (SGS_hf_factor_ctl%iflag .gt. 0) then
          SGS_hf_factor = SGS_hf_factor_ctl%realvalue
        else
          SGS_hf_factor = 1.0d0
        end if
!
        if (SGS_mf_factor_ctl%iflag .gt. 0) then
          SGS_mf_factor = SGS_mf_factor_ctl%realvalue
        else
          SGS_mf_factor = 1.0d0
        end if
!
        if (SGS_mxwl_factor_ctl%iflag .gt. 0) then
          SGS_mawell_factor = SGS_mxwl_factor_ctl%realvalue
        else
          SGS_mawell_factor = 1.0d0
        end if
!
        if (SGS_uxb_factor_ctl%iflag .gt. 0) then
          SGS_uxb_factor = SGS_uxb_factor_ctl%realvalue
        else
          SGS_uxb_factor = 1.0d0
        end if
!
!
        if (SGS_marging_ctl%iflag .eq. 0) then
          iset_SGS_coef_marging = 0
        else
          tmpchara = SGS_marging_ctl%charavalue
!
          if      (cmp_no_case(tmpchara, 'lsq_over_directions')         &
     &        .or. cmp_no_case(tmpchara, 'lsq')) then
             iset_SGS_coef_marging = 0
          else if (cmp_no_case(tmpchara, 'average_over_directions')     &
     &        .or. cmp_no_case(tmpchara, 'average')) then
             iset_SGS_coef_marging = 1
          else if (cmp_no_case(tmpchara, 'weighting_by_correlation')    &
     &        .or. cmp_no_case(tmpchara, 'weighting')) then
             iset_SGS_coef_marging = 2
          end if
        end if
!
        if (min_step_dynamic_ctl%iflag .gt. 0) then
          min_step_dynamic = min_step_dynamic_ctl%intvalue
        else
          min_step_dynamic = 1
        end if
!
        if (max_step_dynamic_ctl%iflag .gt. 0) then
          max_step_dynamic = max_step_dynamic_ctl%intvalue
        else
          max_step_dynamic = 50
        end if
!
        delta_to_shrink_dynamic = delta_to_shrink_dynamic_ctl%realvalue
        delta_to_extend_dynamic = delta_to_extend_dynamic_ctl%realvalue
      end if
!
!
      iflag_SGS_parterbuation = 0
      if(SGS_perturbation_ctl%iflag .gt. 0) then
          if (cmp_no_case(SGS_perturbation_ctl%charavalue, 'reference') &
     &       )               iflag_SGS_parterbuation = 1
          if (cmp_no_case(SGS_perturbation_ctl%charavalue, 'average')   &
     &       )               iflag_SGS_parterbuation = 2
      end if
!
!
      if(heat_flux_csim_type_ctl%iflag .gt. 0                           &
     &   .and. cmp_no_case(heat_flux_csim_type_ctl%charavalue,          &
     &                    'components')) itype_SGS_h_flux_coef = 1
!
      if(mom_flux_csim_type_ctl%iflag .gt. 0                            &
         .and. cmp_no_case(mom_flux_csim_type_ctl%charavalue,           &
     &                     'components')) itype_SGS_m_flux_coef = 1
!
      if(maxwell_csim_type_ctl%iflag .gt. 0                             &
     &   .and. cmp_no_case(maxwell_csim_type_ctl%charavalue,            &
     &                    'components')) itype_SGS_maxwell_coef = 1
!
      if(uxb_csim_type_ctl%iflag .gt. 0                                 &
        .and. cmp_no_case(uxb_csim_type_ctl%charavalue, 'components')   &
     &       ) itype_SGS_uxb_coef = 1
!
      itype_SGS_model_coef = 0
      if(SGS_model_coef_type_ctl%iflag .gt. 0                           &
     &  .and. cmp_no_case(SGS_model_coef_type_ctl%charavalue,           &
     &                    'components') ) then
        itype_SGS_model_coef = 1
!
        itype_SGS_h_flux_coef =   1
        itype_SGS_m_flux_coef =   1
        itype_SGS_maxwell_coef =  1
        itype_SGS_uxb_coef =      1
      end if
!
      icoord_SGS_model_coef = 0
      if(SGS_model_coef_coord_ctl%iflag .gt. 0) then
          if(   cmp_no_case(SGS_model_coef_coord_ctl%charavalue,        &
     &                      'spherical')                                &
     &     .or. cmp_no_case(SGS_model_coef_coord_ctl%charavalue,        &
     &                      'sph')) then
             icoord_SGS_model_coef = iflag_spherical
          end if
          if(   cmp_no_case(SGS_model_coef_coord_ctl%charavalue,        &
     &                      'cylindrical')                              &
     &     .or. cmp_no_case(SGS_model_coef_coord_ctl%charavalue,        &
     &                      'spz')) then
            icoord_SGS_model_coef = iflag_cylindrical
          end if
      end if
!
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        call s_set_control_ele_layering
      end if
!
      if (iflag_SGS_model .eq. id_SGS_NL_grad) then
        if (filter_elen_head_ctl%iflag .eq. 1) then
          filter_elen_head = filter_elen_head_ctl%charavalue
        end if
!
        call choose_file_format(filter_elen_format, ifmt_filter_elen)
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'filter_elen_head: ', trim(filter_elen_head)
          write(*,*) 'filter_elen_format: ', ifmt_filter_elen
        end if
      end if
!
      if (iflag_SGS_model .ne. id_SGS_none) then
        if (iflag_debug .gt. 0)  then
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
        iflag_rst_sgs_coef_code = model_coef_ini_head_ctl%iflag
        if(iflag_rst_sgs_coef_code .gt. 0) then
          rst_sgs_coef_head = model_coef_ini_head_ctl%charavalue
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
      end module set_control_4_SGS
