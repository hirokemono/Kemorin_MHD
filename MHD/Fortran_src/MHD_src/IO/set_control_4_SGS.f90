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
!!      subroutine set_control_SGS_model                                &
!!     &         (sgs_ctl, SGS_param, filter_param, Csim_file_IO,       &
!!     &          i_step_sgs_coefs)
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!        type(SGS_model_control_params), intent(inout) :: SGS_param
!!        type(field_IO_params), intent(inout) :: Csim_file_IO
!!        type(SGS_filtering_params), intent(inout) :: filter_param
!!@endverbatim
!
      module set_control_4_SGS
!
      use m_precision
      use m_constants
      use m_error_IDs
      use m_machine_parameter
      use calypso_mpi
!
      use t_file_IO_parameter
!
      implicit  none
!
      character(len=kchara), parameter                                  &
     &                      :: def_rst_sgs_coef =  'rst_model_coefs'
      private :: def_rst_sgs_coef
      private :: set_fixed_Csim
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SGS_model                                  &
     &         (sgs_ctl, SGS_param, filter_param, Csim_file_IO,         &
     &          i_step_sgs_coefs)
!
      use m_geometry_constants
      use m_phys_labels
      use m_file_format_switch
      use t_SGS_control_parameter
      use t_ctl_data_SGS_model
      use t_field_data_IO
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(SGS_model_control_params), intent(inout) :: SGS_param
      type(SGS_filtering_params), intent(inout) :: filter_param
      type(field_IO_params), intent(inout) :: Csim_file_IO
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
!   set control parameters for SGS model
!
       SGS_param%iflag_SGS =      id_SGS_none
       SGS_param%iflag_dynamic =  id_SGS_DYNAMIC_OFF
       filter_param%iflag_SGS_filter = id_SGS_NO_FILTERING
!
      if (sgs_ctl%SGS_model_name_ctl%iflag .gt. 0) then
        tmpchara = sgs_ctl%SGS_model_name_ctl%charavalue
!
        if      (no_flag(tmpchara)) then
          SGS_param%iflag_SGS =     id_SGS_none
          SGS_param%iflag_dynamic = id_SGS_DYNAMIC_OFF
!
        else if (cmp_no_case(tmpchara, 'gradient')) then
          SGS_param%iflag_SGS =     id_SGS_NL_grad
          SGS_param%iflag_dynamic = id_SGS_DYNAMIC_OFF
!
        else if (cmp_no_case(tmpchara, 'similarity')) then
          SGS_param%iflag_SGS =     id_SGS_similarity
          SGS_param%iflag_dynamic = id_SGS_DYNAMIC_OFF
!
        else if (cmp_no_case(tmpchara, 'dynamic')) then
          SGS_param%iflag_SGS =     id_SGS_NL_grad
          SGS_param%iflag_dynamic = id_SGS_DYNAMIC_ON
!
        else if (cmp_no_case(tmpchara, 'dynamic_similarity')            &
     &      .or. cmp_no_case(tmpchara, 'dynamic_simi')) then
          SGS_param%iflag_SGS =     id_SGS_similarity
          SGS_param%iflag_dynamic = id_SGS_DYNAMIC_ON
!
        else if (cmp_no_case(tmpchara,'diffusion')) then
          SGS_param%iflag_SGS =     id_SGS_diffusion
          SGS_param%iflag_dynamic = id_SGS_DYNAMIC_ON
        end if
      end if
!
      i_step_sgs_coefs = 1
      if (sgs_ctl%istep_dynamic_ctl%iflag .gt. 0) then
        i_step_sgs_coefs = sgs_ctl%istep_dynamic_ctl%intvalue
      end if
!
      SGS_param%stab_weight = one
      if (sgs_ctl%stabilize_weight_ctl%iflag .gt. 0) then
        SGS_param%stab_weight = sgs_ctl%stabilize_weight_ctl%realvalue
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'iflag_SGS_model',   SGS_param%iflag_SGS
        write(*,*) 'iflag_dynamic_SGS', SGS_param%iflag_dynamic
        write(*,*) 'i_step_sgs_coefs',  i_step_sgs_coefs
        write(*,*) 'SGS_param%stab_weight',  SGS_param%stab_weight
      end if
!
!
!  set applied terms
!
      SGS_param%iflag_SGS_h_flux =  id_SGS_none
      SGS_param%iflag_SGS_c_flux =  id_SGS_none
      SGS_param%iflag_SGS_m_flux =  id_SGS_none
      SGS_param%iflag_SGS_lorentz = id_SGS_none
      SGS_param%iflag_SGS_uxb =     id_SGS_none
      SGS_param%iflag_SGS_gravity = id_SGS_none
!
      if (SGS_param%iflag_SGS .ne. id_SGS_none) then
        if (sgs_ctl%SGS_terms_ctl%icou .eq. izero) then
            e_message = 'Set equations to apply SGS model'
            call calypso_MPI_abort(ierr_SGS, e_message)
        else
!
          do i = 1, sgs_ctl%SGS_terms_ctl%num
            tmpchara = sgs_ctl%SGS_terms_ctl%c_tbl(i)
            if(     tmpchara .eq. thd_heat_flux) then
              SGS_param%iflag_SGS_h_flux =  SGS_param%iflag_SGS
            else if(tmpchara .eq. thd_advection) then
              SGS_param%iflag_SGS_m_flux =  SGS_param%iflag_SGS
            else if(tmpchara .eq. thd_lorentz) then
              SGS_param%iflag_SGS_lorentz = SGS_param%iflag_SGS
            else if(tmpchara .eq. thd_induction) then
              SGS_param%iflag_SGS_uxb =     SGS_param%iflag_SGS
            else if(tmpchara .eq. thd_gravity) then
              SGS_param%iflag_SGS_gravity = SGS_param%iflag_SGS
            else if(tmpchara .eq. thd_comp_flux) then
              SGS_param%iflag_SGS_c_flux =  SGS_param%iflag_SGS
            end if
          end do
!
          call dealloc_control_array_chara(sgs_ctl%SGS_terms_ctl)
        end if
      end if
!
      if (SGS_param%iflag_SGS .ne. id_SGS_none) then
        if (iflag_debug .gt. 0)  then
          write(*,*) 'iflag_SGS_heat:         ',                        &
     &              SGS_param%iflag_SGS_h_flux
          write(*,*) 'iflag_SGS_comp_flux:    ',                        &
     &              SGS_param%iflag_SGS_c_flux
          write(*,*) 'iflag_SGS_inertia:      ',                        &
     &              SGS_param%iflag_SGS_m_flux
          write(*,*) 'iflag_SGS_lorentz:      ',                        &
     &              SGS_param%iflag_SGS_lorentz
          write(*,*) 'iflag_SGS_induction:    ',                        &
     &              SGS_param%iflag_SGS_uxb
          write(*,*) 'iflag_SGS_gravity:      ',                        &
     &              SGS_param%iflag_SGS_gravity
        end if
      end if
!
!
      SGS_param%clipping_limit                                          &
     &        = set_fixed_Csim(zero, sgs_ctl%clipping_limit_ctl)
!
      SGS_param%SGS_hf_factor                                           &
     &        = set_fixed_Csim(one, sgs_ctl%SGS_hf_factor_ctl)
      SGS_param%SGS_mf_factor                                           &
     &        = set_fixed_Csim(one, sgs_ctl%SGS_mf_factor_ctl)
      SGS_param%SGS_mawell_factor                                       &
     &        = set_fixed_Csim(one, sgs_ctl%SGS_mxwl_factor_ctl)
      SGS_param%SGS_uxb_factor                                          &
     &        = set_fixed_Csim(one, sgs_ctl%SGS_uxb_factor_ctl)
      SGS_param%SGS_cf_factor                                           &
     &        = set_fixed_Csim(one, sgs_ctl%SGS_cf_factor_ctl)
!
      SGS_param%narea_rave_dynamic = 1
      SGS_param%narea_tave_dynamic = 1
      if(sgs_ctl%radial_ave_area_ctl%iflag .gt. 0) then
        SGS_param%narea_rave_dynamic                                    &
     &      = sgs_ctl%radial_ave_area_ctl%intvalue
      end if
      if(sgs_ctl%med_ave_area_ctl%iflag .gt. 0) then
        SGS_param%narea_tave_dynamic                                    &
     &      = sgs_ctl%med_ave_area_ctl%intvalue
      end if
!
      if (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        SGS_param%iflag_rst_sgs_coef_code                               &
     &        = sgs_ctl%ffile_ctl%model_coef_ini_head_ctl%iflag
        if(SGS_param%iflag_rst_sgs_coef_code .gt. 0) then
          Csim_file_IO%file_prefix                                      &
     &        = sgs_ctl%ffile_ctl%model_coef_ini_head_ctl%charavalue
        else
          Csim_file_IO%file_prefix = def_rst_sgs_coef
        end if
!
        call choose_para_file_format                                    &
     &     (sgs_ctl%ffile_ctl%model_coef_rst_format,                    &
     &      Csim_file_IO%iflag_format)
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'Csim_file_IO%file_prefix: ',                      &
     &      trim(Csim_file_IO%file_prefix)
          write(*,*) 'Csim_file_IO%iflag_format: ',                     &
     &      Csim_file_IO%iflag_format
        end if
      end if
!
      SGS_param%iflag_SGS_buo_usage = id_use_volume
      if(sgs_ctl%SGS_buo_Csim_usage_ctl%iflag .gt. 0) then
        tmpchara = sgs_ctl%SGS_buo_Csim_usage_ctl%charavalue
        if(   cmp_no_case(tmpchara, 'zonal')                            &
     &     .or. cmp_no_case(tmpchara, 'phi')) then
          SGS_param%iflag_SGS_buo_usage = id_use_zonal
        end if
        if(   cmp_no_case(tmpchara, 'sphere')) then
          SGS_param%iflag_SGS_buo_usage = id_use_sphere
        end if
        if(   cmp_no_case(tmpchara, 'volume')                           &
     &     .or. cmp_no_case(tmpchara, 'V')) then
          SGS_param%iflag_SGS_buo_usage = id_use_volume
        end if
      end if
!
      end subroutine set_control_SGS_model
!
! -----------------------------------------------------------------------
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
