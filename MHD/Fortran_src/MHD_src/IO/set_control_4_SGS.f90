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
!!     &         (sgs_ctl, SGS_param, cmt_param, filter_param,          &
!!     &          Csim_file_IO, Cdiff_file_IO, i_step_sgs_coefs)
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!        type(SGS_model_control_params), intent(inout) :: SGS_param
!!        type(commutation_control_params), intent(inout) :: cmt_param
!!        type(field_IO_params), intent(inout) :: Csim_file_IO
!!        type(field_IO_params), intent(inout) :: Cdiff_file_IO
!!        type(SGS_filtering_params), intent(inout) :: filter_param
!!      subroutine set_control_SPH_SGS                                  &
!!     &         (num_sph_filter_ctl, sph_filter_ctl, sph_filters)
!!        type(sph_filter_ctl_type), intent(in) :: sph_filter_ctl(1)
!!        type(sph_filters_type), intent(inout) :: sph_filters(1)
!!@endverbatim
!
      module set_control_4_SGS
!
      use m_precision
      use m_error_IDs
      use m_constants
      use m_machine_parameter
      use calypso_mpi
      use t_file_IO_parameter
!
      implicit  none
!
      character(len=kchara), parameter                                  &
     &                      :: def_rst_sgs_coef =  'rst_model_coefs'
      character(len=kchara), parameter                                  &
     &                      :: def_rst_comm_coef = 'rst_diff_coefs'
      private :: def_rst_sgs_coef, def_rst_comm_coef
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
      use t_ctl_data_SGS_filter
      use t_sph_filtering_data
!
      integer(kind = kint), intent(in) :: num_sph_filter_ctl
      type(sph_filter_ctl_type), intent(in) :: sph_filter_ctl(1)
      type(sph_filters_type), intent(inout) :: sph_filters(1)
!
!
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
      subroutine set_control_SGS_model                                  &
     &         (sgs_ctl, SGS_param, cmt_param, filter_param,            &
     &          Csim_file_IO, Cdiff_file_IO, i_step_sgs_coefs)
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
      type(commutation_control_params), intent(inout) :: cmt_param
      type(SGS_filtering_params), intent(inout) :: filter_param
      type(field_IO_params), intent(inout) :: Csim_file_IO
      type(field_IO_params), intent(inout) :: Cdiff_file_IO
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
      write(*,*) 'sgs_ctl%stabilize_weight_ctl%iflag ',   &
     &          sgs_ctl%stabilize_weight_ctl%iflag 
      write(*,*) 'sgs_ctl%stabilize_weight_ctl%realvalue ',   &
     &          sgs_ctl%stabilize_weight_ctl%realvalue 
      SGS_param%stab_weight = one
      if (sgs_ctl%stabilize_weight_ctl%iflag .gt. 0) then
        SGS_param%stab_weight = sgs_ctl%stabilize_weight_ctl%realvalue
      end if
      write(*,*) 'SGS_param%stab_weight',  SGS_param%stab_weight
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
      call set_control_SGS_commute                                      &
     &   (SGS_param, sgs_ctl, cmt_param, Cdiff_file_IO)
!
      end subroutine set_control_SGS_model
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SGS_commute                                &
     &         (SGS_param, sgs_ctl, cmt_param, Cdiff_file_IO)
!
      use m_phys_labels
      use m_file_format_switch
      use t_ctl_data_SGS_model
      use t_SGS_control_parameter
      use t_field_data_IO
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(commutation_control_params), intent(inout) :: cmt_param
      type(field_IO_params), intent(inout) :: Cdiff_file_IO
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
      if (SGS_param%iflag_SGS .eq. id_SGS_none) return
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
      if (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
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
!
      if(cmt_param%iflag_commute .ne. id_SGS_commute_OFF) then
        cmt_param%iflag_rst_sgs_comm_code                               &
     &        = sgs_ctl%ffile_ctl%commute_coef_ini_head_ctl%iflag
        if(cmt_param%iflag_rst_sgs_comm_code .gt. 0) then
          Cdiff_file_IO%file_prefix                                     &
     &        = sgs_ctl%ffile_ctl%commute_coef_ini_head_ctl%charavalue
        else
          Cdiff_file_IO%file_prefix = def_rst_comm_coef
        end if
!
        call choose_para_file_format                                    &
     &     (sgs_ctl%ffile_ctl%commute_coef_rst_format,                  &
     &      Cdiff_file_IO%iflag_format)
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'Cdiff_file_IO%file_prefix: ',                     &
     &              trim(Cdiff_file_IO%file_prefix)
          write(*,*) 'Cdiff_file_IO%iflag_format: ',                    &
     &              Cdiff_file_IO%iflag_format
        end if
      end if
!
      end subroutine set_control_SGS_commute
!
! -----------------------------------------------------------------------
!
      end module set_control_4_SGS
