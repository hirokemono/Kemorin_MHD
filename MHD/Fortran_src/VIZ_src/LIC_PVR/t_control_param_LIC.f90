!>@file   t_control_param_LIC.f90
!!@brief  module t_control_param_LIC
!!
!!@author H. Matsui
!!@date Programmed in Apr. 2018
!
!> @brief control parameters for parallel LIC
!!
!!@verbatim
!!      subroutine set_control_lic_parameter                            &
!!     &         (num_nod_phys, phys_nod_name, lic_ctl, lic_p)
!!        type(lic_parameter_ctl), intent(in) :: lic_ctl
!!        type(lic_parameter_ctl), intent(inout) :: lic_p
!!@endverbatim
!
      module t_control_param_LIC
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_error_IDs
      use skip_comment_f
      use t_control_params_4_pvr
!
      implicit  none
!
!
      type lic_parameters
!>        Structure for field parameter for LIC
        type(pvr_field_parameter) :: lic_field
!
!>        integer flag to use color
        integer(kind = kint) :: iflag_color_mode =   0
!>        Structure for field color parameter for LIC
        type(pvr_field_parameter) :: color_field
!
!>        integer flag to use opacity
        integer(kind = kint) :: iflag_opacity_mode =   0
!>        Structure for field opacity parameter for LIC
        type(pvr_field_parameter) :: opacity_field
!
!>        integer flag to use source decision
        integer(kind = kint) :: iflag_source_decision =   0
!>        Structure for soure decision field parameter for LIC
        type(pvr_field_parameter) :: soure_field
!>        minimum value of source point range
        real(kind = kreal) :: sorce_min = -1.0e15
!>        maximum value of source point range
        real(kind = kreal) :: sorce_max =  1.0e15
!
!>        integer flag for LIC kernel function
        integer(kind = kint) :: iflag_noise_type = 0
!>        file name of kernel function data
        character(len = kchara) :: noise_file_name
!>        normalization factor for LIC value
        real(kind = kreal) :: freq_noise = one
!
!>        integer flag for LIC kernel function
        integer(kind = kint) :: iflag_kernel_type = 0
!>        file name of kernel function data
        character(len = kchara) :: kernel_file_name
!>        normalization factor for LIC value
        real(kind = kreal) :: trace_length = one
!
!>        integer flag for LIC normalization mode
        integer(kind = kint) :: iflag_normalization = 0
!>        normalization factor for LIC value
        real(kind = kreal) :: factor_normal = one
      end type lic_parameters
!
      character(len = kchara), parameter                                &
     &                        :: cflag_from_file = 'file'
      character(len = kchara), parameter                                &
     &                        :: cflag_randum = 'randum'
      character(len = kchara), parameter                                &
     &                        :: cflag_linear = 'linear'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_by_range = 'set_by_range'
      character(len = kchara), parameter                                &
     &                        :: cflag_by_ctl =   'set_by_control'
!
      integer(kind = kint), parameter :: iflag_from_file =    0
      integer(kind = kint), parameter :: iflag_randum =       1
      integer(kind = kint), parameter :: iflag_linear =       1
!
      integer(kind = kint), parameter :: iflag_from_lic =     0
      integer(kind = kint), parameter :: iflag_from_control = 1
!
!
      private :: cflag_from_file, cflag_randum, cflag_linear
      private :: cflag_by_range, cflag_by_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_lic_parameter                              &
     &         (num_nod_phys, phys_nod_name, lic_ctl, lic_p)
!
      use t_control_data_LIC
      use set_field_comp_for_viz
      use set_components_flags
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(lic_parameter_ctl), intent(in) :: lic_ctl
      type(lic_parameters), intent(inout) :: lic_p
!
      integer(kind = kint) :: icheck_ncomp(1)
      integer(kind = kint) :: ifld_tmp(1), icomp_tmp(1), ncomp_tmp(1)
      character(len = kchara) :: fldname_tmp(1)
      character(len = kchara) :: tmpfield(1), tmpcomp(1)
      character(len = kchara) :: tmpchara
!
!
      lic_p%iflag_color_mode = lic_ctl%LIC_field_ctl%iflag
      if(lic_ctl%LIC_field_ctl%iflag .ne. 0) then
        tmpfield(1) = lic_ctl%LIC_field_ctl%charavalue
        tmpcomp(1) =  'vector'
        call set_components_4_viz                                       &
     &     (num_nod_phys, phys_nod_name, ione, tmpfield, tmpcomp, ione, &
     &      ifld_tmp, icomp_tmp, icheck_ncomp, ncomp_tmp, fldname_tmp)
        lic_p%lic_field%id_field =          ifld_tmp(1)
        lic_p%lic_field%id_component =      icomp_tmp(1)
        lic_p%lic_field%num_original_comp = ncomp_tmp(1)
        lic_p%lic_field%field_name =        fldname_tmp(1)
!
        if(icomp_tmp(1) .ne. icomp_VECTOR) then
          e_message = 'Set vector for LIC field'
          call calypso_mpi_abort(ierr_fld, e_message)
        end if
      else
        e_message = 'Set LIC field'
        call calypso_mpi_abort(ierr_fld, e_message)
      end if
!
!
      lic_p%iflag_color_mode = lic_ctl%color_field_ctl%iflag
      if(lic_p%iflag_color_mode .ne. id_turn_OFF) then
        if(lic_ctl%color_component_ctl%iflag .eq. 0) then
          e_message = 'Set component for LIC color component'
          call calypso_mpi_abort(ierr_fld, e_message)
        end if
!
        tmpfield(1) = lic_ctl%color_field_ctl%charavalue
        tmpcomp(1) =  lic_ctl%color_component_ctl%charavalue
        call set_components_4_viz                                       &
     &     (num_nod_phys, phys_nod_name, ione, tmpfield, tmpcomp, ione, &
     &      ifld_tmp, icomp_tmp, icheck_ncomp, ncomp_tmp, fldname_tmp)
        lic_p%color_field%id_field =          ifld_tmp(1)
        lic_p%color_field%id_component =      icomp_tmp(1)
        lic_p%color_field%num_original_comp = ncomp_tmp(1)
        lic_p%color_field%field_name =        fldname_tmp(1)
      end if
!
!
      lic_p%iflag_opacity_mode = lic_ctl%opacity_field_ctl%iflag
      if(lic_p%iflag_opacity_mode .ne. id_turn_OFF) then
        if(lic_ctl%opacity_component_ctl%iflag .eq. 0) then
          e_message = 'Set component for LIC opacity component'
          call calypso_mpi_abort(ierr_fld, e_message)
        end if
!
        tmpfield(1) = lic_ctl%opacity_field_ctl%charavalue
        tmpcomp(1) =  lic_ctl%opacity_component_ctl%charavalue
        call set_components_4_viz                                       &
     &     (num_nod_phys, phys_nod_name, ione, tmpfield, tmpcomp, ione, &
     &      ifld_tmp, icomp_tmp, icheck_ncomp, ncomp_tmp, fldname_tmp)
        lic_p%opacity_field%id_field =          ifld_tmp(1)
        lic_p%opacity_field%id_component =      icomp_tmp(1)
        lic_p%opacity_field%num_original_comp = ncomp_tmp(1)
        lic_p%opacity_field%field_name =        fldname_tmp(1)
      end if
!
!
      lic_p%iflag_source_decision = lic_ctl%source_ref_field_ctl%iflag
      if(lic_p%iflag_source_decision .ne. id_turn_OFF) then
        if(lic_ctl%source_ref_component_ctl%iflag .eq. 0) then
          e_message = 'Set component for LIC source decision'
          call calypso_mpi_abort(ierr_fld, e_message)
        end if
!
        tmpfield(1) = lic_ctl%source_ref_field_ctl%charavalue
        tmpcomp(1) =  lic_ctl%source_ref_component_ctl%charavalue
        call set_components_4_viz                                       &
     &     (num_nod_phys, phys_nod_name, ione, tmpfield, tmpcomp, ione, &
     &      ifld_tmp, icomp_tmp, icheck_ncomp, ncomp_tmp, fldname_tmp)
        lic_p%soure_field%id_field =          ifld_tmp(1)
        lic_p%soure_field%id_component =      icomp_tmp(1)
        lic_p%soure_field%num_original_comp = ncomp_tmp(1)
        lic_p%soure_field%field_name =        fldname_tmp(1)
!
        lic_p%sorce_min = -1.0e15
        if(lic_ctl%source_minimum_ctl%iflag .gt. 0) then
          lic_p%sorce_min = lic_ctl%source_minimum_ctl%realvalue
        end if
!
        lic_p%sorce_max = 1.0e15
        if(lic_ctl%source_maximum_ctl%iflag .gt. 0) then
          lic_p%sorce_max = lic_ctl%source_maximum_ctl%realvalue
        end if
      end if
!
!
      lic_p%iflag_noise_type = iflag_from_file
      if(lic_ctl%noise_type_ctl%iflag .gt. 0) then
        tmpchara = lic_ctl%noise_type_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_from_file)) then
          lic_p%iflag_noise_type = iflag_from_file
        else if(cmp_no_case(tmpchara, cflag_randum)) then
          lic_p%iflag_noise_type = iflag_randum
        end if
      end if
!
      if(lic_p%iflag_noise_type .eq. iflag_from_file) then
        if(lic_ctl%noise_file_name_ctl%iflag .gt. 0) then
          lic_p%noise_file_name                                         &
     &       = lic_ctl%noise_file_name_ctl%charavalue
        else
          e_message = 'Set LIC noise file name'
          call calypso_mpi_abort(ierr_VIZ, e_message)
        end if
      end if
!
      lic_p%freq_noise = one
      if(lic_ctl%noise_frequency_ctl%iflag .gt. 0) then
        lic_p%freq_noise = lic_ctl%noise_frequency_ctl%realvalue
      end if
!
!
      lic_p%iflag_kernel_type = iflag_linear
      if(lic_ctl%kernel_function_type_ctl%iflag .gt. 0) then
        tmpchara = lic_ctl%kernel_function_type_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_from_file)) then
          lic_p%iflag_kernel_type = iflag_from_file
        else if(cmp_no_case(tmpchara, cflag_linear)) then
          lic_p%iflag_kernel_type = iflag_linear
        end if
      end if
!
      if(lic_p%iflag_kernel_type .eq. iflag_from_file) then
        if(lic_ctl%kernal_file_name_ctl%iflag .gt. 0) then
          lic_p%kernel_file_name                                        &
     &       = lic_ctl%kernal_file_name_ctl%charavalue
        else
          e_message = 'Set LIC kernel file name'
          call calypso_mpi_abort(ierr_VIZ, e_message)
        end if
      end if
!
      lic_p%trace_length = one
      if(lic_ctl%LIC_trace_length_ctl%iflag .gt. 0) then
        lic_p%trace_length = lic_ctl%LIC_trace_length_ctl%realvalue
      end if
!
!
      lic_p%iflag_normalization = iflag_from_control
      if(lic_ctl%normalization_type_ctl%iflag .gt. 0) then
        tmpchara = lic_ctl%normalization_type_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_by_range)) then
          lic_p%iflag_normalization = iflag_from_lic
        else if(cmp_no_case(tmpchara, cflag_by_ctl)) then
          lic_p%iflag_normalization = iflag_from_control
        end if
      end if
!
      lic_p%factor_normal = one
      if(lic_ctl%normalization_value_ctl%iflag .gt. 0) then
        lic_p%factor_normal = lic_ctl%normalization_value_ctl%realvalue
      end if
!
      end subroutine set_control_lic_parameter
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_LIC
