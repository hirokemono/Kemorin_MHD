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
!!      subroutine dealloc_lic_noise_data(lic_p)
!!      subroutine dealloc_lic_masking_ranges(lic_p)
!!      subroutine dealloc_lic_kernel(lic_p)
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
      use t_control_param_LIC_masking
      use t_noise_node_data
      use t_LIC_kernel_image
      use lic_noise_generator
!
      implicit  none
!
!
      type lic_parameters
!>        Structure of field parameter for LIC
        type(pvr_field_parameter) :: lic_field
!
!>        integer flag to use color
        integer(kind = kint) :: iflag_color_mode =   0
!>        Structure of field color parameter for LIC
        type(pvr_field_parameter) :: color_field
!
!>        integer flag to use opacity
        integer(kind = kint) :: iflag_opacity_mode =   0
!>        Structure for field opacity parameter for LIC
        type(pvr_field_parameter) :: opacity_field
!
!>        Number of masking field
        integer(kind = kint) :: num_masking =   0
!>        Structure of masking parameter
        type(lic_masking_parameter), allocatable :: masking(:)
!
!>        integer flag for LIC kernel function
!>          cflag_from_file: Read noise data file
!>          iflag_randum:    generate fram randum number
        integer(kind = kint) :: iflag_noise_type = 0
!>        file name of kernel function data
        character(len = kchara) :: noise_file_name
!>        normalization factor for LIC value
        real(kind = kreal) :: freq_noise = one
!>        1-D grid size of LIC noise, resolution = size * frequency
        integer(kind = kint) :: noise_resolution
!>        input noise texture size
        integer(kind = kint) :: noise_dim(3)
        integer(kind = kint) :: noise_size
!>        noise texture, 1 BYTE for each noise value
        character, allocatable:: noise_data(:)
!>        Precomputed noise gradient for lighting
        character, allocatable:: noise_grad_data(:)
!
!>        integer flag for LIC kernel function
!>          iflag_linear:    Use symmetric linear kernel
!>          iflag_from_file: Use kernel image file
        integer(kind = kint) :: iflag_kernel_type = 0
!>        file name of kernel function data
        character(len = kchara) :: kernel_image_prefix
!>        Structure of LIC kernel image
        type(LIC_kernel_image) :: kernel_image
!
!>        mode of unstructured volume rendering for lic
!>        sample by fixed step size or sample each cell's center once
        integer(kind = kint) :: iflag_vr_sample_mode = izero
        real(kind = kreal) :: step_size = 0.005
!
!>        Element counts of LIC field line tracing
        integer(kind = kint) :: iflag_trace_length_type = izero
!>        Lengh of LIC field line tracing
        real(kind = kreal) :: trace_length = one
!>        Element counts of LIC field line tracing
        integer(kind = kint) :: trace_count =  ieight
!
!>        integer flag for LIC normalization mode
!>          iflag_from_control: Set from control file
!>          iflag_from_lic:     Use LIC value range
        integer(kind = kint) :: iflag_normalization = 0
!>        normalization factor for LIC value
        real(kind = kreal) :: factor_normal = one
!
!>        integer flag for reflection reference
!>          iflag_from_file:   Use noise gradient file
!>          iflag_from_color:  Use gradient of color data
        integer(kind = kint) :: iflag_reflection_ref = 0
!>        file name of reflection file
        character(len = kchara) :: reflection_file_name
!>        reflection parameter
        real(kind = kreal) :: reflection_parameter
      end type lic_parameters
!
      character(len = kchara), parameter :: cflag_LIC = 'LIC'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_from_file = 'file'
      character(len = kchara), parameter                                &
     &                        :: cflag_randum = 'randum'
      character(len = kchara), parameter                                &
     &                        :: cflag_linear = 'linear'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_fixed_size =   'fixed_size'
      character(len = kchara), parameter                                &
     &                        :: cflag_by_element = 'ele_size'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_by_lengh =   'length'
      character(len = kchara), parameter                                &
     &                        :: cflag_by_e_count = 'element_count'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_by_range = 'set_by_range'
      character(len = kchara), parameter                                &
     &                        :: cflag_by_ctl =   'set_by_control'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_noise_file = 'noise_file'
      character(len = kchara), parameter                                &
     &                        :: cflag_from_color = 'color_field'
!
      integer(kind = kint), parameter :: iflag_from_file =    0
      integer(kind = kint), parameter :: iflag_randum =       1
      integer(kind = kint), parameter :: iflag_linear =       1
!
      integer(kind = kint), parameter :: iflag_from_lic =     0
      integer(kind = kint), parameter :: iflag_from_control = 1
!
      integer(kind = kint), parameter :: iflag_fixed_size =   0
      integer(kind = kint), parameter :: iflag_by_element = 1
!
      integer(kind = kint), parameter :: iflag_by_lengh =   0
      integer(kind = kint), parameter :: iflag_by_e_count = 1
!
      integer(kind = kint), parameter :: iflag_from_color =   2
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
      use set_parallel_file_name
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
      integer(kind = kint) :: i
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
      lic_p%num_masking = lic_ctl%num_masking_ctl
      if(lic_p%num_masking .gt. 0) then
        allocate(lic_p%masking(lic_p%num_masking))
!
        do i = 1, lic_p%num_masking
          call set_control_lic_masking(num_nod_phys, phys_nod_name,     &
     &        lic_ctl%mask_ctl(i), lic_p%masking(i))
        end do
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'LIC field: ', lic_p%lic_field%id_field,             &
     &            trim(lic_p%lic_field%field_name)
        write(*,*) 'LIC color: ', lic_p%color_field%id_field,           &
     &            lic_p%color_field%id_component,                       &
     &            trim(lic_p%color_field%field_name)
        write(*,*) 'LIC opacity: ', lic_p%opacity_field%id_field,       &
     &            lic_p%opacity_field%id_component,                     &
     &            trim(lic_p%opacity_field%field_name)
!
        write(*,*) 'num_masking: ', lic_p%num_masking
        do i = 1, lic_p%num_masking
          write(*,*) 'LIC masking field: ',  i,                         &
     &              lic_p%masking(i)%field_info%id_field,               &
     &              lic_p%masking(i)%field_info%id_component,           &
     &              trim(lic_p%masking(i)%field_info%field_name)
          write(*,*) 'LIC masking range min: ',                         &
     &              lic_p%masking(i)%range_min(:)
          write(*,*) 'LIC masking range max: ',                         &
     &              lic_p%masking(i)%range_max(:)
        end do
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
        if(lic_ctl%noise_file_prefix_ctl%iflag .gt. 0) then
          lic_p%noise_file_name                                         &
     &       = lic_ctl%noise_file_prefix_ctl%charavalue
        else
          e_message = 'Set LIC noise file name'
          call calypso_mpi_abort(ierr_VIZ, e_message)
        end if
      end if
!
      call add_grd_extension                                            &
     &   (lic_p%noise_file_name, lic_p%reflection_file_name)
!
!
      lic_p%freq_noise = one
      if(lic_ctl%noise_resolution_ctl%iflag .gt. 0) then
        lic_p%noise_resolution = lic_ctl%noise_resolution_ctl%intvalue
      end if
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
        if(lic_ctl%kernal_file_prefix_ctl%iflag .gt. 0) then
          lic_p%kernel_image_prefix                                     &
     &       = lic_ctl%kernal_file_prefix_ctl%charavalue
        else
          e_message = 'Set LIC kernel file prefix'
          call calypso_mpi_abort(ierr_VIZ, e_message)
        end if
      end if
!
      lic_p%iflag_vr_sample_mode = iflag_fixed_size
      if(lic_ctl%vr_sample_mode_ctl%iflag .gt. 0) then
        tmpchara = lic_ctl%vr_sample_mode_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_fixed_size)) then
          lic_p%iflag_vr_sample_mode = iflag_fixed_size
        else if(cmp_no_case(tmpchara, cflag_by_element)) then
          lic_p%iflag_vr_sample_mode = iflag_by_element
        end if
      end if
!
      if(lic_p%iflag_vr_sample_mode .eq. iflag_fixed_size) then
        lic_p%step_size = 0.01
        if(lic_ctl%step_size_ctl%iflag .gt. 0) then
          lic_p%step_size = lic_ctl%step_size_ctl%realvalue
        end if
      end if
!
      lic_p%iflag_trace_length_type = iflag_by_lengh
      if(lic_ctl%LIC_trace_length_def_ctl%iflag .gt. 0) then
        tmpchara = lic_ctl%LIC_trace_length_def_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_by_lengh)) then
          lic_p%iflag_trace_length_type = iflag_by_lengh
        else if(cmp_no_case(tmpchara, cflag_by_e_count)) then
          lic_p%iflag_trace_length_type = iflag_by_e_count
        end if
      end if
!
      if(lic_p%iflag_trace_length_type .eq. iflag_by_lengh) then
        lic_p%trace_length = one
        if(lic_ctl%LIC_trace_length_ctl%iflag .gt. 0) then
          lic_p%trace_length = lic_ctl%LIC_trace_length_ctl%realvalue
        end if
      else if(lic_p%iflag_trace_length_type .eq. iflag_by_e_count) then
        lic_p%trace_count =  ieight
        if(lic_ctl%LIC_trace_count_ctl%iflag .gt. 0) then
          lic_p%trace_count = lic_ctl%LIC_trace_count_ctl%intvalue
        end if
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
      lic_p%iflag_reflection_ref = iflag_from_color
      if(lic_ctl%reflection_ref_type_ctl%iflag .gt. 0) then
        tmpchara = lic_ctl%reflection_ref_type_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_noise_file)) then
          lic_p%iflag_reflection_ref = iflag_from_file
        else if(cmp_no_case(tmpchara, cflag_from_color)) then
          lic_p%iflag_reflection_ref = iflag_from_color
        end if
      end if
!
      lic_p%reflection_parameter = one
      if(lic_ctl%reflection_parameter_ctl%iflag .gt. 0) then
          lic_p%reflection_parameter                                    &
     &          = lic_ctl%reflection_parameter_ctl%realvalue
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'iflag_noise_type: ', lic_p%iflag_noise_type
        write(*,*) 'noise_file_name: ', trim(lic_p%noise_file_name)
        write(*,*) 'reflection_file_name: ',                            &
     &            trim(lic_p%reflection_file_name)
        write(*,*) 'noise_resolution: ', lic_p%noise_resolution
        write(*,*) 'freq_noise: ', lic_p%freq_noise
!
        write(*,*) 'iflag_kernel_type: ', lic_p%iflag_kernel_type
        if(lic_p%iflag_kernel_type .eq. iflag_from_file) then
          write(*,*) 'kernel_image_prefix: ', lic_p%kernel_image_prefix
        end if
!
        write(*,*) 'iflag_vr_sample_mode: ', lic_p%iflag_vr_sample_mode
        write(*,*) 'step_size: ', lic_p%step_size
!
        write(*,*) 'iflag_trace_length_type: ',                         &
     &             lic_p%iflag_trace_length_type
        write(*,*) 'trace_length: ', lic_p%trace_length
        write(*,*) 'trace_count: ',  lic_p%trace_count
!
        write(*,*) 'iflag_normalization: ', lic_p%iflag_normalization
        write(*,*) 'factor_normal: ',       lic_p%factor_normal
!
        write(*,*) 'iflag_reflection_ref: ', lic_p%iflag_reflection_ref
        write(*,*) 'reflection_parameter: ', lic_p%reflection_parameter
      end if
!
      end subroutine set_control_lic_parameter
!
!  ---------------------------------------------------------------------
!
!     if true, the reference value is in the mask range, so it can be visualized
      logical function mask_flag(lic_p, value)
!
      type(lic_parameters), intent(in) :: lic_p
      real(kind=kreal), intent(in) :: value(:)
!
      integer(kind=kint) :: i,j, iFlag_inmask
!
      mask_flag = .true.
      do i = 1, lic_p%num_masking
        iFlag_inmask = izero
        do j = 1, lic_p%masking(i)%num_range
          if((value(i) .ge. lic_p%masking(i)%range_min(j)) .and.        &
          &   (value(i) .le. lic_p%masking(i)%range_max(j))) then
            iFlag_inmask = 1
            exit
          end if
        end do
        if(iFlag_inmask .eq. izero) then
          mask_flag = .false.
          return
        end if
      end do

      end function mask_flag
!
!-----------------------------------------------------------------------
!
      subroutine load_noise_data(lic_p)
!
      use t_control_data_LIC
!
      integer(kind = kint) :: read_err
      type(lic_parameters), intent(inout) :: lic_p
!
      if(my_rank .eq. 0) write(*,*) 'loading noise texture from: ',     &
     &                             trim(lic_p%noise_file_name)
      call import_noise_ary(lic_p%noise_file_name,                      &
     &    lic_p%noise_data, lic_p%noise_dim, read_err)
      if(read_err .eq. 0) then
        if(iflag_debug .gt. 0) then
          write(*,*) 'loading noise successfuly, ',                     &
     &         'loading gradient from: ', lic_p%reflection_file_name
        end if
        call import_noise_grad_ary(lic_p%reflection_file_name,          &
     &      lic_p%noise_grad_data, lic_p%noise_dim, read_err)
      end if
!
      lic_p%noise_size = lic_p%noise_dim(1) * lic_p%noise_dim(2)        &
     &                  * lic_p%noise_dim(3)
      lic_p%freq_noise = lic_p%noise_resolution / lic_p%noise_dim(1)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'load noise texture from: ', lic_p%noise_file_name
        write(*,*) 'noise size: ', lic_p%noise_size
      end if
      end subroutine load_noise_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_noise_data(lic_p)
!
      type(lic_parameters), intent(inout) :: lic_p
!
      if(lic_p%noise_size .gt. 0) then
        deallocate(lic_p%noise_data)
        deallocate(lic_p%noise_grad_data)
      end if
      end subroutine dealloc_lic_noise_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_masking_ranges(lic_p)
!
      type(lic_parameters), intent(inout) :: lic_p
!
      integer(kind = kint) :: i
!
!
        do i = 1, lic_p%num_masking
          call dealloc_lic_masking_range(lic_p%masking(i))
        end do
        deallocate(lic_p%masking)
!
      end subroutine dealloc_lic_masking_ranges
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_kernel(lic_p)
!
      type(lic_parameters), intent(inout) :: lic_p
!
      call dealloc_lic_kernel_image(lic_p%kernel_image)
!
      end subroutine dealloc_lic_kernel
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_LIC
