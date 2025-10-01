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
!!     &         (num_nod_phys, phys_nod_name, lic_ctl,                 &
!!     &          lic_p, flag_each_repart)
!!        type(lic_parameter_ctl), intent(in) :: lic_ctl
!!        type(lic_parameter_ctl), intent(inout) :: lic_p
!!        type(lic_repart_reference), intent(inout) :: rep_ref
!!      logical function lic_mask_flag(lic_p, value)
!!        type(lic_parameters), intent(in) :: lic_p
!!        real(kind=kreal), intent(in) :: value(:)
!!      subroutine dealloc_lic_noise_data(lic_p)
!!      subroutine dealloc_lic_masking_ranges(lic_p)
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
      use t_ctl_param_masking
      use t_noise_node_data
      use t_3d_noise
      use t_LIC_kernel
      use t_control_param_vol_grping
!
      implicit  none
!
!
      type lic_parameters
!>        Structure of field parameter for LIC
        type(pvr_field_parameter) :: lic_field
!
!>        integer flag to use color
!>             iflag_from_lic (0):     Use LIC value
!>             iflag_from_control (1): Use field defined by control
        integer(kind = kint) :: iflag_color_mode =   0
!>        Structure of field color parameter for LIC
        type(pvr_field_parameter) :: color_field
!
!>        Number of masking field
        integer(kind = kint) :: num_masking =   0
!>        Structure of masking parameter
        type(masking_parameter), allocatable :: masking(:)
!
!>        Structure for repartitioning parameters
        type(volume_partioning_param) :: each_part_p
!
!>        Structure of noise on cube
        type(noise_cube) :: noise_t
!>        Structure of LIC kernel
        type(LIC_kernel) :: kernel_t
!
!
!>        mode of unstructured volume rendering for lic
!>        sample by fixed step size or sample each cell's center once
        integer(kind = kint) :: iflag_vr_sample_mode = izero
        real(kind = kreal) :: step_size = 0.005
!
!>        integer flag for LIC normalization mode
!>          iflag_from_control: Set from control file
!>          iflag_from_lic:     Use LIC value range
        integer(kind = kint) :: iflag_normalization = 0
!>        normalization factor for LIC value
        real(kind = kreal) :: factor_normal = one
!
!>        Logical flag to output detailed elapsed time for LIC
logical :: flag_LIC_elapsed_dump = .TRUE.
      end type lic_parameters
!
      character(len = kchara), parameter :: cflag_LIC = 'LIC'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_fixed_size =   'fixed_size'
      character(len = kchara), parameter                                &
     &                        :: cflag_by_element = 'ele_size'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_by_range = 'set_by_range'
      character(len = kchara), parameter                                &
     &                        :: cflag_by_ctl =   'set_by_control'
!
      integer(kind = kint), parameter :: iflag_from_lic =     0
      integer(kind = kint), parameter :: iflag_from_control = 1
!
      integer(kind = kint), parameter :: iflag_fixed_size = 0
      integer(kind = kint), parameter :: iflag_by_element = 1
!
      integer(kind = kint), parameter :: iflag_from_color =   2
!
      private :: cflag_by_range, cflag_by_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_lic_parameter                              &
     &         (num_nod_phys, phys_nod_name, lic_ctl,                   &
     &          lic_p, rep_ref, flag_each_repart)
!
      use t_control_data_LIC
      use t_lic_repart_reference
      use set_field_comp_for_viz
      use set_components_flags
      use set_parallel_file_name
      use set_ucd_extensions
      use set_control_LIC_masking
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(lic_parameter_ctl), intent(in) :: lic_ctl
      type(lic_parameters), intent(inout) :: lic_p
      type(lic_repart_reference), intent(inout) :: rep_ref
      logical, intent(inout) :: flag_each_repart
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
!      lic_p%flag_LIC_elapsed_dump = .FALSE.
      if(lic_ctl%subdomain_elapsed_dump_ctl%iflag .gt. 0) then
        tmpchara = lic_ctl%subdomain_elapsed_dump_ctl%charavalue
        if(yes_flag(tmpchara)) lic_p%flag_LIC_elapsed_dump = .TRUE.
      end if
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
      else
        lic_p%color_field%id_field =      1
        lic_p%color_field%id_component =  1
      end if
!
      call set_ctl_param_vol_repart(lic_ctl%repart_ctl,                 &
     &                              lic_p%each_part_p)
      call set_lic_repart_reference_param                               &
     &   (lic_ctl%repart_ctl%new_part_ctl, lic_p%each_part_p, rep_ref)
      if(lic_p%each_part_p%flag_repartition) flag_each_repart = .TRUE.
!
      lic_p%num_masking = lic_ctl%mul_mask_c%num_masking_ctl
      if(lic_p%num_masking .gt. 0) then
        allocate(lic_p%masking(lic_p%num_masking))
!
        do i = 1, lic_p%num_masking
          call s_set_control_LIC_masking(num_nod_phys, phys_nod_name,   &
     &        lic_ctl%mul_mask_c%mask_ctl(i), lic_p%masking(i))
        end do
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'LIC field: ', lic_p%lic_field%id_field,             &
     &            trim(lic_p%lic_field%field_name)
        write(*,*) 'LIC color: ', lic_p%color_field%id_field,           &
     &            lic_p%color_field%id_component,                       &
     &            trim(lic_p%color_field%field_name)
!
        write(*,*) 'num_masking: ', lic_p%num_masking
        do i = 1, lic_p%num_masking
          write(*,*) 'LIC masking field: ',  i,                         &
     &              lic_p%masking(i)%id_mask_field,                     &
     &              lic_p%masking(i)%id_mask_comp,                      &
     &   trim(lic_ctl%mul_mask_c%mask_ctl(i)%field_name_ctl%charavalue)
          write(*,*) 'LIC masking range min: ',                         &
     &              lic_p%masking(i)%range_min(:)
          write(*,*) 'LIC masking range max: ',                         &
     &              lic_p%masking(i)%range_max(:)
        end do
      end if
!
      if(my_rank .eq. 0) then
        call set_control_3d_cube_noise(lic_ctl%noise_ctl,               &
     &                                 lic_p%noise_t)
      end if
!
      if(my_rank .eq. 0) then
        call set_control_LIC_kernel(lic_ctl%kernel_ctl, lic_p%kernel_t)
        call sel_const_LIC_kernel(lic_p%kernel_t)
      end if
      call bcast_LIC_kernel(lic_p%kernel_t)
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
      if(iflag_debug .gt. 0) then
        write(*,*) 'iflag_vr_sample_mode: ', lic_p%iflag_vr_sample_mode
        write(*,*) 'step_size: ', lic_p%step_size
!
        write(*,*) 'iflag_normalization: ', lic_p%iflag_normalization
        write(*,*) 'factor_normal: ',       lic_p%factor_normal
      end if
!
      end subroutine set_control_lic_parameter
!
!  ---------------------------------------------------------------------
!
!     if true, the reference value is in the mask range, so it can be visualized
      logical function lic_mask_flag(lic_p, value)
!
      type(lic_parameters), intent(in) :: lic_p
      real(kind=kreal), intent(in) :: value(lic_p%num_masking)
!
!
      lic_mask_flag                                                     &
     &    = multi_mask_flag(lic_p%num_masking, lic_p%masking, value)

      end function lic_mask_flag
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_lic_masking_ranges(lic_p)
!
      type(lic_parameters), intent(inout) :: lic_p
!
      integer(kind = kint) :: i
!
!
      do i = 1, lic_p%num_masking
        call dealloc_masking_range(lic_p%masking(i))
      end do
      deallocate(lic_p%masking)
!
      end subroutine dealloc_lic_masking_ranges
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_LIC
