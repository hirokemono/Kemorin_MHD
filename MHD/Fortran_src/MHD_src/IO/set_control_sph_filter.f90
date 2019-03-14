!>@file   set_control_sph_filter.f90
!!@brief  module set_control_sph_filter
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
!!      subroutine set_control_SPH_SGS_filters                          &
!!     &         (sgs_ctl, SGS_param, dynamic_SPH)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(SGS_model_control), intent(in) :: sgs_ctl
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!@endverbatim
!
      module set_control_sph_filter
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
      private :: set_control_SPH_SGS_1filter
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SPH_SGS_filters                            &
     &         (sgs_ctl, SGS_param, dynamic_SPH)
!
      use m_error_IDs
      use t_SGS_control_parameter
      use t_ctl_data_SGS_MHD_model
      use t_sph_filtering
      use skip_comment_f
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_model_control), intent(in) :: sgs_ctl
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
      integer(kind = kint) :: i
!
!
      if(sgs_ctl%num_sph_filter_ctl .le. 0) then
        call calypso_mpi_abort(ierr_SGS, 'Set filter configrations')
      else if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON            &
     &    .and. sgs_ctl%num_sph_filter_ctl .eq. 1) then
        call calypso_mpi_abort                                          &
     &     (ierr_SGS, 'Set more than two filter configrations')
      else
        call alloc_sph_filter_type                                      &
     &     (sgs_ctl%num_sph_filter_ctl, dynamic_SPH)
      end if
!
      do i = 1, dynamic_SPH%num_sph_filteres
        call set_control_SPH_SGS_1filter                                &
     &     (sgs_ctl%sph_filter_ctl(i), dynamic_SPH%sph_filters(i))
!
        if(     dynamic_SPH%sph_filters(i)%itype_sph_filter             &
     &                     .eq. iflag_recursive_filter                  &
     &     .or. dynamic_SPH%sph_filters(i)%itype_radial_filter          &
     &                     .eq. iflag_recursive_filter) then
          if(dynamic_SPH%sph_filters(i)%id_1st_ref_filter .ge. i) then
            dynamic_SPH%sph_filters(i)%id_1st_ref_filter = i - 1
          end if
          if(dynamic_SPH%sph_filters(i)%id_2nd_ref_filter .ge. i) then
            dynamic_SPH%sph_filters(i)%id_2nd_ref_filter = i - 1
          end if
        end if
      end do
!
      if(     dynamic_SPH%sph_filters(i)%itype_sph_filter               &
     &                     .eq. iflag_recursive_filter                  &
     &   .or. dynamic_SPH%sph_filters(i)%itype_radial_filter            &
     &                     .eq. iflag_recursive_filter) then
        call calypso_mpi_abort                                          &
     &   (ierr_SGS, 'Do not set recursive filter for the first filter')
      end if
!
      end subroutine set_control_SPH_SGS_filters
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SPH_SGS_1filter                            &
     &         (sph_filter_ctl, sph_each_filter)
!
      use t_ctl_data_SGS_filter
      use t_sph_filtering_data
      use skip_comment_f
!
      type(sph_filter_ctl_type), intent(in) :: sph_filter_ctl
      type(sph_filters_type), intent(inout) :: sph_each_filter
!
      character(len=kchara) :: tmpchara
!
!
      sph_each_filter%itype_sph_filter = iflag_gaussian_filter
      if(sph_filter_ctl%sph_filter_type_ctl%iflag .gt. 0) then
        tmpchara = sph_filter_ctl%sph_filter_type_ctl%charavalue
        if     (cmp_no_case(tmpchara,gaussian_label)) then
          sph_each_filter%itype_sph_filter = iflag_gaussian_filter
        else if(cmp_no_case(tmpchara,cutoff_label)) then
          sph_each_filter%itype_sph_filter = iflag_cutoff_filter
        else if(cmp_no_case(tmpchara,recursive_label)) then
          sph_each_filter%itype_sph_filter = iflag_recursive_filter
        end if
      end if
!
      sph_each_filter%itype_radial_filter = iflag_gaussian_filter
      if(sph_filter_ctl%radial_filter_type_ctl%iflag .gt. 0) then
        tmpchara = sph_filter_ctl%radial_filter_type_ctl%charavalue
        if     (cmp_no_case(tmpchara,gaussian_label)) then
          sph_each_filter%itype_radial_filter = iflag_gaussian_filter
!        else if(cmp_no_case(tmpchara,cutoff_label)) then
!          sph_each_filter%itype_radial_filter = iflag_cutoff_filter
        else if(cmp_no_case(tmpchara,recursive_label)) then
          sph_each_filter%itype_radial_filter = iflag_recursive_filter
        end if
      end if
!
      if(sph_filter_ctl%maximum_moments_ctl%iflag .gt. 0) then
        sph_each_filter%r_moments%num_momentum                          &
     &     = sph_filter_ctl%maximum_moments_ctl%intvalue
        sph_each_filter%sph_moments%num_momentum                        &
     &     = sph_filter_ctl%maximum_moments_ctl%intvalue
      end if
!
      if(sph_filter_ctl%radial_filter_width_ctl%iflag .gt. 0) then
        sph_each_filter%width                                           &
     &     = sph_filter_ctl%radial_filter_width_ctl%realvalue
      end if
!
      if(sph_filter_ctl%sphere_filter_width_ctl%iflag .gt. 0) then
        sph_each_filter%sph_filter%f_width                              &
     &     = sph_filter_ctl%sphere_filter_width_ctl%realvalue
      end if
!
      sph_each_filter%id_1st_ref_filter = 1
      sph_each_filter%id_2nd_ref_filter = 1
      if(sph_each_filter%itype_sph_filter                               &
     &    .eq. iflag_recursive_filter) then
        if(sph_filter_ctl%first_reference_ctl%iflag .gt. 0) then
          sph_each_filter%id_1st_ref_filter                             &
     &        = sph_filter_ctl%first_reference_ctl%intvalue
        end if
        if(sph_filter_ctl%second_reference_ctl%iflag .gt. 0) then
          sph_each_filter%id_2nd_ref_filter                             &
     &        = sph_filter_ctl%second_reference_ctl%intvalue
        end if
      end if
!
      end subroutine set_control_SPH_SGS_1filter
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_filter
