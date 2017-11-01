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
!!      subroutine set_control_SPH_SGS_1filter                          &
!!     &         (num_sph_filter_ctl, sph_filter_ctl, sph_filters)
!!        type(sph_filter_ctl_type), intent(in) :: sph_filter_ctl
!!        type(sph_filters_type), intent(inout) :: sph_filters
!!      subroutine set_control_SPH_SGS_3filter                          &
!!     &         (num_sph_filter_ctl, sph_filter_ctl, sph_filters)
!!        type(sph_filter_ctl_type), intent(in) :: sph_filter_ctl(2)
!!        type(sph_filters_type), intent(inout) :: sph_filters(3)
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SPH_SGS_1filter                            &
     &         (num_sph_filter_ctl, sph_filter_ctl, sph_filters)
!
      use t_ctl_data_SGS_filter
      use t_sph_filtering_data
!
      integer(kind = kint), intent(in) :: num_sph_filter_ctl
      type(sph_filter_ctl_type), intent(in) :: sph_filter_ctl
      type(sph_filters_type), intent(inout) :: sph_filters
!
!
      if(num_sph_filter_ctl .le. 0) then
        call calypso_mpi_abort(1, 'Set filter configrations')
      end if
!
      if(sph_filter_ctl%maximum_moments_ctl%iflag .gt. 0) then
        sph_filters%r_moments%num_momentum                              &
     &     = sph_filter_ctl%maximum_moments_ctl%intvalue
        sph_filters%sph_moments%num_momentum                            &
     &     = sph_filter_ctl%maximum_moments_ctl%intvalue
      end if
!
      if(sph_filter_ctl%radial_filter_width_ctl%iflag .gt. 0) then
        sph_filters%width                                               &
     &     = sph_filter_ctl%radial_filter_width_ctl%realvalue
      end if
!
      if(sph_filter_ctl%sphere_filter_width_ctl%iflag .gt. 0) then
        sph_filters%sph_filter%f_width                                  &
     &     = sph_filter_ctl%sphere_filter_width_ctl%realvalue
      end if
!
      end subroutine set_control_SPH_SGS_1filter
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SPH_SGS_3filter                            &
     &         (num_sph_filter_ctl, sph_filter_ctl, sph_filters)
!
      use t_ctl_data_SGS_filter
      use t_sph_filtering_data
!
      integer(kind = kint), intent(in) :: num_sph_filter_ctl
      type(sph_filter_ctl_type), intent(in) :: sph_filter_ctl(2)
      type(sph_filters_type), intent(inout) :: sph_filters(3)
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
      if(sph_filter_ctl(2)%maximum_moments_ctl%iflag .gt. 0) then
        sph_filters(2)%r_moments%num_momentum                           &
     &     = sph_filter_ctl(2)%maximum_moments_ctl%intvalue
        sph_filters(2)%sph_moments%num_momentum                         &
     &     = sph_filter_ctl(2)%maximum_moments_ctl%intvalue
!
        sph_filters(3)%r_moments%num_momentum                           &
     &     = sph_filter_ctl(2)%maximum_moments_ctl%intvalue
        sph_filters(3)%sph_moments%num_momentum                         &
     &     = sph_filter_ctl(2)%maximum_moments_ctl%intvalue
      end if
!
      if(sph_filter_ctl(2)%radial_filter_width_ctl%iflag .gt. 0) then
        sph_filters(2)%width                                            &
     &     = sph_filter_ctl(2)%radial_filter_width_ctl%realvalue
        sph_filters(3)%width                                            &
     &     = two*sph_filter_ctl(2)%radial_filter_width_ctl%realvalue
      end if
!
      if(sph_filter_ctl(2)%sphere_filter_width_ctl%iflag .gt. 0) then
        sph_filters(2)%sph_filter%f_width                               &
     &     = sph_filter_ctl(2)%sphere_filter_width_ctl%realvalue
        sph_filters(3)%sph_filter%f_width                               &
     &     = itwo * sph_filter_ctl(2)%sphere_filter_width_ctl%realvalue
      end if
!
      end subroutine set_control_SPH_SGS_3filter
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_filter
