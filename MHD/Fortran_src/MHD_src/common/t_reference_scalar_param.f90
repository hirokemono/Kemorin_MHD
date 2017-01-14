!>@file   t_reference_scalar_param.f90
!!@brief  module t_reference_scalar_param
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Okuda in 2000
!!@n    Mmodified by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!!@n    Mmodified by H. Matsui in Jan, 2017
!
!> @brief set reference fields for MHD simulation from control data
!!
!!@verbatim
!!      subroutine set_reference_scalar_ctl(charaflag,                  &
!!     &          ref_temp_ctl, low_temp_ctl, high_temp_ctl,            &
!!     &          stratified_ctl, takepiro_ctl, ref_param, takepiro)
!!        type(read_character_item), intent(in) :: ref_temp_ctl
!!        type(read_character_item), intent(in) :: stratified_ctl
!!        type(reference_point_control), intent(in) :: low_temp_ctl
!!        type(reference_point_control), intent(in) :: high_temp_ctl
!!        type(reference_point_control), intent(in) :: takepiro_ctl
!!        type(reference_scalar_param), intent(inout) :: ref_param
!!        type(takepiro_model_param), intent(inout) :: takepiro
!!@endverbatim
!
      module t_reference_scalar_param
!
      use m_precision
      use m_error_IDs
!
      use m_machine_parameter
      use m_control_parameter
      use m_t_int_parameter
!
      implicit  none
!
      type reference_scalar_param
!>      temperature setting
        integer (kind=kint) :: iflag_reference
!
!>        reference lowest temperature (at upper boundary)
        real (kind = kreal) :: low_value
!>        reference highest temperature (at lower boundary)
        real (kind = kreal) :: high_value
!>        position at lowest temperature (upper boundary)
        real (kind = kreal) :: depth_low
!>        position at highest temperature (lower boundary)
        real (kind = kreal) :: depth_high
      end type reference_scalar_param
!
!
      type takepiro_model_param
!>      temperature setting
        integer (kind=kint) :: iflag_stratified
!
!>       Parameter for stratified layer (amplitude)
        real  (kind=kreal) :: stratified_sigma
!>       Parameter for stratified layer (thckness)
        real  (kind=kreal) :: stratified_width
!>       Parameter for stratified layer (radius)
        real  (kind=kreal) :: stratified_outer_r
      end type takepiro_model_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_reference_scalar_ctl(charaflag,                    &
     &          ref_temp_ctl, low_temp_ctl, high_temp_ctl,              &
     &          stratified_ctl, takepiro_ctl, ref_param, takepiro)
!
      use calypso_mpi
      use m_t_step_parameter
      use t_ctl_data_temp_model
      use t_control_elements
!
      character(len = kchara), intent(in) :: charaflag
      type(read_character_item), intent(in) :: ref_temp_ctl
      type(read_character_item), intent(in) :: stratified_ctl
      type(reference_point_control), intent(in) :: low_temp_ctl
      type(reference_point_control), intent(in) :: high_temp_ctl
      type(takepiro_model_control), intent(in) :: takepiro_ctl
!
      type(reference_scalar_param), intent(inout) :: ref_param
      type(takepiro_model_param), intent(inout) :: takepiro
!
      integer (kind = kint) :: iflag
      character(len=kchara) :: tmpchara
!
!   set control for temperature 
!
      if (ref_temp_ctl%iflag .eq. 0) then
        ref_param%iflag_reference = id_no_ref_temp
      else
        tmpchara = ref_temp_ctl%charavalue
        if (cmp_no_case(tmpchara, 'spherical_shell')) then
          ref_param%iflag_reference = id_sphere_ref_temp
        else if (cmp_no_case(tmpchara, 'sph_constant_heat')) then
          ref_param%iflag_reference = id_linear_r_ref_temp
        else if (cmp_no_case(tmpchara, 'linear_x')) then
          ref_param%iflag_reference = id_x_ref_temp
        else if (cmp_no_case(tmpchara, 'linear_y')) then
          ref_param%iflag_reference = id_y_ref_temp
        else if (cmp_no_case(tmpchara, 'linear_z')) then
          ref_param%iflag_reference = id_z_ref_temp
        end if
      end if
!
      iflag = low_temp_ctl%depth%iflag*low_temp_ctl%value%iflag
      if (iflag .eq. 0) then
        if (ref_param%iflag_reference .eq. id_no_ref_temp) then
          ref_param%low_value  =  0.0d0
          ref_param%depth_low  =  0.0d0
        else
          e_message                                                     &
     &          = 'Set lower temperature and its position'
          call calypso_MPI_abort(ierr_fld, e_message)
        end if
      else
        ref_param%low_value  = low_temp_ctl%value%realvalue
        ref_param%depth_low  = low_temp_ctl%depth%realvalue
      end if
!
      iflag = high_temp_ctl%depth%iflag*high_temp_ctl%value%iflag
      if (iflag .eq. 0) then
        if (ref_param%iflag_reference .eq. id_no_ref_temp) then
          ref_param%high_value =  0.0d0
          ref_param%depth_high =  0.0d0
        else
          e_message                                                     &
     &         = 'Set lower temperature and its position'
          call calypso_MPI_abort(ierr_fld, e_message)
        end if
      else
        ref_param%high_value = high_temp_ctl%value%realvalue
        ref_param%depth_high = high_temp_ctl%depth%realvalue
      end if
!
!
      takepiro%iflag_stratified = id_turn_OFF
      if (stratified_ctl%iflag .gt. id_turn_OFF                         &
        .and. yes_flag(stratified_ctl%charavalue))  then
         takepiro%iflag_stratified = id_turn_ON
      end if
!
      if (takepiro%iflag_stratified .eq. id_turn_OFF) then
        takepiro%stratified_sigma = 0.0d0
        takepiro%stratified_width = 0.0d0
        takepiro%stratified_outer_r = 0.0d0
      else
        iflag = takepiro_ctl%stratified_sigma_ctl%iflag                 &
     &         *takepiro_ctl%stratified_width_ctl%iflag                 &
     &         *takepiro_ctl%stratified_outer_r_ctl%iflag
        if(iflag .eq. 0) then
          e_message                                                     &
     &        = 'Set parameteres for stratification'
          call calypso_MPI_abort(ierr_fld, e_message)
        else
          takepiro%stratified_sigma                                     &
     &          = takepiro_ctl%stratified_sigma_ctl%realvalue
          takepiro%stratified_width                                     &
     &          = takepiro_ctl%stratified_width_ctl%realvalue
          takepiro%stratified_outer_r                                   &
     &           = takepiro_ctl%stratified_outer_r_ctl%realvalue
        end if
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) trim(charaflag)
        write(*,*) 'iflag_reference ', ref_param%iflag_reference
        write(*,*) 'low_value ',       ref_param%low_value
        write(*,*) 'high_value ',      ref_param%high_value
        write(*,*) 'depth_low ',       ref_param%depth_low
        write(*,*) 'depth_high ',      ref_param%depth_high
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_t_strat ',      takepiro%iflag_stratified
        write(*,*) 'stratified_sigma ',   takepiro%stratified_sigma
        write(*,*) 'stratified_width ',   takepiro%stratified_width
        write(*,*) 'stratified_outer_r ', takepiro%stratified_outer_r
      end if
!
      end subroutine set_reference_scalar_ctl
!
! -----------------------------------------------------------------------
!
      end module t_reference_scalar_param
