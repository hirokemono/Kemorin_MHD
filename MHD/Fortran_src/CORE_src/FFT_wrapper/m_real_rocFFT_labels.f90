!>@file   m_real_rocFFT_labels.f90
!!@brief  module m_real_rocFFT_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for rocFFT
!!
!!@verbatim
!!      subroutine init_rocFFT_r2r_flags(rocFFT_base_flags)
!!        type(multi_flag_labels), intent(in) :: rocFFT_base_flags
!!      integer(kind = kint) function find_set_rocFFT_r2r_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara)                                         &
!!     &               function find_rocFFT_r2r_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_rocFFT_r2r_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!   ------------------------------------------------------------------
!!    FFT Package lists
!!      rocFFT_real:             AMD rocFFT with real data only
!!   ------------------------------------------------------------------
!!    FFT size flags
!!
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!@endverbatim
      module m_real_rocFFT_labels
!
      use m_precision
      use m_FFT_selector
      use t_multi_flag_labels
!
      implicit none
!
!
!>      flag parts for real-real rocFFT
      character(len = kchara), parameter :: r2r_names(2)                &
     &                     = (/'real', 'r2r '/)
!
!>       Character lables for at once real-real rocFFT for transform
!!         'rocFFT_real', 'rocFFT_r2r', 'real_rocFFT', 'r2r_rocFFT',
!!         'rocFFT_real_once', 'rocFFT_real_at_once', 'rocFFT_r2r_once',
!!         'rocFFT_r2r_at_once',  'real_rocFFT_once',
!!         'real_rocFFT_at_once', 'r2r_rocFFT_once', 
!!         'r2r_rocFFT_at_once',  'once_rocFFT_real', 'once_rocFFT_r2r',
!!         'once_real_rocFFT',    'once_r2r_rocFFT', 
!!         'at_once_rocFFT_real', 'at_once_rocFFT_r2r',
!!         'at_once_real_rocFFT', 'at_once_r2r_rocFFT' 
      type(multi_flag_labels), save :: at_once_rocFFT_r2r_flags
!>       Character lables for once real-real rocFFT over domain
!!         'rocFFT_real_domain', 'rocFFT_r2r_domain', 
!!         'real_rocFFT_domain', 'r2r_rocFFT_domain', 
!!         'domain_rocFFT_real', 'domain_rocFFT_r2r', 
!!         'domain_real_rocFFT', 'domain_r2r_rocFFT' 
      type(multi_flag_labels), save :: domain_rocFFT_r2r_flags
!>       Character lables for once real-real rocFFT over component
!!          'rocFFT_real_component', 'rocFFT_real_comps', 
!!          'rocFFT_r2r_component',  'rocFFT_r2r_comps', 
!!          'real_rocFFT_component', 'real_rocFFT_comps',
!!          'r2r_rocFFT_component',  'r2r_rocFFT_comps',
!!          'component_rocFFT_real', 'component_rocFFT_r2r',
!!          'component_real_rocFFT', 'component_r2r_rocFFT',
!!          'comps_rocFFT_real', 'comps_rocFFT_r2r',
!!          'comps_real_rocFFT', 'comps_r2r_rocFFT'
      type(multi_flag_labels), save :: comp_rocFFT_r2r_flags
!>       Character lables for single real-real rocFFT
!!          'rocFFT_real_single', 'rocFFT_real_sgl', 
!!          'rocFFT_r2r_single',  'rocFFT_r2r_sgl', 
!!          'real_rocFFT_single', 'real_rocFFT_sgl', 
!!          'r2r_rocFFT_single',  'r2r_rocFFT_sgl', 
!!          'single_rocFFT_real', 'single_rocFFT_r2r',
!!          'single_real_rocFFT', 'single_r2r_rocFFT', 
!!          'sgl_rocFFT_real',    'sgl_rocFFT_r2r',
!!          'sgl_real_rocFFT',    'sgl_r2r_rocFFT' 
     type(multi_flag_labels), save :: single_rocFFT_r2r_flags
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_rocFFT_r2r_flags(rocFFT_base_flags)
!
      type(multi_flag_labels), intent(in) :: rocFFT_base_flags
!
!>     Character lables for real-complex rocFFT:
!!          'rocFFT_real', 'rocFFT_r2r', 'real_rocFFT', 'r2r_rocFFT' 
      type(multi_flag_labels) :: rocFFT_r2r_flags
!>     Character lables for real to complex: 'real', r2r'
      type(multi_flag_labels) :: r2r_flags
!
      integer(kind = kint) :: icou
!
      call init_multi_flags_by_labels(itwo, r2r_names, r2r_flags)
      call init_from_two_kinds_flags(rocFFT_base_flags, r2r_flags,      &
     &                               rocFFT_r2r_flags, icou)
!
      call init_each_FFT_mode_flags(rocFFT_r2r_flags,                   &
     &    at_once_rocFFT_r2r_flags, domain_rocFFT_r2r_flags,            &
     &    comp_rocFFT_r2r_flags, single_rocFFT_r2r_flags)
!
      end subroutine init_rocFFT_r2r_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_rocFFT_r2r_flag(label)
!
      character(len = kchara), intent(in) :: label
      integer(kind = kint) :: iflag_fft
!
      if(check_mul_flags(label, at_once_rocFFT_r2r_flags))     then
        iflag_fft = iflag_real_rocFFT + iflag_once_fft
      else if(check_mul_flags(label, domain_rocFFT_r2r_flags)) then
        iflag_fft = iflag_real_rocFFT + iflag_domain_once
      else if(check_mul_flags(label, comp_rocFFT_r2r_flags))   then
        iflag_fft = iflag_real_rocFFT + iflag_component_once
      else if(check_mul_flags(label, single_rocFFT_r2r_flags)) then
        iflag_fft = iflag_real_rocFFT + iflag_single_fft
      else
        iflag_fft = -1
      end if
      find_set_rocFFT_r2r_flag = iflag_fft
!
      end function find_set_rocFFT_r2r_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara)                                           &
     &               function find_rocFFT_r2r_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if(mod(iflag_fft,10) .eq. iflag_once_fft) then
        tmpchara = at_once_rocFFT_r2r_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_domain_once) then
        tmpchara = domain_rocFFT_r2r_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_component_once) then
        tmpchara = comp_rocFFT_r2r_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_single_fft) then
        tmpchara = single_rocFFT_r2r_flags%flags(1)
      end if
      find_rocFFT_r2r_label = tmpchara
!
      end function find_rocFFT_r2r_label
!
! ----------------------------------------------------------------------
!
      subroutine check_rocFFT_r2r_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara) :: title
!
      write(title,'(4x,a)') 'at_once_rocFFT_r2r_flags:'
      call write_multi_flags(id_file, title, at_once_rocFFT_r2r_flags)
      write(title,'(4x,a)') 'domain_rocFFT_r2r_flags:'
      call write_multi_flags(id_file, title, domain_rocFFT_r2r_flags)
      write(title,'(4x,a)') 'comp_rocFFT_r2r_flags:'
      call write_multi_flags(id_file, title, comp_rocFFT_r2r_flags)
      write(title,'(4x,a)') 'single_rocFFT_r2r_flags:'
      call write_multi_flags(id_file, title, single_rocFFT_r2r_flags)
!
      end subroutine check_rocFFT_r2r_mode_flags
!
! ----------------------------------------------------------------------
!
      end module m_real_rocFFT_labels
