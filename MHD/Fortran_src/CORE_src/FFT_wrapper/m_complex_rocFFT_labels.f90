!>@file   m_complex_rocFFT_labels.f90
!!@brief  module m_complex_rocFFT_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for rocFFT
!!
!!@verbatim
!!      subroutine init_rocFFT_r2c_flags(rocFFT_base_flags)
!!        type(multi_flag_labels), intent(in) :: rocFFT_base_flags
!!      integer(kind = kint) function find_set_rocFFT_r2c_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara) function                                &
!!     &                       find_rocFFT_r2c_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_rocFFT_r2c_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!   ------------------------------------------------------------------
!!    FFT Package lists
!!      rocFFT, rocFFT_complex:  AMD rocFFT
!!   ------------------------------------------------------------------
!!    FFT size flags
!!
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!@endverbatim
      module m_complex_rocFFT_labels
!
      use m_precision
      use m_FFT_selector
      use t_multi_flag_labels
!
      implicit none
!
!>      flag parts for real-complex rocFFT
      character(len = kchara), parameter :: r2c_names(2)                &
     &                     = (/'complex', 'r2c    '/)
!
!>      Character lables for at once real-complex rocFFT for transform
!!         'rocFFT', 'rocFFT_complex',  'rocFFT_r2c', 'complex_rocFFT',
!!         'r2c_rocFFT', 'rocFFT_once', 'rocFFT_at_once', 
!!         'rocFFT_complex_once',       'rocFFT_complex_at_once',
!!         'rocFFT_r2c_once',           'rocFFT_r2c_at_once',
!!         'complex_rocFFT_once',       'complex_rocFFT_at_once',
!!         'r2c_rocFFT_once',     'r2c_rocFFT_at_once', 'once_rocFFT',
!!         'once_rocFFT_complex', 'once_rocFFT_r2c',
!!         'once_complex_rocFFT', 'once_r2c_rocFFT', 'at_once_rocFFT',
!!         'at_once_rocFFT_complex', 'at_once_rocFFT_r2c',
!!         'at_once_complex_rocFFT', 'at_once_r2c_rocFFT' 
      type(multi_flag_labels), save :: at_once_rocFFT_r2c_flags
!>       Character lables for once real-complex rocFFT over domain
!!          'rocFFT_domain',         'rocFFT_complex_domain', 
!!          'rocFFT_r2c_domain',     'complex_rocFFT_domain', 
!!          'r2c_rocFFT_domain',     'domain_rocFFT', 
!!          'domain_rocFFT_complex', 'domain_rocFFT_r2c', 
!!          'domain_complex_rocFFT', 'domain_r2c_rocFFT' 
      type(multi_flag_labels), save :: domain_rocFFT_r2c_flags
!>       Character lables for once real-complex rocFFT over component
!!          'rocFFT_component',         'rocFFT_comps', 
!!          'rocFFT_complex_component', 'rocFFT_complex_comps',
!!          'rocFFT_r2c_component',     'rocFFT_r2c_comps', 
!!          'complex_rocFFT_component', 'complex_rocFFT_comps',
!!          'r2c_rocFFT_component',     'r2c_rocFFT_comps',
!!          'component_rocFFT',         'component_rocFFT_complex',
!!          'component_rocFFT_r2c',     'component_complex_rocFFT',
!!          'component_r2c_rocFFT',     'comps_rocFFT',
!!          'comps_rocFFT_complex',     'comps_rocFFT_r2c',
!!          'comps_complex_rocFFT',     'comps_r2c_rocFFT' 
      type(multi_flag_labels), save :: comp_rocFFT_r2c_flags
!>       Character lables for single real-complex rocFFT
!!          'rocFFT_single',      'rocFFT_sgl', 'rocFFT_complex_single',
!!          'rocFFT_complex_sgl', 'rocFFT_r2c_single', 'rocFFT_r2c_sgl',
!!          'complex_rocFFT_single', 'complex_rocFFT_sgl', 
!!          'r2c_rocFFT_single',     'r2c_rocFFT_sgl', 'single_rocFFT',
!!          'single_rocFFT_complex', 'single_rocFFT_r2c', 
!!          'single_complex_rocFFT', 'single_r2c_rocFFT', 'sgl_rocFFT',
!!          'sgl_rocFFT_complex',    'sgl_rocFFT_r2c', 
!!          'sgl_complex_rocFFT',    'sgl_r2c_rocFFT' 
     type(multi_flag_labels), save :: single_rocFFT_r2c_flags
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_rocFFT_r2c_flags(rocFFT_base_flags)
!
      type(multi_flag_labels), intent(in) :: rocFFT_base_flags
!
!>     Character lables for real-complex rocFFT: 
!!           'rocFFT', 'rocFFT_complex', 'rocFFT_r2c',
!!                     'complex_rocFFT', 'r2c_rocFFT'
      type(multi_flag_labels) :: rocFFT_r2c_flags
!>     Character lables for real to complex: 'complex', r2c'
      type(multi_flag_labels) :: r2c_flags
      type(multi_flag_labels) :: tmp_flags
      integer(kind = kint) :: icou
!
!
      call init_multi_flags_by_labels(itwo, r2c_names, r2c_flags)
      call init_from_two_kinds_flags(rocFFT_base_flags, r2c_flags,      &
     &                               tmp_flags, icou)
      call dealloc_multi_flags(r2c_flags)
!
      call alloc_multi_flags(izero, rocFFT_r2c_flags)
      call append_multi_flag_labels(rocFFT_base_flags, rocFFT_r2c_flags)
      call append_multi_flag_labels(tmp_flags, rocFFT_r2c_flags)
      call dealloc_multi_flags(tmp_flags)
!
      call init_each_FFT_mode_flags(rocFFT_r2c_flags,                   &
     &    at_once_rocFFT_r2c_flags, domain_rocFFT_r2c_flags,            &
     &    comp_rocFFT_r2c_flags, single_rocFFT_r2c_flags)
      call dealloc_multi_flags(rocFFT_r2c_flags)
!
      end subroutine init_rocFFT_r2c_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_rocFFT_r2c_flag(label)
!
      character(len = kchara), intent(in) :: label
      integer(kind = kint) :: iflag_fft
!
      if(check_mul_flags(label, at_once_rocFFT_r2c_flags))     then
        iflag_fft = iflag_rocFFT + iflag_once_fft
      else if(check_mul_flags(label, domain_rocFFT_r2c_flags)) then
        iflag_fft = iflag_rocFFT + iflag_domain_once
      else if(check_mul_flags(label, comp_rocFFT_r2c_flags))   then
        iflag_fft = iflag_rocFFT + iflag_component_once
      else if(check_mul_flags(label, single_rocFFT_r2c_flags)) then
        iflag_fft = iflag_rocFFT + iflag_single_fft
      else
        iflag_fft = -1
      end if
      find_set_rocFFT_r2c_flag = iflag_fft
!
      end function find_set_rocFFT_r2c_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara) function                                  &
     &                       find_rocFFT_r2c_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if(mod(iflag_fft,10) .eq. iflag_once_fft) then
        tmpchara = at_once_rocFFT_r2c_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_domain_once) then
        tmpchara = domain_rocFFT_r2c_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_component_once) then
        tmpchara = comp_rocFFT_r2c_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_single_fft) then
        tmpchara = single_rocFFT_r2c_flags%flags(1)
      end if
      find_rocFFT_r2c_label = tmpchara
!
      end function find_rocFFT_r2c_label
!
! ----------------------------------------------------------------------
!
      subroutine check_rocFFT_r2c_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara) :: title
!
      write(title,'(4x,a)') 'at_once_rocFFT_r2c_flags:'
      call write_multi_flags(id_file, title, at_once_rocFFT_r2c_flags)
      write(title,'(4x,a)') 'domain_rocFFT_r2c_flags:'
      call write_multi_flags(id_file, title, domain_rocFFT_r2c_flags)
      write(title,'(4x,a)') 'comp_rocFFT_r2c_flags:'
      call write_multi_flags(id_file, title, comp_rocFFT_r2c_flags)
      write(title,'(4x,a)') 'single_rocFFT_r2c_flags:'
      call write_multi_flags(id_file, title, single_rocFFT_r2c_flags)
!
      end subroutine check_rocFFT_r2c_mode_flags
!
! ----------------------------------------------------------------------
!
      end module m_complex_rocFFT_labels
