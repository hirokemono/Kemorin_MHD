!>@file   m_rocFFT_labels.f90
!!@brief  module m_rocFFT_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for rocFFT
!!
!!@verbatim
!!      subroutine init_rocFFT_mode_flags()
!!      integer(kind = kint) function find_set_rocFFT_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara) function find_rocFFT_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_rocFFT_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!   ------------------------------------------------------------------
!!    FFT Package lists
!!      rocFFT, rocFFT_complex:  AMD rocFFT
!!      rocFFT_real:             AMD rocFFT with real data only
!!      OpenMP_rocFFT:           AMD rocFFT with OpenMP offloading
!!   ------------------------------------------------------------------
!!    FFT size flags
!!
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!@endverbatim
      module m_rocFFT_labels
!
      use m_precision
      use m_FFT_selector
      use t_multi_flag_labels
!
      implicit none
!
!>      Character flag to use rocFFT
      character(len = kchara), parameter, private                       &
     &                              :: hd_rocFFT =     'rocFFT'
!>      Character flag to use real to real rocFFT
      character(len = kchara), parameter, private                       &
     &                              :: hd_rocFFT_r2r = 'rocFFT_real'
!>      Character flag to use rocFFT with OpenMP
      character(len = kchara), parameter, private                       &
     &                              :: hd_OMP_rocFFT = 'OpenMP_rocFFT'
!
!>      flag parts for OpenMP
      character(len = kchara), parameter, private                       &
     &               :: OpenMP_names(2) = (/'OpenMP', 'OMP   '/)
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
!>       Character lables for at once rocFFT  with OpenMP for transform
!!          'OpenMP_rocFFT',         'OMP_rocFFT', 
!!          'rocFFT_OpenMP',         'rocFFT_OMP',
!!          'OpenMP_rocFFT_once',    'OpenMP_rocFFT_at_once',
!!          'OMP_rocFFT_once',       'OMP_rocFFT_at_once',
!!          'rocFFT_OpenMP_once',    'rocFFT_OpenMP_at_once',
!!          'rocFFT_OMP_once',       'rocFFT_OMP_at_once',
!!          'once_OpenMP_rocFFT',    'once_OMP_rocFFT',
!!          'once_rocFFT_OpenMP',    'once_rocFFT_OMP',
!!          'at_once_OpenMP_rocFFT', 'at_once_OMP_rocFFT',
!!          'at_once_rocFFT_OpenMP', 'at_once_rocFFT_OMP'
      type(multi_flag_labels), save :: at_once_OMP_rocFFT_flags
!>       Character lables for once rocFFT  with OpenMP over domain
!!          'OpenMP_rocFFT_domain', 'OMP_rocFFT_domain',
!!          'rocFFT_OpenMP_domain', 'rocFFT_OMP_domain',
!!          'domain_OpenMP_rocFFT', 'domain_OMP_rocFFT',
!!          'domain_rocFFT_OpenMP', 'domain_rocFFT_OMP'
      type(multi_flag_labels), save :: domain_OMP_rocFFT_flags
!>       Character lables for once rocFFT with OpenMP over component
!!          'OpenMP_rocFFT_component', 'OpenMP_rocFFT_comps',
!!          'OMP_rocFFT_component',    'OMP_rocFFT_comps', 
!!          'rocFFT_OpenMP_component', 'rocFFT_OpenMP_comps',
!!          'rocFFT_OMP_component',    'rocFFT_OMP_comps',
!!          'component_OpenMP_rocFFT', 'component_OMP_rocFFT',
!!          'component_rocFFT_OpenMP', 'component_rocFFT_OMP',
!!          'comps_OpenMP_rocFFT',     'comps_OMP_rocFFT',
!!          'comps_rocFFT_OpenMP',     'comps_rocFFT_OMP'
      type(multi_flag_labels), save :: comp_OMP_rocFFT_flags
!>       Character lables for single rocFFT with OpenMP
!!          'OpenMP_rocFFT_single', 'OpenMP_rocFFT_sgl',
!!          'OMP_rocFFT_single',    'OMP_rocFFT_sgl',
!!          'rocFFT_OpenMP_single', 'rocFFT_OpenMP_sgl',
!!          'rocFFT_OMP_single',    'rocFFT_OMP_sgl', 
!!          'single_OpenMP_rocFFT', 'single_OMP_rocFFT',
!!          'single_rocFFT_OpenMP', 'single_rocFFT_OMP', 
!!          'sgl_OpenMP_rocFFT',    'sgl_OMP_rocFFT', 
!!          'sgl_rocFFT_OpenMP',    'sgl_rocFFT_OMP' 
      type(multi_flag_labels), save :: single_OMP_rocFFT_flags
!
      private :: init_rocFFT_flags
      private :: find_set_rocFFT_r2c_flag, find_rocFFT_r2c_label
      private :: find_set_rocFFT_r2r_flag, find_rocFFT_r2r_label
      private :: find_set_OMP_rocFFT_flag, find_OMP_rocFFT_label
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_rocFFT_mode_flags()
!
!>     Character lables for real-complex rocFFT: 
!!           'rocFFT', 'rocFFT_complex', 'rocFFT_r2c',
!!                     'complex_rocFFT', 'r2c_rocFFT'
      type(multi_flag_labels) :: rocFFT_r2c_flags
!>     Character lables for real-complex rocFFT:
!!          'rocFFT_real', 'rocFFT_r2r', 'real_rocFFT', 'r2r_rocFFT' 
      type(multi_flag_labels) :: rocFFT_r2r_flags
!>     Character lables for real only rocFFT: 
!!          'rocFFT_OpenMP', 'rocFFT_OMP', 'OpenMP_rocFFT', 'OMP_rocFFT'
      type(multi_flag_labels) :: rocFFT_OMP_flags
!
!
      call init_rocFFT_flags(rocFFT_r2c_flags, rocFFT_r2r_flags,        &
     &                       rocFFT_OMP_flags)
!
      call init_each_FFT_mode_flags(rocFFT_r2c_flags,                   &
     &    at_once_rocFFT_r2c_flags, domain_rocFFT_r2c_flags,            &
     &    comp_rocFFT_r2c_flags, single_rocFFT_r2c_flags)
!
      call init_each_FFT_mode_flags(rocFFT_r2r_flags,                   &
     &    at_once_rocFFT_r2r_flags, domain_rocFFT_r2r_flags,            &
     &    comp_rocFFT_r2r_flags, single_rocFFT_r2r_flags)
!
      call init_each_FFT_mode_flags(rocFFT_OMP_flags,                   &
     &    at_once_OMP_rocFFT_flags, domain_OMP_rocFFT_flags,            &
     &    comp_OMP_rocFFT_flags, single_OMP_rocFFT_flags)
!
      end subroutine init_rocFFT_mode_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_rocFFT_flag(label)
!
      character(len = kchara), intent(in) :: label
!
      integer(kind = kint) :: iflag_fft
!
      iflag_fft = find_set_rocFFT_r2c_flag(label)
      if(iflag_fft .lt. 0) iflag_fft = find_set_rocFFT_r2r_flag(label)
      if(iflag_fft .lt. 0) iflag_fft = find_set_OMP_rocFFT_flag(label)
      find_set_rocFFT_flag = iflag_fft
!
      end function find_set_rocFFT_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara) function find_rocFFT_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
!
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if((iflag_fft/10) .eq. (iflag_rocFFT/10)) then
        tmpchara = find_rocFFT_r2c_label(iflag_fft)
      else if((iflag_fft/10) .eq. (iflag_real_rocFFT/10)) then
        tmpchara = find_rocFFT_r2r_label(iflag_fft)
      else if((iflag_fft/10) .eq. (iflag_OMP_rocFFT/10)) then
        tmpchara = find_OMP_rocFFT_label(iflag_fft)
      end if
      find_rocFFT_label = tmpchara
!
      end function find_rocFFT_label
!
! ----------------------------------------------------------------------
!
      subroutine check_rocFFT_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara) :: title
!
      write(id_file,'(a)') ''
      write(title,'(a)') trim('      at_once_rocFFT_r2c_flags:')
      call write_multi_flags(id_file, title, at_once_rocFFT_r2c_flags)
      write(title,'(a)') trim('      domain_rocFFT_r2c_flags:')
      call write_multi_flags(id_file, title, domain_rocFFT_r2c_flags)
      write(title,'(a)') trim('      comp_rocFFT_r2c_flags:')
      call write_multi_flags(id_file, title, comp_rocFFT_r2c_flags)
      write(title,'(a)') trim('      single_rocFFT_r2c_flags:')
      call write_multi_flags(id_file, title, single_rocFFT_r2c_flags)
!
!
      write(id_file,'(a)') ''
      write(title,'(a)') trim('      at_once_rocFFT_r2r_flags:')
      call write_multi_flags(id_file, title, at_once_rocFFT_r2r_flags)
      write(title,'(a)') trim('      domain_rocFFT_r2r_flags:')
      call write_multi_flags(id_file, title, domain_rocFFT_r2r_flags)
      write(title,'(a)') trim('      comp_rocFFT_r2r_flags:')
      call write_multi_flags(id_file, title, comp_rocFFT_r2r_flags)
      write(title,'(a)') trim('      single_rocFFT_r2r_flags:')
      call write_multi_flags(id_file, title, single_rocFFT_r2r_flags)
!
!
      write(id_file,'(a)') ''
      write(title,'(a)') trim('      at_once_OMP_rocFFT_flags:')
      call write_multi_flags(id_file, title, at_once_OMP_rocFFT_flags)
      write(title,'(a)') trim('      domain_OMP_rocFFT_flags:')
      call write_multi_flags(id_file, title, domain_OMP_rocFFT_flags)
      write(title,'(a)') trim('      comp_OMP_rocFFT_flags:')
      call write_multi_flags(id_file, title, comp_OMP_rocFFT_flags)
      write(title,'(a)') trim('      single_OMP_rocFFT_flags:')
      call write_multi_flags(id_file, title, single_OMP_rocFFT_flags)
!
      end subroutine check_rocFFT_mode_flags
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_rocFFT_flags(rocFFT_r2c_flags, rocFFT_r2r_flags,  &
     &                             rocFFT_OMP_flags)
!
      type(multi_flag_labels), intent(inout) :: rocFFT_r2c_flags
      type(multi_flag_labels), intent(inout) :: rocFFT_r2r_flags
      type(multi_flag_labels), intent(inout) :: rocFFT_OMP_flags
!
!>     Character lables for real to complex: 'complex', r2c'
      type(multi_flag_labels) :: r2c_flags
!>     Character lables for real to complex: 'real', r2r'
      type(multi_flag_labels) :: r2r_flags
!
      type(multi_flag_labels) :: tmp_flags
      integer(kind = kint) :: icou
!
!
      call init_multi_flags_by_one_label(hd_rocFFT, rocFFT_r2c_flags)
!
      call init_multi_flags_by_labels(itwo, OpenMP_names, tmp_flags)
      call init_from_two_kinds_flags(tmp_flags, rocFFT_r2c_flags,       &
     &                               rocFFT_OMP_flags, icou)
      call dealloc_multi_flags(tmp_flags)
!
      call init_multi_flags_by_labels(itwo, r2c_names, r2c_flags)
      call init_from_two_kinds_flags(rocFFT_r2c_flags, r2c_flags,       &
     &                               tmp_flags, icou)
      call append_multi_flag_labels(tmp_flags, rocFFT_r2c_flags)
      call dealloc_multi_flags(tmp_flags)
!
      call init_multi_flags_by_one_label(hd_rocFFT, tmp_flags)
      call init_multi_flags_by_labels(itwo, r2r_names, r2r_flags)
      call init_from_two_kinds_flags(tmp_flags, r2r_flags,              &
     &                               rocFFT_r2r_flags, icou)
      call dealloc_multi_flags(tmp_flags)
!
      end subroutine init_rocFFT_flags
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
      integer(kind = kint) function find_set_OMP_rocFFT_flag(label)
!
      character(len = kchara), intent(in) :: label
      integer(kind = kint) :: iflag_fft
!
      if(check_mul_flags(label, at_once_OMP_rocFFT_flags))     then
        iflag_fft = iflag_OMP_rocFFT + iflag_once_fft
      else if(check_mul_flags(label, domain_OMP_rocFFT_flags)) then
        iflag_fft = iflag_OMP_rocFFT + iflag_domain_once
      else if(check_mul_flags(label, comp_OMP_rocFFT_flags))   then
        iflag_fft = iflag_OMP_rocFFT + iflag_component_once
      else if(check_mul_flags(label, single_OMP_rocFFT_flags)) then
        iflag_fft = iflag_OMP_rocFFT + iflag_single_fft
      else
        iflag_fft = -1
      end if
      find_set_OMP_rocFFT_flag = iflag_fft
!
      end function find_set_OMP_rocFFT_flag
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      character(len = kchara) function find_rocFFT_r2c_label(iflag_fft)
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
      character(len = kchara) function find_rocFFT_r2r_label(iflag_fft)
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
      character(len = kchara) function find_OMP_rocFFT_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if(mod(iflag_fft,10) .eq. iflag_once_fft) then
        tmpchara = at_once_OMP_rocFFT_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_domain_once) then
        tmpchara = domain_OMP_rocFFT_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_component_once) then
        tmpchara = comp_OMP_rocFFT_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_single_fft) then
        tmpchara = single_OMP_rocFFT_flags%flags(1)
      end if
      find_OMP_rocFFT_label = tmpchara
!
      end function find_OMP_rocFFT_label
!
! ----------------------------------------------------------------------
!
      end module m_rocFFT_labels
