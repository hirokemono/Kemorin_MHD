!>@file   m_OMP_rocFFT_labels.f90
!!@brief  module m_OMP_rocFFT_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for rocFFT
!!
!!@verbatim
!!      subroutine init_OMP_rocFFT_flags(rocFFT_r2c_flags)
!!        type(multi_flag_labels), intent(in) :: rocFFT_r2c_flags
!!      integer(kind = kint) function find_set_OMP_rocFFT_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara)                                         &
!!     &                function find_OMP_rocFFT_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_OMP_rocFFT_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!   ------------------------------------------------------------------
!!    FFT Package lists
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
      module m_OMP_rocFFT_labels
!
      use m_precision
      use m_FFT_selector
      use t_multi_flag_labels
!
      implicit none
!
!>      flag parts for OpenMP
      character(len = kchara), parameter, private                       &
     &               :: OpenMP_names(2) = (/'OpenMP', 'OMP   '/)
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
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_OMP_rocFFT_flags(rocFFT_base_flags)
!
      type(multi_flag_labels), intent(in) :: rocFFT_base_flags
!>     Character lables for real only rocFFT: 
!!          'rocFFT_OpenMP', 'rocFFT_OMP', 'OpenMP_rocFFT', 'OMP_rocFFT'
      type(multi_flag_labels) :: rocFFT_OMP_flags
      type(multi_flag_labels) :: tmp_flags
      integer(kind = kint) :: icou
!
!
      call init_multi_flags_by_labels(itwo, OpenMP_names, tmp_flags)
      call init_from_two_kinds_flags(tmp_flags, rocFFT_base_flags,      &
     &                               rocFFT_OMP_flags, icou)
      call dealloc_multi_flags(tmp_flags)
!
      call init_each_FFT_mode_flags(rocFFT_OMP_flags,                   &
     &    at_once_OMP_rocFFT_flags, domain_OMP_rocFFT_flags,            &
     &    comp_OMP_rocFFT_flags, single_OMP_rocFFT_flags)
      call dealloc_multi_flags(rocFFT_OMP_flags)
!
      end subroutine init_OMP_rocFFT_flags
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
!
      character(len = kchara)                                           &
     &                function find_OMP_rocFFT_label(iflag_fft)
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
      subroutine check_OMP_rocFFT_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara) :: title
!
      write(title,'(4x,a)') 'at_once_OMP_rocFFT_flags:'
      call write_multi_flags(id_file, title, at_once_OMP_rocFFT_flags)
      write(title,'(4x,a)') 'domain_OMP_rocFFT_flags:'
      call write_multi_flags(id_file, title, domain_OMP_rocFFT_flags)
      write(title,'(4x,a)') 'comp_OMP_rocFFT_flags:'
      call write_multi_flags(id_file, title, comp_OMP_rocFFT_flags)
      write(title,'(4x,a)') 'single_OMP_rocFFT_flags:'
      call write_multi_flags(id_file, title, single_OMP_rocFFT_flags)
!
      end subroutine check_OMP_rocFFT_mode_flags
!
! ----------------------------------------------------------------------
!
      end module m_OMP_rocFFT_labels
