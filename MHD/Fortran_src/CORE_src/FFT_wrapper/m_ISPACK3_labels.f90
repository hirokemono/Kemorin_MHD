!>@file   m_ISPACK3_labels.f90
!!@brief  module m_ISPACK3_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for ISPACK
!!
!!@verbatim
!!      subroutine init_ISPACK3_mode_flags()
!!      integer(kind = kint) function find_set_ISPACK3_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara) function find_ISPACK3_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_ISPACK3_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!   ------------------------------------------------------------------
!!    FFT Package
!!      ISPACK3:                 ISPACK Ver.3
!!   ------------------------------------------------------------------
!!    FFT size flags
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!@endverbatim
      module m_ISPACK3_labels
!
      use m_precision
      use m_FFT_selector
      use t_multi_flag_labels
!
      implicit none
!
!>      Character flag to use ISPACK
      character(len = kchara), parameter, private                       &
     &                                   :: hd_ISPACK3 =  'ISPACK3'
!
!>     Character lables for at once ISPACKv3 for transform
!!        'ISPACK3', 'ISPACK3_once', 'ISPACK3_at_once',
!!                   'once_ISPACK3', 'at_once_ISPACK3'
      type(multi_flag_labels), save :: at_once_ISPACK3_flags
!>     Character lables for once ISPACKv3 over domain
!!         'ISPACK3_domain', 'domain_ISPACK3'
      type(multi_flag_labels), save :: domain_ISPACK3_flags
!>     Character lables for once ISPACKv3 over component
!!         'ISPACK3_component', 'ISPACK3_comps',
!!         'component_ISPACK3', 'comps_ISPACK3'
      type(multi_flag_labels), save :: comp_ISPACK3_flags
!>     Character lables for single ISPACKv3
!!        'ISPACK3_single', 'ISPACK3_sgl',
!!        'single_ISPACK3', 'sgl_ISPACK3' 
      type(multi_flag_labels), save :: single_ISPACK3_flags
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_ISPACK3_mode_flags()
!
!>     Character lables for ISPACK 3: 'ISPACK3'
      type(multi_flag_labels), save :: ISPACK3_flags
!
      call init_multi_flags_by_one_label(hd_ISPACK3, ISPACK3_flags)
      call init_each_FFT_mode_flags(ISPACK3_flags,                      &
     &    at_once_ISPACK3_flags, domain_ISPACK3_flags,                  &
     &    comp_ISPACK3_flags, single_ISPACK3_flags)
!
      end subroutine init_ISPACK3_mode_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_ISPACK3_flag(label)
!
      character(len = kchara), intent(in) :: label
      integer(kind = kint) :: iflag_fft
!
      if(check_mul_flags(label, at_once_ISPACK3_flags))     then
        iflag_fft = iflag_ISPACK3 + iflag_once_fft
      else if(check_mul_flags(label, domain_ISPACK3_flags)) then
        iflag_fft = iflag_ISPACK3 + iflag_domain_once
      else if(check_mul_flags(label, comp_ISPACK3_flags))   then
        iflag_fft = iflag_ISPACK3 + iflag_component_once
      else if(check_mul_flags(label, single_ISPACK3_flags)) then
        iflag_fft = iflag_ISPACK3 + iflag_single_fft
      else
        iflag_fft = -1
      end if
      find_set_ISPACK3_flag = iflag_fft
!
      end function find_set_ISPACK3_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara) function find_ISPACK3_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if(mod(iflag_fft,10) .eq. iflag_once_fft) then
        tmpchara = at_once_ISPACK3_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_domain_once) then
        tmpchara = domain_ISPACK3_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_component_once) then
        tmpchara = comp_ISPACK3_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_single_fft) then
        tmpchara = single_ISPACK3_flags%flags(1)
      end if
      find_ISPACK3_label = tmpchara
!
      end function find_ISPACK3_label
!
! ----------------------------------------------------------------------
!
      subroutine check_ISPACK3_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara) :: title
!
      write(title,'(4x,a)') 'at_once_ISPACK3_flags'
      call write_multi_flags(id_file, title, at_once_ISPACK3_flags)
      write(title,'(4x,a)') 'domain_ISPACK3_flags'
      call write_multi_flags(id_file, title, domain_ISPACK3_flags)
      write(title,'(4x,a)') 'comp_ISPACK3_flags'
      call write_multi_flags(id_file, title, comp_ISPACK3_flags)
      write(title,'(4x,a)') 'single_ISPACK3_flags'
      call write_multi_flags(id_file, title, single_ISPACK3_flags)
!
      end subroutine check_ISPACK3_mode_flags
!
! ----------------------------------------------------------------------
!
      end module m_ISPACK3_labels
