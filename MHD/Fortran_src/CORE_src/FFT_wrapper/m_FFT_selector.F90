!>@file   m_FFT_selector.F90
!!@brief  module m_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      integer(kind = kint) function                                   &
!!     &            set_fft_library_ctl(iflag_ctl, FFT_library_ctl)
!!        integer(kind = kint), intent(in) :: iflag_ctl
!!        character(len = kchara), intent(in) :: FFT_library_ctl
!!      subroutine write_elapsed_4_FFT(i_mode, etime_fft)
!!      character(len = kchara) function chosen_fft_name(i_mode)
!|
!!   ------------------------------------------------------------------
!!      FFT Package lists
!!
!|      FFTPACK:                 FFTPACK5.11d
!!      ISPACK:                  ISPACK Ver.1
!!      ISPACK3:                 ISPACK Ver.3
!!      FFTW,     FFTW3:         FFTW3
!!      OMP_FFTW, OMP_FFTW3:     FFTW3 with OopenMP parallelization
!!      rocFFT, rocFFT_complex:  AMD rocFFT
!!      rocFFT_real:             AMD rocFFT with real data only
!!      OpenMP_rocFFT:           AMD rocFFT with OpenMP offloading
!!   ------------------------------------------------------------------
!!      FFT size flags
!!
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!|
!!   ------------------------------------------------------------------
!!
!!       Current broken mode:
!!     FFT_library_ctl    'FFTW_COMPONENT'
!!     FFT_library_ctl    'FFTW_SINGLE'
!!     FFT_library_ctl    'FFTW_DOMAIN'
!!     FFT_library_ctl    'FFTPACK_COMPONENT'
!!     FFT_library_ctl    'FFTPACK_SINGLE'
!!     FFT_library_ctl    'ISPACK3_SINGLE'
!!     FFT_library_ctl    'ISPACK3_COMPONENT'
!!
!!@endverbatim
!!
      module m_FFT_selector
!
      use m_precision
      use m_constants
      use t_multi_flag_labels
!
      implicit none
!

!>      Character flag to sarch fastest FFT
      character(len = kchara), parameter                                &
     &          :: hd_search_fastest_fft = 'Search_fastest'
!
!>      Character flag to use test FFT
      character(len = kchara), parameter :: hd_FFT_TEST =  'TEST'
!
!>      Character flag to use FFTPACK
      character(len = kchara), parameter :: hd_FFTPACK =  'FFTPACK'
!>      Character flag to use FFTPACK5
      character(len = kchara), parameter :: hd_FFTPACK5 = 'FFTPACK5'
!
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW =     'FFTW'
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW3 =    'FFTW3'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_OMP_FFTW =  'OMP_FFTW'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_OMP_FFTW3 = 'OMP_FFTW3'
!
!>      Character flag to use ISPACK
      character(len = kchara), parameter :: hd_ISPACK =   'ISPACK'
!>      Character flag to use ISPACK
      character(len = kchara), parameter :: hd_ISPACK3 =  'ISPACK3'
!
!>      Character flag to use ISPACK
      character(len = kchara), parameter, private                       &
     &                              :: hd_rocFFT =     'rocFFT'
!>      Character flag to use ISPACK
      character(len = kchara), parameter, private                       &
     &                              :: hd_rocFFT_c2r = 'rocFFT_complex'
!>      Character flag to use ISPACK
      character(len = kchara), parameter, private                       &
     &                              :: hd_rocFFT_r2r = 'rocFFT_real'
!>      Character flag to use ISPACK
      character(len = kchara), parameter, private                       &
     &                              :: hd_OMP_rocFFT = 'OpenMP_rocFFT'
!
!>      Character flag for at once transeform
      character(len = kchara), parameter, private                       &
     &                              :: hd_at_once =       'once'
!>      Character flag for once transform over component
      character(len = kchara), parameter, private                       &
     &                              :: hd_once_for_comp = 'component'
!>      Character flag for once transform over domain
      character(len = kchara), parameter, private                       &
     &                              :: hd_once_for_mode = 'domain'
!>      Character flag single transform
      character(len = kchara), parameter, private                       &
     &                              :: hd_single_FFT =    'single'
!
!
!>      flag parts for FFTPACK
      character(len = kchara), parameter :: FFTPACK_names(2)            &
     &                               = (/'FFTPACK ', 'FFTPACK5'/)
!>      flag parts for FFTW3
      character(len = kchara), parameter :: FFTW_names(2)               &
     &                               = (/'FFTW ', 'FFTW3'/)
!>      flag parts for OpenMP
      character(len = kchara), parameter :: OpenMP_names(2)             &
     &                               = (/'OpenMP', 'OMP   '/)
!>      flag parts for ISPACK 0.97
      character(len = kchara), parameter :: ISPACK_names(3)             &
     &                     = (/'ISPACK   ', 'ISPACK1  ', 'ISPACK097'/)
!
!>      flag parts for once FFT over component
      character(len = kchara), parameter :: at_once_FFT_names(2)        &
     &                               = (/'once   ', 'at_once'/)
!>      flag parts for once FFT over component
      character(len = kchara), parameter :: comps_FFT_names(2)          &
     &                               = (/'component', 'comps    '/)
!>      flag parts for single FFT
      character(len = kchara), parameter :: single_FFT_names(2)         &
     &                               = (/'single', 'sgl   '/)
!
!>     Character lables for FFTPACK5: 'FFTPACK', 'FFTPACK5'
      type(multi_flag_labels), save :: FFTPACK_flags
!>     Character lables for FFTW3:    'FFTW',    'FFTW3'
      type(multi_flag_labels), save :: FFTW_flags
!>     Character lables for OpenMP FFTW3:    'FFTW',    'FFTW3'
      type(multi_flag_labels), save :: OMP_FFTW_flags
!>     Character lables for ISPACK 0.97: 'ISPACK', 'ISPACK097'
      type(multi_flag_labels), save :: ISPACK0_flags
!>     Character lables for ISPACK 3: 'ISPACK3'
      type(multi_flag_labels), save :: ISPACK3_flags
!
!>     Character lables for at once FFT:  'once'
      type(multi_flag_labels), save :: at_once_FFT_flags
!>     Character lables for once FFT over domain:  'domain'
      type(multi_flag_labels), save :: domain_FFT_flags
!>     Character lables for once FFT over component:
!!                                     'component',  'comps'
      type(multi_flag_labels), save :: comp_FFT_flags
!>     Character lables for single FFT:            'single',  'sgl'
      type(multi_flag_labels), save :: single_FFT_flags
!
!>     Character lables for at once FFTPACK5 for transform
!!        'FFTPACK', 'FFTPACK5', 'FFTPACK_once', 'FFTPACK_at_once', 
!!        'FFTPACK5_once', 'FFTPACK5_at_once', 'once_FFTPACK', 
!!        'once_FFTPACK5', 'at_once_FFTPACK', 'at_once_FFTPACK5' 
      type(multi_flag_labels), save :: at_once_FFTPACK_flags
!>     Character lables for once FFTPACK5 over domain
!!        'FFTPACK_domain', 'FFTPACK5_domain',
!!        'domain_FFTPACK', 'domain_FFTPACK5' 
      type(multi_flag_labels), save :: domain_FFTPACK_flags
!>     Character lables for once FFTPACK5 over component
!!        'FFTPACK_component',  'FFTPACK_comps', 'FFTPACK5_component',
!!        'FFTPACK5_comps', 'component_FFTPACK', 'component_FFTPACK5',
!!        'comps_FFTPACK',  'comps_FFTPACK5'
      type(multi_flag_labels), save :: comp_FFTPACK_flags
!>     Character lables for single FFTPACK5
!!        'FFTPACK_single',  'single_FFTPACK', 'FFTPACK5_single', 
!!        'single_FFTPACK5', 'FFTPACK5_sgl',   'sgl_FFTPACK5',
!!        'FFTPACK5_sgl',    'sgl_FFTPACK5'
      type(multi_flag_labels), save :: single_FFTPACK_flags
!
!
!>     Character lables for at once FFTW3 for transform
!!        'FFTW', 'FFTW3', 'FFTPACK_once', 'FFTPACK_at_once', 
!!        'FFTPACK5_once', 'FFTPACK5_at_once', 'once_FFTPACK', 
!!        'once_FFTPACK5', 'at_once_FFTPACK', 'at_once_FFTPACK5' 
      type(multi_flag_labels), save :: at_once_FFTW_flags
!>     Character lables for once FFTW3 over domain
!!         'FFTW_domain', 'FFTW3_domain', 'domain_FFTW', 'domain_FFTW3'
      type(multi_flag_labels), save :: domain_FFTW_flags
!>     Character lables for once FFTW3 over component
!!        'FFTW_component', 'FFTW_comps',     'FFTW3_component',
!!        'FFTW3_comps',    'component_FFTW', 'component_FFTW3',
!!        'comps_FFTW',     'comps_FFTW3' 
      type(multi_flag_labels), save :: comp_FFTW_flags
!>     Character lables for single FFTW3
!!        'FFTW_single', 'FFTW_sgl',     'FFTW3_single', 'FFTW3_sgl',
!!        'single_FFTW', 'single_FFTW3', 'sgl_FFTW',     'sgl_FFTW3' 
      type(multi_flag_labels), save :: single_FFTW_flags
!
!
!>     Character lables for at once ISPACKv0.97 for transform
!!        'ISPACK',          'ISPACK1',         'ISPACK097',
!!        'ISPACK_once',     'ISPACK_at_once',  'ISPACK1_once', 
!!        'ISPACK1_at_once', 'ISPACK097_once',  'ISPACK097_at_once',
!!        'once_ISPACK',     'once_ISPACK1',    'once_ISPACK097',
!!        'at_once_ISPACK',  'at_once_ISPACK1', 'at_once_ISPACK097' 
      type(multi_flag_labels), save :: at_once_ISPACK0_flags
!>     Character lables for once ISPACKv0.97 over domain
!!         'ISPACK_domain', 'ISPACK1_domain', 'ISPACK097_domain', 
!!         'domain_ISPACK', 'domain_ISPACK1', 'domain_ISPACK097' 
      type(multi_flag_labels), save :: domain_ISPACK0_flags
!>     Character lables for once ISPACKv0.97 over component
!!        'ISPACK_component', 'ISPACK_comps',      'ISPACK1_component',
!!        'ISPACK1_comps',    'ISPACK097_component', 'ISPACK097_comps',
!!        'component_ISPACK',    'component_ISPACK1', 
!!        'component_ISPACK097', 'comps_ISPACK',
!!        'comps_ISPACK1',       'comps_ISPACK097' 
      type(multi_flag_labels), save :: comp_ISPACK0_flags
!>     Character lables for single ISPACKv0.97
!!        'ISPACK_single', 'ISPACK_sgl',       'ISPACK1_single', 
!!        'ISPACK1_sgl',   'ISPACK097_single', 'ISPACK097_sgl', 
!!        'single_ISPACK', 'single_ISPACK1',   'single_ISPACK097',
!!        'sgl_ISPACK',    'sgl_ISPACK1',      'sgl_ISPACK097' 
      type(multi_flag_labels), save :: single_ISPACK0_flags
!
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
!
!>      Character flag to use single FFTPACK5
      character(len = kchara), parameter, private                       &
     &                            :: hd_FFTPACK_S = 'FFTPACK_SINGLE'
!>      Character flag to use FFTPACK5 for each component
      character(len = kchara), parameter, private                       &
     &                            :: hd_FFTPACK_C = 'FFTPACK_COMPONENT'
!>      Character flag to use FFTPACK5 for each domain
      character(len = kchara), parameter, private                       &
     &                            :: hd_FFTPACK_D = 'FFTPACK_DOMAIN'
!>      Character flag to use FFTPACK5 at once
      character(len = kchara), parameter, private                       &
     &                            :: hd_FFTPACK_O = 'FFTPACK_ONCE'
!
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_FFTW_D =  'FFTW_DOMAIN'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_FFTW3_D = 'fftw3_domain'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter :: hd_FFTW_S =  'FFTW_SINGLE'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter :: hd_FFTW3_S = 'fftw3_single'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter, private                       &
     &                               :: hd_FFTW_C =   'FFTW_COMPONENT'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter, private                       &
     &                               :: hd_FFTW3_C =  'fftw3_component'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter, private                       &
     &                               :: hd_FFTW_O =   'FFTW_ONCE'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter, private                       &
     &                               :: hd_FFTW3_O =  'FFTW3_ONCE'
!
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter                                &
     &                           :: hd_OMP_FFTW_D =  'OMP_FFTW_DOMAIN'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter                                &
     &                           :: hd_OMP_FFTW3_D = 'OMP_FFTW3_DOMAIN'
!
!>      Character flag to use ISPACK for domain
      character(len = kchara), parameter, private                       &
     &                               :: hd_ISPACK_D =  'ISPACK_DOMAIN'
!>      Character flag to use ISPACK at once
      character(len = kchara), parameter, private                       &
     &                               :: hd_ISPACK_O =  'ISPACK_ONCE'
!
!>      Character flag to use ISPACK for domain
      character(len = kchara), parameter, private                       &
     &                           :: hd_ISPACK3_D =  'ISPACK3_DOMAIN'
!>      Character flag to use ISPACK for component
      character(len = kchara), parameter, private                       &
     &                           :: hd_ISPACK3_C =  'ISPACK3_COMPONENT'
!>      Character flag to use single ISPACK
      character(len = kchara), parameter, private                       &
     &                           :: hd_ISPACK3_S =  'ISPACK3_SINGLE'
!>      Character flag to use ISPACK at once
      character(len = kchara), parameter, private                       &
     &                           :: hd_ISPACK3_O =  'ISPACK3_ONCE'
!
!!>      integer flag for undefined FFT routine
!      integer(kind = kint), parameter :: iflag_UNDEFINED_FFT =   -999
!>      integer flag for fastest FFT search
      integer(kind = kint), parameter :: iflag_SEARCH_FASTEST_FFT = -1
!
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK =      50
!>      integer flag to use FFTW3
      integer(kind = kint), parameter :: iflag_FFTW =         10
!>      integer flag to use FFTW3 with OopenMP
      integer(kind = kint), parameter :: iflag_OMP_FFTW =     40
!>      integer flag to use ISPACK Ver.0.93
      integer(kind = kint), parameter :: iflag_ISPACK1 =      20
!>      integer flag to use ISPACK Ver.3
      integer(kind = kint), parameter :: iflag_ISPACK3 =      30
!>      integer flag to use rocFFT
      integer(kind = kint), parameter :: iflag_rocFFT =       60
!>      integer flag to use rocFFT only with real value
      integer(kind = kint), parameter :: iflag_real_rocFFT =  70
!>      integer flag to use rocFFT with OopenMP
      integer(kind = kint), parameter :: iflag_OMP_rocFFT =   80
!
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_once_fft =       1
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_single_fft =     2
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_component_once = 3
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_domain_once =    4
!
!
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_ONCE =        1
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_SINGLE =      2
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_COMPONENT =   3
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_DOMAIN =      4
!
!>      integer flag to use FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_ONCE =          11
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_SINGLE =        12
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_COMPONENT =     13
!>      integer flag to use FFTW3 for each component
      integer(kind = kint), parameter :: iflag_FFTW_DOMAIN =        14
!
!>      integer flag to use FFTW3 with OopenMP at once
      integer(kind = kint), parameter :: iflag_OMP_FFTW_ONCE =      41
!>      integer flag to use FFTW3 with OopenMP for domain
      integer(kind = kint), parameter :: iflag_OMP_FFTW_DOMAIN =    42
!
!>      integer flag to use ISPACK Ver.0.93 at once
      integer(kind = kint), parameter :: iflag_ISPACK1_ONCE =       21
!>      integer flag to use ISPACK Ver.0.93 for domain
      integer(kind = kint), parameter :: iflag_ISPACK1_DOMAIN =     22
!
!>      integer flag to use ISPACK Ver. 3.01
      integer(kind = kint), parameter :: iflag_ISPACK3_ONCE =       31
!>      integer flag to use ISPACK Ver. 3.01 for domain
      integer(kind = kint), parameter :: iflag_ISPACK3_DOMAIN =     32
!>      integer flag to use ISPACK Ver. 3.01 for domain
      integer(kind = kint), parameter :: iflag_ISPACK3_COMPONENT =  33
!>      integer flag to use ISPACK Ver. 3.01 for domain
      integer(kind = kint), parameter :: iflag_ISPACK3_SINGLE =     34
!
!>      integer flag to use test FFT
      integer(kind = kint), parameter :: iflag_FFT_TEST =    99
!
      private :: hd_FFTPACK
      private :: hd_FFTW, hd_FFTW3, hd_FFTW_S, hd_FFTW3_S
      private :: hd_FFTW_D, hd_FFTW3_D
      private :: hd_ISPACK, hd_ISPACK3, hd_FFT_TEST
      private :: hd_OMP_FFTW,  hd_OMP_FFTW_D
      private :: hd_OMP_FFTW3, hd_OMP_FFTW3_D
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_FFT_mode_flags()
!
      type(multi_flag_labels) :: tmp_flags
      integer(kind = kint) :: icou
!
!
      call init_multi_flags_by_labels(itwo, FFTPACK_names,              &
     &                                FFTPACK_flags)
      call init_multi_flags_by_labels(itwo, FFTW_names,                 &
     &                                FFTW_flags)
      call init_multi_flags_by_labels(ithree, ISPACK_names,             &
     &                                ISPACK0_flags)
      call init_multi_flags_by_one_label(hd_ISPACK3, ISPACK3_flags)
!
      call init_multi_flags_by_labels(itwo, OpenMP_names, tmp_flags)
      call init_from_two_kinds_flags(tmp_flags, FFTW_flags,             &
     &                               OMP_FFTW_flags, icou)
      call dealloc_multi_flags(tmp_flags)
!
      call init_multi_flags_by_labels(itwo, at_once_FFT_names,          &
     &                                at_once_FFT_flags)
      call init_multi_flags_by_one_label(hd_once_for_mode,              &
     &                                   domain_FFT_flags)
      call init_multi_flags_by_labels(itwo, comps_FFT_names,            &
     &                                comp_FFT_flags)
      call init_multi_flags_by_labels(itwo, single_FFT_names,           &
     &                                single_FFT_flags)
!
      call init_multi_flags_by_labels(itwo, FFTPACK_names,              &
     &                                at_once_FFTPACK_flags)
      call init_from_two_kinds_flags(FFTPACK_flags, at_once_FFT_flags,  &
     &                               tmp_flags, icou)
      call append_multi_flag_labels(tmp_flags, at_once_FFTPACK_flags)
      call dealloc_multi_flags(tmp_flags)
!
      call init_from_two_kinds_flags(FFTPACK_flags, domain_FFT_flags,   &
     &                               domain_FFTPACK_flags, icou)
      call init_from_two_kinds_flags(FFTPACK_flags, comp_FFT_flags,     &
     &                               comp_FFTPACK_flags, icou)
      call init_from_two_kinds_flags(FFTPACK_flags, single_FFT_flags,   &
     &                               single_FFTPACK_flags, icou)
!
!
      call init_multi_flags_by_labels(itwo, FFTW_names,                 &
     &                                at_once_FFTW_flags)
      call init_from_two_kinds_flags(FFTW_flags, at_once_FFT_flags,     &
     &                               tmp_flags, icou)
      call append_multi_flag_labels(tmp_flags, at_once_FFTW_flags)
      call dealloc_multi_flags(tmp_flags)
!
      call init_from_two_kinds_flags(FFTW_flags, domain_FFT_flags,      &
     &                               domain_FFTW_flags, icou)
      call init_from_two_kinds_flags(FFTW_flags, comp_FFT_flags,        &
     &                               comp_FFTW_flags, icou)
      call init_from_two_kinds_flags(FFTW_flags, single_FFT_flags,      &
     &                               single_FFTW_flags, icou)
!
!
      call init_multi_flags_by_labels(itwo, FFTW_names,                 &
     &                                at_once_OMP_FFTW_flags)
      call init_from_two_kinds_flags(FFTW_flags, at_once_OMP_FFTW_flags,     &
     &                               tmp_flags, icou)
      call append_multi_flag_labels(tmp_flags, at_once_OMP_FFTW_flags)
      call dealloc_multi_flags(tmp_flags)
!
      call init_from_two_kinds_flags(OMP_FFTW_flags, domain_FFT_flags,  &
     &                               domain_OMP_FFTW_flags, icou)
      call init_from_two_kinds_flags(OMP_FFTW_flags, comp_FFT_flags,    &
     &                               comp_OMP_FFTW_flags, icou)
      call init_from_two_kinds_flags(OMP_FFTW_flags, single_FFT_flags,  &
     &                               single_OMP_FFTW_flags, icou)
!
!
      call init_multi_flags_by_labels(ithree, ISPACK_names,             &
     &                                at_once_ISPACK0_flags)
      call init_from_two_kinds_flags(ISPACK0_flags, at_once_FFT_flags,  &
     &                               tmp_flags, icou)
      call append_multi_flag_labels(tmp_flags, at_once_ISPACK0_flags)
      call dealloc_multi_flags(tmp_flags)
!
      call init_from_two_kinds_flags(ISPACK0_flags, domain_FFT_flags,   &
     &                               domain_ISPACK0_flags, icou)
      call init_from_two_kinds_flags(ISPACK0_flags, comp_FFT_flags,     &
     &                               comp_ISPACK0_flags, icou)
      call init_from_two_kinds_flags(ISPACK0_flags, single_FFT_flags,   &
     &                               single_ISPACK0_flags, icou)
!
!
      call init_multi_flags_by_one_label(hd_ISPACK3,                    &
     &                                   at_once_ISPACK3_flags)
      call init_from_two_kinds_flags(ISPACK3_flags, at_once_FFT_flags,  &
     &                               tmp_flags, icou)
      call append_multi_flag_labels(tmp_flags, at_once_ISPACK3_flags)
      call dealloc_multi_flags(tmp_flags)
!
      call init_from_two_kinds_flags(ISPACK3_flags, domain_FFT_flags,   &
     &                               domain_ISPACK3_flags, icou)
      call init_from_two_kinds_flags(ISPACK3_flags, comp_FFT_flags,     &
     &                               comp_ISPACK3_flags, icou)
      call init_from_two_kinds_flags(ISPACK3_flags, single_FFT_flags,   &
     &                               single_ISPACK3_flags, icou)
!
      end subroutine init_FFT_mode_flags

! ------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &            set_fft_library_ctl(iflag_ctl, FFT_library_ctl)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: iflag_ctl
      character(len = kchara), intent(in) :: FFT_library_ctl
      integer(kind = kint) :: iflag
!
!
#ifdef FFTW3
      iflag = iflag_FFTW_SINGLE
#else
      iflag = iflag_FFTPACK_ONCE
#endif
      if(iflag_ctl .eq. 0) then
        set_fft_library_ctl = iflag
        return
      end if
!
      if(cmp_no_case(FFT_library_ctl, hd_search_fastest_fft)) then
        iflag = iflag_SEARCH_FASTEST_FFT
!
      else if(cmp_no_case(FFT_library_ctl, hd_FFTPACK_O)) then
        iflag = iflag_FFTPACK_ONCE
      else if(cmp_no_case(FFT_library_ctl, hd_FFTPACK_C)) then
        iflag = iflag_FFTPACK_COMPONENT
      else if(cmp_no_case(FFT_library_ctl, hd_FFTPACK_D)) then
        iflag = iflag_FFTPACK_DOMAIN
!
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK_O)                 &
     &   .or. cmp_no_case(FFT_library_ctl, hd_ISPACK)) then
        iflag = iflag_ISPACK1_ONCE
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK_D)) then
        iflag = iflag_ISPACK1_DOMAIN
!
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK3_O)) then
        iflag = iflag_ISPACK3_ONCE
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK3_D)) then
        iflag = iflag_ISPACK3_DOMAIN
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK3_C)) then
        iflag = iflag_ISPACK3_COMPONENT
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK3_S)                &
     &   .or. cmp_no_case(FFT_library_ctl, hd_ISPACK3)) then
        iflag = iflag_ISPACK3_SINGLE
!
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_O)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_O)) then
        iflag = iflag_FFTW_ONCE
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_S)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_S)                &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3)) then
        iflag = iflag_FFTW_SINGLE
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_C)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_C)) then
        iflag = iflag_FFTW_COMPONENT
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_D)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_D)) then
        iflag = iflag_FFTW_DOMAIN
!
      else if(cmp_no_case(FFT_library_ctl, hd_OMP_FFTW)                 &
     &     .or. cmp_no_case(FFT_library_ctl, hd_OMP_FFTW3)) then
        iflag = iflag_OMP_FFTW_ONCE
!
      else if(cmp_no_case(FFT_library_ctl, hd_OMP_FFTW_D)               &
     &     .or. cmp_no_case(FFT_library_ctl, hd_OMP_FFTW3_D)) then
        iflag = iflag_OMP_FFTW_DOMAIN
!
      else if(cmp_no_case(FFT_library_ctl, hd_FFT_TEST)) then
        iflag = iflag_FFT_TEST
!
      else if(cmp_no_case(FFT_library_ctl, hd_FFTPACK_S)                &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTPACK)) then
        iflag = iflag_FFTPACK_SINGLE
      end if
      set_fft_library_ctl = iflag
!
      end function set_fft_library_ctl
!
! ------------------------------------------------------------------
!
      subroutine write_elapsed_4_FFT(i_mode, etime_fft)
!
      integer(kind = kint), intent(in) :: i_mode
      real(kind = kreal), intent(in) :: etime_fft
!
      if     (i_mode .eq. iflag_FFTPACK_ONCE) then
        write(*,*) 'elapsed by FFTPACK at once               (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTPACK_SINGLE) then
        write(*,*) 'elapsed by single FFTPACK                (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTPACK_COMPONENT) then
        write(*,*) 'elapsed by FFTPACK for all component     (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTPACK_DOMAIN) then
        write(*,*) 'elapsed by FFTPACK for domain            (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
!
      else if(i_mode .eq. iflag_FFTW_ONCE) then
        write(*,*) 'elapsed by FFTW3 for at once             (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_SINGLE) then
        write(*,*) 'elapsed by single FFTW3                  (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_COMPONENT) then
        write(*,*) 'elapsed by FFTW3 for all component       (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_DOMAIN) then
        write(*,*) 'elapsed by FFTW3 for domain              (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
!
      else if(i_mode .eq. iflag_OMP_FFTW_ONCE) then
        write(*,*) 'elapsed by FFTW3 with OpoenMP at once    (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_OMP_FFTW_DOMAIN) then
        write(*,*) 'elapsed by FFTW3 with OpoenMP for domain (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
!
      else if(i_mode .eq. iflag_ISPACK1_ONCE) then
        write(*,*) 'elapsed by ISPACK V0.93                  (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK1_DOMAIN) then
        write(*,*) 'elapsed by ISPACK V0.93 for domain       (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
!
      else if(i_mode .eq. iflag_ISPACK3_ONCE) then
        write(*,*) 'elapsed by ISPACK V3.0.1                 (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK3_DOMAIN) then
        write(*,*) 'elapsed by ISPACK V3.0.1 for domain      (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK3_COMPONENT) then
        write(*,*) 'elapsed by ISPACK V3.0.1 for component   (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK3_SINGLE) then
        write(*,*) 'elapsed by single ISPACK V3.0.1          (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      end if
!
      end subroutine write_elapsed_4_FFT
!
! ------------------------------------------------------------------
!
      character(len = kchara) function chosen_fft_name(i_mode)
!
      integer(kind = kint), intent(in) :: i_mode
!
      if     (i_mode .eq. iflag_FFTPACK_ONCE) then
        chosen_fft_name = hd_FFTPACK_O
      else if(i_mode .eq. iflag_FFTPACK_SINGLE) then
        chosen_fft_name = hd_FFTPACK
      else if(i_mode .eq. iflag_FFTPACK_COMPONENT) then
        chosen_fft_name = hd_FFTPACK_C
      else if(i_mode .eq. iflag_FFTPACK_DOMAIN) then
        chosen_fft_name = hd_FFTPACK_D
!
      else if(i_mode .eq. iflag_FFTW_SINGLE) then
        chosen_fft_name = hd_FFTW
      else if(i_mode .eq. iflag_FFTW_COMPONENT) then
        chosen_fft_name = hd_FFTW_C
      else if(i_mode .eq. iflag_FFTW_DOMAIN) then
        chosen_fft_name = hd_FFTW_D
      else if(i_mode .eq. iflag_FFTW_ONCE) then
        chosen_fft_name = hd_FFTW_O
!
      else if(i_mode .eq. iflag_OMP_FFTW_ONCE) then
        chosen_fft_name = hd_OMP_FFTW
      else if(i_mode .eq. iflag_OMP_FFTW_DOMAIN) then
        chosen_fft_name = hd_OMP_FFTW_D
!
      else if(i_mode .eq. iflag_ISPACK1_ONCE) then
        chosen_fft_name = hd_ISPACK
      else if(i_mode .eq. iflag_ISPACK1_DOMAIN) then
        chosen_fft_name = hd_ISPACK_D
!
      else if(i_mode .eq. iflag_ISPACK3_ONCE) then
        chosen_fft_name = hd_ISPACK3_O
      else if(i_mode .eq. iflag_ISPACK3_DOMAIN) then
        chosen_fft_name = hd_ISPACK3_D
      else if(i_mode .eq. iflag_ISPACK3_COMPONENT) then
        chosen_fft_name = hd_ISPACK3_C
      else if(i_mode .eq. iflag_ISPACK3_SINGLE) then
        chosen_fft_name = hd_ISPACK3
!
      else if(i_mode .eq. iflag_FFT_TEST) then
        chosen_fft_name = hd_FFT_TEST
      end if
!
      end function chosen_fft_name
!
! ------------------------------------------------------------------
!
      end module m_FFT_selector
