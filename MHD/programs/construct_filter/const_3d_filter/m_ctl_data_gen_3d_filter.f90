!
!      module m_ctl_data_gen_3d_filter
!
      module m_ctl_data_gen_3d_filter
!
!      Written by H. Matsui on July, 2006
!
      use m_precision
      use m_ctl_data_4_solvers
      use m_ctl_data_gen_filter
!
      implicit  none
!
!
      integer(kind = kint), parameter :: filter_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_filter_ctl = "ctl_filter"
      character(len = kchara), parameter                                &
     &                        :: fname_sort_flt_ctl = "ctl_sort_filter"
!
      integer(kind = kint) :: num_filtering_grp_ctl = 0
      character(len = kchara), allocatable :: filter_area_name_ctl(:)
!
      character(len=kchara) :: mass_matrix_type_ctl =  'CONSIST'
      character(len=kchara) :: method_esize_ctl =      'GPBiCG'
      character(len=kchara) :: precond_esize_ctl =     'DIAG'
      integer (kind=kint) :: itr_esize_ctl =      20000
      real (kind=kreal) :: eps_esize_ctl =        1.0d-15
      real (kind=kreal) :: sigma_esize_ctl =      1.0d0
      real (kind=kreal) :: sigma_diag_esize_ctl = 1.0d0
!
!
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_control = 'const_filter'
      integer (kind=kint) :: i_filter_control = 0
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_area_ctl =  'filter_area_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_deltax_ctl =       'element_size_ctl'
      integer (kind=kint) :: i_filter_area_ctl =    0
      integer (kind=kint) :: i_deltax_ctl =         0
!
!     3rd level for filter_area
!
      character(len=kchara), parameter                                  &
     &         :: hd_num_filter_area = 'filter_ele_grp_ctl'
      integer (kind=kint) :: i_num_filter_area =  0
!
!     3rd level for mass matrix
!
      character(len=kchara), parameter                                  &
     &         :: hd_mass_matrix_type = 'mass_matrix_type_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_esize_solver = 'esize_solver_control'
      integer (kind=kint) :: i_mass_matrix_type =  0
      integer (kind=kint) :: i_esize_solver_ctl =  0
!
!     4th level for solver_control for element size
!
      character(len=kchara), parameter                                  &
     &         :: hd_method_esize =     'method_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_precond_esize =    'precond_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_itr_esize =        'itr_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_eps_esize =        'eps_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_sigma_esize =      'sigma_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_sigma_diag_esize = 'sigma_diag_ctl'
!
      integer (kind=kint) :: i_method_esize =       0
      integer (kind=kint) :: i_precond_esize =      0
      integer (kind=kint) :: i_itr_esize =          0
      integer (kind=kint) :: i_eps_esize =          0
      integer (kind=kint) :: i_sigma_esize =        0
      integer (kind=kint) :: i_sigma_diag_esize =   0
!
!
!      subroutine allocate_filtering_area_ctl
!
!      subroutine deallocate_filtering_area_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_filtering_area_ctl
!
      allocate(filter_area_name_ctl(num_filtering_grp_ctl))
!
      end subroutine allocate_filtering_area_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_filtering_area_ctl
!
      deallocate(filter_area_name_ctl)
!
      end subroutine deallocate_filtering_area_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_gen_3d_filter
