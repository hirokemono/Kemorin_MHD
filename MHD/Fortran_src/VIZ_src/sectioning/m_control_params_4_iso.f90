!
!      module m_control_params_4_iso
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine allocate_control_params_4_iso(num_iso)
!
      module m_control_params_4_iso
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint), parameter :: iflag_constant_iso = -1
      integer(kind = kint), parameter :: iflag_field_iso =     1
!
      integer(kind = kint), parameter :: iso_ctl_file_code = 11
!
      character(len = kchara), target, allocatable :: iso_header(:)
      integer(kind = kint), target, allocatable :: itype_iso_file(:)
!
!
      integer(kind = kint), allocatable :: id_isosurf_data(:)
      integer(kind = kint), allocatable :: id_isosurf_comp(:)
      real(kind=kreal), allocatable :: isosurf_value(:)
      real(kind=kreal), allocatable :: result_value_iso(:)
!
      integer(kind = kint), allocatable :: id_iso_result_type(:)
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_control_params_4_iso(num_iso)
!
      use m_field_file_format
!
      integer(kind= kint), intent(in) :: num_iso
!
!
      allocate(iso_header(num_iso))
      allocate(itype_iso_file(num_iso))
!
      allocate(id_isosurf_data(num_iso))
      allocate(id_isosurf_comp(num_iso))
      allocate(isosurf_value(num_iso))
      allocate(result_value_iso(num_iso))
      allocate(id_iso_result_type(num_iso))
!
!
      itype_iso_file = iflag_ucd
      id_iso_result_type = 0
!
      id_isosurf_data =  0
      id_isosurf_comp =  0
      isosurf_value =    0.0d0
      result_value_iso = 0.0d0
!
      end subroutine allocate_control_params_4_iso
!
!  ---------------------------------------------------------------------
!
      end module m_control_params_4_iso
