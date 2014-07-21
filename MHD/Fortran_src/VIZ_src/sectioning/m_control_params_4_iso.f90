!
!      module m_control_params_4_iso
!
!        programmed by H.Matsui on May. 2006
!
      module m_control_params_4_iso
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint), parameter :: iso_ctl_file_code = 11
!
      integer(kind = kint) :: num_iso
      character(len = kchara), target, allocatable :: iso_header(:)
!
!
      integer(kind = kint), allocatable :: nele_grp_area_iso(:)
      integer(kind = kint), allocatable :: istack_grp_area_iso(:)
      integer(kind = kint) :: nele_grp_area_iso_tot
      integer(kind = kint), allocatable :: id_ele_grp_area_iso(:)
!
      integer(kind = kint), target, allocatable :: itype_iso_file(:)
!
      integer(kind = kint), allocatable :: id_isosurf_data(:)
      integer(kind = kint), allocatable :: id_isosurf_comp(:)
      real(kind=kreal), allocatable :: isosurf_value(:)
      real(kind=kreal), allocatable :: result_value_iso(:)
!
      integer(kind = kint), allocatable :: id_iso_result_type(:)
!
      integer(kind = kint) :: num_iso_total_out
      integer(kind = kint) :: max_ncomp_iso_out
      integer(kind = kint), allocatable, target :: num_iso_output(:)
      integer(kind = kint), allocatable, target :: istack_iso_output(:)
      integer(kind = kint), allocatable :: id_iso_output(:)
      integer(kind = kint), allocatable :: icomp_iso_output(:)
      integer(kind = kint), allocatable :: ncomp_iso_org(:)
!  number and stack of component for each surfaces
!
      integer(kind = kint), parameter :: iflag_constant_iso = -1
      integer(kind = kint), parameter :: iflag_field_iso =     1
!
!      subroutine allocate_control_params_4_iso
!      subroutine allocate_output_comps_4_iso
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_control_params_4_iso
!
      use m_field_file_format
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
      allocate(num_iso_output(num_iso))
      allocate(istack_iso_output(0:num_iso))
!
      allocate(nele_grp_area_iso(num_iso))
      allocate(istack_grp_area_iso(0:num_iso))
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
      num_iso_output =    0
      istack_iso_output = 0
!
      nele_grp_area_iso =   0
      istack_grp_area_iso = 0
!
      end subroutine allocate_control_params_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_output_comps_4_iso
!
      num_iso_total_out = istack_iso_output(num_iso)
      nele_grp_area_iso_tot = istack_grp_area_iso(num_iso)
!
      allocate(id_iso_output(num_iso_total_out))
      allocate(icomp_iso_output(num_iso_total_out))
      allocate(ncomp_iso_org(num_iso_total_out))
!
      allocate(id_ele_grp_area_iso(nele_grp_area_iso_tot))
!
      id_iso_output =    0
      icomp_iso_output = 0
      ncomp_iso_org =    0
!
      id_ele_grp_area_iso = 0
!
      end subroutine allocate_output_comps_4_iso
!
!  ---------------------------------------------------------------------
!
      end module m_control_params_4_iso
