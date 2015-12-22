!>@file   m_psf_results.f90
!!@brief  module m_psf_results
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief set edge information for PSF results
!!
!!@verbatim
!!      subroutine load_psf_data_to_link_IO(istep, psf_ucd)
!!      subroutine load_psf_data(istep)
!!
!!      subroutine allocate_psf_field_data
!!      subroutine deallocate_psf_results
!!@endverbatim
!
      module m_psf_results
!
      use m_precision
      use m_field_file_format
!
      use t_geometry_data
      use t_phys_data
      use t_ucd_data
!
      implicit none
!
!
!>      structure for section data
      type(node_data), save :: psf_nod
!>      structure for section data
      type(element_data), save :: psf_ele
!
!>      structure for section data
      type(phys_data), save :: psf_phys
!
      character(len=kchara) :: psf_file_header
      integer(kind = kint) :: iflag_psf_fmt = iflag_udt
!
      real(kind = kreal), allocatable :: ave_psf(:), rms_psf(:)
      real(kind = kreal), allocatable :: xmin_psf(:), xmax_psf(:)
      real(kind = kreal), allocatable :: sdev_psf(:)
!
      character(len=kchara) :: flag_psf
      integer(kind = kint) :: iflag_psf
!
      private :: set_psf_udt_mesh, set_psf_udt_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine load_psf_data_to_link_IO(istep, psf_ucd)
!
      use set_ucd_data_to_type
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(inout) :: psf_ucd
!
!
      call load_psf_data(istep)
!
      call link_node_data_2_ucd(psf_nod, psf_ucd)
      call link_ele_data_2_ucd(psf_ele, psf_ucd)
      call link_field_data_to_ucd(psf_phys, psf_ucd)
      psf_ucd%ifmt_file = iflag_psf_fmt
!
      end subroutine load_psf_data_to_link_IO
!
!-----------------------------------------------------------------------
!
      subroutine load_psf_data(istep)
!
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep
!
      type(ucd_data) :: read_psf
!
!
      read_psf%file_prefix = psf_file_header
      read_psf%ifmt_file =   iflag_psf_fmt
!
      call sel_read_ucd_file(iminus, istep, ithree, read_psf)
!
      call set_psf_udt_mesh(read_psf)
      call set_psf_udt_data(read_psf)
      call allocate_psf_field_data
!
      end subroutine load_psf_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_field_data
!
!
      allocate ( xmin_psf(psf_phys%ntot_phys) )
      allocate ( xmax_psf(psf_phys%ntot_phys) )
      allocate ( ave_psf(psf_phys%ntot_phys) )
      allocate ( rms_psf(psf_phys%ntot_phys) )
      allocate ( sdev_psf(psf_phys%ntot_phys) )
!
      xmin_psf = 1.0d30
      xmax_psf = 0.0d0
      ave_psf =  0.0d0
      rms_psf =  0.0d0
      sdev_psf = 0.0d0
!
      end subroutine allocate_psf_field_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_psf_results
!
!
      call dealloc_phys_data_type(psf_phys)
      call dealloc_phys_name_type(psf_phys)
      call deallocate_ele_connect_type(psf_ele)
      call deallocate_node_geometry_type(psf_nod)
!
      deallocate ( xmin_psf )
      deallocate ( xmax_psf )
      deallocate ( ave_psf, rms_psf, sdev_psf )
!
      end subroutine deallocate_psf_results
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_psf_udt_mesh(ucd)
!
      use m_geometry_constants
      use m_phys_constants
      use cal_mesh_position
!
      type(ucd_data), intent(inout) :: ucd
!
!
      psf_nod%numnod =     int(ucd%nnod)
      psf_ele%numele =     int(ucd%nele)
      psf_ele%nnod_4_ele = num_triangle
      psf_phys%ntot_phys = ucd%ntot_comp
      call allocate_node_geometry_type(psf_nod)
      call allocate_ele_connect_type(psf_ele)
!
      psf_nod%inod_global(1:psf_nod%numnod)                             &
     &      = ucd%inod_global(1:psf_nod%numnod)
      psf_ele%iele_global(1:psf_ele%numele)                             &
     &      = ucd%iele_global(1:psf_ele%numele)
      psf_nod%xx(1:psf_nod%numnod,1:n_vector)                           &
     &      = ucd%xx(1:psf_nod%numnod,1:n_vector)
      psf_ele%ie(1:psf_ele%numele,1:ithree)                             &
     &      = int(ucd%ie(1:psf_ele%numele,1:ithree))
!
      call deallocate_ucd_node(ucd)
      call deallocate_ucd_ele(ucd)
!
      call set_spherical_position(psf_nod)
!
      end subroutine set_psf_udt_mesh
!
!-----------------------------------------------------------------------
!
      subroutine set_psf_udt_data(ucd)
!
      use set_ucd_data_to_type
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call alloc_phys_data_type_by_output(ucd, psf_nod, psf_phys)
!
      psf_phys%d_fld(1:psf_nod%numnod,1:psf_phys%ntot_phys)             &
     &   = ucd%d_ucd(1:psf_nod%numnod,1:psf_phys%ntot_phys)
!
      call deallocate_ucd_data(ucd)
!
      end subroutine set_psf_udt_data
!
!-----------------------------------------------------------------------
!
      end module  m_psf_results
