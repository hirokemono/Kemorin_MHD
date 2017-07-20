!>@file   t_psf_results.f90
!!@brief  module t_psf_results
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief set edge information for PSF results
!!
!!@verbatim
!!      subroutine load_psf_data_to_link_IO                             &
!!     &         (istep, psf_file_param, psf, psf_ucd)
!!      subroutine load_psf_data(istep, psf_file_param, psf)
!!
!!      subroutine dealloc_psf_results(psf_nod, psf_ele, psf_phys)
!!@endverbatim
!
      module t_psf_results
!
      use m_precision
      use m_field_file_format
!
      use t_geometry_data
      use t_phys_data
      use t_ucd_data
      use t_file_IO_parameter
!
      implicit none
!
!
!>       structure for section data
      type psf_results
!>       structure for sectioned nodes
        type(node_data) :: psf_nod
!>       structure for sectioned element
        type(element_data) :: psf_ele
!>       structure for sectioned field
        type(phys_data) :: psf_phys
      end type psf_results
!
      private :: set_psf_udt_mesh, set_psf_udt_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine load_psf_data_to_link_IO                               &
     &         (istep, psf_file_param, psf, psf_ucd)
!
      use set_ucd_data_to_type
!
      integer(kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: psf_file_param
      type(psf_results), intent(inout) :: psf
      type(ucd_data), intent(inout) :: psf_ucd
!
!
      call load_psf_data(istep, psf_file_param, psf)
!
      call link_node_data_2_ucd(psf%psf_nod, psf_ucd)
      call link_ele_data_2_ucd(psf%psf_ele, psf_ucd)
      call link_field_data_to_ucd(psf%psf_phys, psf_ucd)
!
      end subroutine load_psf_data_to_link_IO
!
!-----------------------------------------------------------------------
!
      subroutine load_psf_data(istep, psf_file_param, psf)
!
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: psf_file_param
      type(psf_results), intent(inout) :: psf
!
      type(ucd_data) :: read_psf
!
!
      call sel_read_ucd_file                                            &
     &   (iminus, istep, ithree, psf_file_param, read_psf)
!
      call set_psf_udt_mesh                                             &
     &   (read_psf, psf%psf_nod, psf%psf_ele, psf%psf_phys)
      call set_psf_udt_data(read_psf, psf%psf_nod, psf%psf_phys)
!
      end subroutine load_psf_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_psf_results(psf)
!
      type(psf_results), intent(inout) :: psf
!
!
      call dealloc_phys_data_type(psf%psf_phys)
      call dealloc_phys_name_type(psf%psf_phys)
      call deallocate_ele_connect_type(psf%psf_ele)
      call deallocate_node_geometry_type(psf%psf_nod)
!
      end subroutine dealloc_psf_results
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_psf_udt_mesh(ucd, psf_nod, psf_ele, psf_phys)
!
      use m_geometry_constants
      use m_phys_constants
      use cal_mesh_position
!
      type(ucd_data), intent(inout) :: ucd
      type(node_data), intent(inout) :: psf_nod
      type(element_data), intent(inout) :: psf_ele
      type(phys_data), intent(inout) :: psf_phys
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
      subroutine set_psf_udt_data(ucd, psf_nod, psf_phys)
!
      use set_ucd_data_to_type
!
      type(ucd_data), intent(inout) :: ucd
      type(node_data), intent(in) :: psf_nod
      type(phys_data), intent(inout) :: psf_phys
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
      end module  t_psf_results
