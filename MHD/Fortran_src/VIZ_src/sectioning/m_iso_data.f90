!>@file   m_iso_data.f90
!!@brief  module m_iso_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@endverbatim
!!@verbatim
!
      module m_iso_data
!
      use m_precision
      use t_mesh_data
      use t_phys_data
      use t_psf_geometry_list
!
      implicit none
!
!
!>      Structure for isosurface mesh
!!        iso_mesh(i_iso)%node%numnod = 
!!        iso_mesh(i_iso)%node%istack_nod_smp(ip) = istack_nod_psf_smp
!!        iso_mesh(i_iso)%node%xx = xyz_psf
!!        iso_mesh(i_iso)%node%rr =    sph_psf(:,1)
!!        iso_mesh(i_iso)%node%theta = sph_psf(:,2)
!!        iso_mesh(i_iso)%node%phi =   sph_psf(:,3)
!!        iso_mesh(i_iso)%node%a_r =   sph_psf(:,1)
!!        iso_mesh(i_iso)%node%ss =    cyl_psf(:,1)
!!        iso_mesh(i_iso)%node%a_s =   cyl_psf(:,2)
!!
!!        iso_mesh(i_iso)%ele%nnod_4_ele = 3
!!        iso_mesh(i_iso)%ele%istack_ele_smp = istack_patch_psf_smp
!!        iso_mesh(i_iso)%ele%ie = ie_patch_psf
      type(mesh_geometry), allocatable, save :: iso_mesh(:)
!
!>      Structure for isosurface field
!!        iso_fld(i_iso)%num_phys =             num_iso_output(i_iso)
!!        iso_fld(i_iso)%ntot_phys =            num_iso_out_comp(i_iso)
!!        iso_fld(i_iso)%num_component(:) =     ncomp_iso_output
!!        iso_fld(i_iso)%istack_component(:) =
!!        iso_fld(i_iso)%phys_name(:) =         name_iso_output(:)
!!        iso_fld(i_iso)%d_fld(:) = 
      type(phys_data), allocatable, save :: iso_fld(:)
!
!>      Structure for table for sections
      type(sectiong_list), allocatable, save :: iso_list(:)
!
!>      Structure for search table for sections
      type(psf_search_lists), allocatable, save :: iso_search(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iso_field_type(num_iso)
!
      integer(kind = kint), intent(in) :: num_iso
!
!
      allocate(iso_mesh(num_iso))
      allocate(iso_fld(num_iso))
      allocate(iso_list(num_iso))
      allocate(iso_search(num_iso))
!
      end subroutine alloc_iso_field_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iso_field_type
!
!
      deallocate(iso_mesh, iso_fld, iso_list, iso_search)
!
      end subroutine dealloc_iso_field_type
!
!  ---------------------------------------------------------------------
!
      end module m_iso_data
      