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
      use t_psf_patch_data
      use t_psf_outputs
      use t_ucd_data
!
      implicit none
!
!
!>      Number of isosurfaces
      integer(kind = kint) :: num_iso
!
!>      Structure for isosurface mesh
!!        iso_mesh(i_iso)%node%numnod = 
!!        iso_mesh(i_iso)%node%istack_nod_smp(ip) = istack_nod_iso_smp
!!        iso_mesh(i_iso)%node%xx = xyz_iso
!!        iso_mesh(i_iso)%node%rr =    sph_iso(:,1)
!!        iso_mesh(i_iso)%node%theta = sph_iso(:,2)
!!        iso_mesh(i_iso)%node%phi =   sph_iso(:,3)
!!        iso_mesh(i_iso)%node%a_r =   sph_iso(:,1)
!!        iso_mesh(i_iso)%node%ss =    cyl_iso(:,1)
!!        iso_mesh(i_iso)%node%a_s =   cyl_iso(:,2)
!!
!!        iso_mesh(i_iso)%ele%nnod_4_ele = 3
!!        iso_mesh(i_iso)%ele%istack_ele_smp = istack_patch_iso_smp
!!        iso_mesh(i_iso)%ele%ie = ie_patch_iso
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
      type(psf_parameters), allocatable, save :: iso_param(:)
!
!
      type(psf_patch_data), save :: iso_pat
      type(psf_collect_type), save :: iso_col
!
!>      Structure for isosurface output (used by master process)
      type(ucd_data), allocatable, save :: iso_out(:)
!
!
!>      End point of node list for each isosurfaces
      integer(kind = kint), allocatable :: istack_nod_iso(:)
      integer(kind = kint), allocatable :: istack_nod_iso_smp(:)
!
      integer(kind = kint), allocatable :: istack_patch_iso(:)
      integer(kind = kint), allocatable :: istack_patch_iso_smp(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iso_field_type(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      allocate(iso_mesh(num_iso))
      allocate(iso_fld(num_iso))
      allocate(iso_list(num_iso))
      allocate(iso_search(num_iso))
      allocate(iso_param(num_iso))
!
      if(my_rank .eq. 0) then
        allocate( iso_out(num_iso) )
      else
        allocate( iso_out(0) )
      end if
!
      end subroutine alloc_iso_field_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iso_field_type
!
!
      deallocate(iso_mesh, iso_fld, iso_list)
      deallocate( iso_search, iso_out, iso_param)
!
      end subroutine dealloc_iso_field_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_patch_iso(np_smp)
!
      integer(kind= kint), intent(in) :: np_smp
!
      allocate(istack_nod_iso(0:num_iso))
      allocate(istack_patch_iso(0:num_iso))
      allocate(istack_nod_iso_smp(0:np_smp*num_iso))
      allocate(istack_patch_iso_smp(0:np_smp*num_iso))
!
      istack_nod_iso = 0
      istack_patch_iso = 0
      istack_nod_iso_smp = 0
      istack_patch_iso_smp = 0
!
      end subroutine allocate_num_patch_iso
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_patch_iso
!
      deallocate(istack_nod_iso,   istack_nod_iso_smp)
      deallocate(istack_patch_iso, istack_patch_iso_smp)
!
      end subroutine deallocate_num_patch_iso
!
!  ---------------------------------------------------------------------
!
      end module m_iso_data
      