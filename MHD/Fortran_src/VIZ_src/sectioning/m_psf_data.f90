!>@file   m_psf_data.f90
!!@brief  module m_psf_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!@endverbatim
!
      module m_psf_data
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
!>      Structure for isosurface mesh
!!        psf_mesh(i_psf)%node%numnod = 
!!        psf_mesh(i_psf)%node%istack_nod_smp(ip) = istack_nod_psf_smp
!!        psf_mesh(i_psf)%node%xx = xyz_psf
!!        psf_mesh(i_psf)%node%rr =    sph_psf(:,1)
!!        psf_mesh(i_psf)%node%theta = sph_psf(:,2)
!!        psf_mesh(i_psf)%node%phi =   sph_psf(:,3)
!!        psf_mesh(i_psf)%node%a_r =   sph_psf(:,4)
!!        psf_mesh(i_psf)%node%ss =    cyl_psf(:,1)
!!        psf_mesh(i_psf)%node%a_s =   cyl_psf(:,2)
!!
!!        psf_mesh(i_psf)%ele%nnod_4_ele = 3
!!        psf_mesh(i_psf)%ele%istack_ele_smp = istack_patch_psf_smp
!!        psf_mesh(i_psf)%ele%ie = ie_patch_psf
      type(mesh_geometry), allocatable, save :: psf_mesh(:)
!
!>      Structure for sectioned field
!!        psf_fld(i_psf)%num_phys =             num_psf_output(i_psf)
!!        psf_fld(i_psf)%ntot_phys =            num_psf_out_comp(i_psf)
!!        psf_fld(i_psf)%num_component(:) =     ncomp_psf_output
!!        psf_fld(i_psf)%istack_component(:) =
!!        psf_fld(i_psf)%phys_name(:) =         name_psf_output(:)
!!        psf_fld(i_psf)%d_fld(:) = 
      type(phys_data), allocatable, save :: psf_fld(:)
!
!>      Structure for table for sections
      type(sectiong_list), allocatable, save :: psf_list(:)
!
!>      Structure for search table for sections
      type(psf_search_lists), allocatable, save :: psf_search(:)
!
      type(psf_parameters), allocatable, save :: psf_param(:)
!
      type(psf_patch_data), save :: psf_pat
      type(psf_collect_type), save :: psf_col
!
!>      Structure for cross sectioning output (used by master process)
      type(ucd_data), allocatable, save :: psf_out(:)
!
!
      integer(kind = kint), allocatable :: istack_nod_psf(:)
      integer(kind = kint), allocatable :: istack_nod_psf_smp(:)
!
      integer(kind = kint), allocatable :: istack_patch_psf(:)
      integer(kind = kint), allocatable :: istack_patch_psf_smp(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_field_type(my_rank, num_psf)
!
      integer(kind = kint), intent(in) :: my_rank, num_psf
!
!
      allocate(psf_mesh(num_psf))
      allocate(psf_fld(num_psf))
      allocate(psf_list(num_psf))
      allocate(psf_search(num_psf))
      allocate(psf_param(num_psf))
!
      if(my_rank .eq. 0) then
        allocate( psf_out(num_psf) )
      else
        allocate( psf_out(0) )
      end if
!
      end subroutine alloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_field_type
!
!
      deallocate(psf_mesh, psf_fld, psf_list)
      deallocate(psf_search, psf_out, psf_param)
!
      end subroutine dealloc_psf_field_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_patch_psf(np_smp, num_psf)
!
      integer(kind= kint), intent(in) :: np_smp, num_psf
!
      allocate(istack_nod_psf(0:num_psf))
      allocate(istack_patch_psf(0:num_psf))
      allocate(istack_nod_psf_smp(0:np_smp*num_psf))
      allocate(istack_patch_psf_smp(0:np_smp*num_psf))
!
      istack_nod_psf = 0
      istack_patch_psf = 0
      istack_nod_psf_smp = 0
      istack_patch_psf_smp = 0
!
      end subroutine allocate_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_patch_psf
!
      deallocate(istack_nod_psf, istack_nod_psf_smp)
      deallocate(istack_patch_psf, istack_patch_psf_smp)
!
      end subroutine deallocate_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      end module m_psf_data
