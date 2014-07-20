!>@file   t_psf_geometry_list.f90
!!@brief  module t_psf_geometry_list
!!
!!@author H. Matsui
!!@date Programmed in July, 2014
!
!>@brief Structure for cross sectioning data
!!
!!@verbatim
!!      subroutine alloc_num_psf_search_list(np_smp, list)
!!      subroutine alloc_psf_search_list(list)
!!      subroutine alloc_mark_ele_psf(search)
!!      subroutine dealloc_num_psf_search_list(list)
!!      subroutine dealloc_psf_search_list(list)
!!      subroutine dealloc_mark_ele_psf(search)
!!
!!      subroutine alloc_ref_field_4_psf(numnod, psf_list)
!!      subroutine alloc_nnod_psf(np_smp, numnod, numedge, psf_list)
!!      subroutine alloc_inod_psf(psf_list)
!!      subroutine dealloc_ref_field_4_psf(psf_list)
!!      subroutine dealloc_nnod_psf(psf_list)
!!      subroutine dealloc_inod_psf(psf_list)
!!@endverbatim
!
      module t_psf_geometry_list
!
      use m_precision
!
      implicit none
!
!
!>      Structure for search list for surfacing
      type sect_search_list
!>        Number of node/edge/surface/element for searching
        integer(kind = kint) :: num_search
!>        SMP stack for searching
        integer(kind = kint), pointer :: istack_search_smp(:)
!>        Local node/edge/surface/element ID for searching
        integer(kind = kint), pointer :: id_search(:)
      end type sect_search_list
!
!>      Structure for search list for sectioning
      type psf_search_lists
!>        Structure for search list for element sectioning
        type(sect_search_list) :: elem_list
!>        Structure for search list for surface sectioning
        type(sect_search_list) :: surf_list
!>        Structure for search list for edge sectioning
        type(sect_search_list) :: edge_list
!>        Structure for search list for node sectioning
        type(sect_search_list) :: node_list
!
!>        marking for element generation
        integer(kind = kint), pointer :: mark_e(:)
      end type psf_search_lists
!
!
!>      Structure for cross sectioning list
      type sectiong_list
!
!>        reference field for sectioning
        real(kind = kreal), pointer :: ref_fld(:)
!
!>        Number of nodes for sections on node
        integer(kind = kint) :: nnod_on_nod
!>        SMP stack for sections on node
        integer(kind = kint), pointer :: istack_n_on_n_smp(:)
!
!>        Node ID for sections on node
        integer(kind = kint), pointer :: inod_4_nod(:)
!>        ID for node on node
        integer(kind = kint), pointer :: id_n_on_n(:)
!
!
!>        Number of nodes for sections on edge
        integer(kind = kint) :: nnod_on_edge
!>        SMP stack for sections on edge
        integer(kind = kint), pointer :: istack_n_on_e_smp(:)
!
!>        Edge ID for sections on node
        integer(kind = kint), pointer :: iedge_4_nod(:)
!>        ID for node on edge
        integer(kind = kint), pointer :: id_n_on_e(:)
!
!>        Interpolation coefficients for node on edge
        real(kind = kreal), pointer :: coef_on_edge(:,:)
      end type sectiong_list
!
!
      type psf_patch_data
        integer(kind = kint) :: nnod_psf_tot
        integer(kind = kint), pointer :: istack_nod_psf(:)
        integer(kind = kint), pointer :: istack_nod_psf_smp(:)
!
        integer(kind = kint), pointer :: inod_hash_psf(:)
        real(kind = kreal), pointer :: xyz_psf(:,:)
!
        real(kind = kreal), pointer :: rr(:)
        real(kind = kreal), pointer :: theta(:)
        real(kind = kreal), pointer :: phi(:)
        real(kind = kreal), pointer :: ar(:)
        real(kind = kreal), pointer :: ss(:)
        real(kind = kreal), pointer :: as(:)
!
!
        integer(kind = kint) :: npatch_tot
        integer(kind = kint), pointer :: istack_patch_psf(:)
        integer(kind = kint), pointer :: istack_patch_psf_smp(:)
        integer(kind = kint), pointer :: ie_tri(:,:)
!
!
        real(kind = kreal), pointer :: dat_psf(:,:)
!
        real(kind = kreal), pointer :: tmp_psf(:,:)
      end type psf_patch_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_psf_search_list(np_smp, list)
!
      integer(kind = kint), intent(in) :: np_smp
      type(sect_search_list), intent(inout) :: list
!
!
      allocate( list%istack_search_smp(0:np_smp) )
      list%istack_search_smp = 0
!
      end subroutine alloc_num_psf_search_list
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_search_list(list)
!
      type(sect_search_list), intent(inout) :: list
!
!
      allocate( list%id_search(list%num_search) )
      if(list%num_search .gt. 0) list%id_search = 0
!
      end subroutine alloc_psf_search_list
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_mark_ele_psf(search)
!
      type(psf_search_lists), intent(inout) :: search
!
!
      allocate( search%mark_e(search%elem_list%num_search) )
      if(search%elem_list%num_search .gt. 0) search%mark_e = 0
!
      end subroutine alloc_mark_ele_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_psf_search_list(list)
!
      type(sect_search_list), intent(inout) :: list
!
!
      deallocate(list%istack_search_smp)
!
      end subroutine dealloc_num_psf_search_list
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_search_list(list)
!
      type(sect_search_list), intent(inout) :: list
!
!
      deallocate(list%id_search)
!
      end subroutine dealloc_psf_search_list
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_mark_ele_psf(search)
!
      type(psf_search_lists), intent(inout) :: search
!
!
      deallocate(search%mark_e)
!
      end subroutine dealloc_mark_ele_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_ref_field_4_psf(numnod, psf_list)
!
      integer(kind= kint), intent(in) :: numnod
      type(sectiong_list), intent(inout) :: psf_list
!
!
      allocate(psf_list%ref_fld(numnod) )
      if(numnod .gt. 0) psf_list%ref_fld = 0.0d0
!
      end subroutine alloc_ref_field_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_nnod_psf(np_smp, numnod, numedge, psf_list)
!
      integer(kind= kint), intent(in) :: np_smp, numnod, numedge
      type(sectiong_list), intent(inout) :: psf_list
!
!
      allocate(psf_list%istack_n_on_n_smp(0:np_smp))
      allocate(psf_list%istack_n_on_e_smp(0:np_smp))
      allocate(psf_list%id_n_on_n(numnod))
      allocate(psf_list%id_n_on_e(numedge))
!
      psf_list%istack_n_on_n_smp = 0
      psf_list%istack_n_on_e_smp = 0
      if(numnod .gt. 0) then
        psf_list%id_n_on_n = 0
      end if
      if(numedge .gt. 0) then
        psf_list%id_n_on_e = 0
      end if
!
      end subroutine alloc_nnod_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_inod_psf(psf_list)
!
      type(sectiong_list), intent(inout) :: psf_list
!
!
      allocate(psf_list%inod_4_nod(psf_list%nnod_on_nod))
!
      allocate(psf_list%iedge_4_nod(psf_list%nnod_on_edge))
      allocate(psf_list%coef_on_edge(psf_list%nnod_on_edge,2))
!
      if(psf_list%nnod_on_nod .gt. 0) then
        psf_list%inod_4_nod = 0
      end if
!
      if(psf_list%nnod_on_edge .gt. 0) then
        psf_list%iedge_4_nod = 0
        psf_list%coef_on_edge = 0.0d0
      end if
!
      end subroutine alloc_inod_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ref_field_4_psf(psf_list)
!
      type(sectiong_list), intent(inout) :: psf_list
!
!
      deallocate(psf_list%ref_fld)
!
      end subroutine dealloc_ref_field_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_nnod_psf(psf_list)
!
      type(sectiong_list), intent(inout) :: psf_list
!
!
      deallocate(psf_list%istack_n_on_n_smp)
      deallocate(psf_list%istack_n_on_e_smp)
      deallocate(psf_list%id_n_on_n)
      deallocate(psf_list%id_n_on_e)
!
      end subroutine dealloc_nnod_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_inod_psf(psf_list)
!
      type(sectiong_list), intent(inout) :: psf_list
!
!
      deallocate(psf_list%inod_4_nod)
      deallocate(psf_list%iedge_4_nod, psf_list%coef_on_edge)
!
      end subroutine dealloc_inod_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_patch_psf(np_smp, num_psf, pat)
!
      integer(kind= kint), intent(in) :: np_smp, num_psf
      type(psf_patch_data), intent(inout) :: pat
!
      allocate(pat%istack_nod_psf(0:num_psf))
      allocate(pat%istack_patch_psf(0:num_psf))
      allocate(pat%istack_nod_psf_smp(0:np_smp*num_psf))
      allocate(pat%istack_patch_psf_smp(0:np_smp*num_psf))
!
      pat%istack_nod_psf = 0
      pat%istack_patch_psf = 0
      pat%istack_nod_psf_smp = 0
      pat%istack_patch_psf_smp = 0
!
      end subroutine alloc_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_position_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      allocate(pat%inod_hash_psf(pat%nnod_psf_tot))
      allocate(pat%xyz_psf(pat%nnod_psf_tot,3))
      allocate(pat%rr(pat%nnod_psf_tot))
      allocate(pat%theta(pat%nnod_psf_tot))
      allocate(pat%phi(pat%nnod_psf_tot))
      allocate(pat%ar(pat%nnod_psf_tot))
      allocate(pat%ss(pat%nnod_psf_tot))
      allocate(pat%as(pat%nnod_psf_tot))
!
      if(pat%nnod_psf_tot .gt. 0) then
        pat%inod_hash_psf = 0
        pat%xyz_psf = 0.0d0
        pat%rr =    0.0d0
        pat%theta = 0.0d0
        pat%phi =   0.0d0
        pat%ar =    0.0d0
        pat%ss =    0.0d0
        pat%as =    0.0d0
      end if
!
      end subroutine alloc_position_psf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dat_on_patch_psf(max_ncomp_psf, pat)
!
      integer(kind = kint), intent(in) :: max_ncomp_psf
      type(psf_patch_data), intent(inout) :: pat
!
      allocate(pat%dat_psf(pat%nnod_psf_tot,max_ncomp_psf))
      allocate(pat%tmp_psf(pat%nnod_psf_tot,6))
!
      if(pat%nnod_psf_tot .gt. 0) then
        pat%dat_psf = 0.0d0
        pat%tmp_psf = 0.0d0
      end if
!
      end subroutine alloc_dat_on_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_patch_data_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      allocate(pat%ie_tri(pat%npatch_tot,3))
      if(pat%npatch_tot .gt. 0) pat%ie_tri = 0
!
      end subroutine alloc_patch_data_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_patch_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      deallocate(pat%istack_nod_psf, pat%istack_nod_psf_smp)
      deallocate(pat%istack_patch_psf, pat%istack_patch_psf_smp)
!
      end subroutine dealloc_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_position_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      deallocate(pat%xyz_psf, pat%inod_hash_psf)
      deallocate(pat%rr, pat%theta, pat%phi)
      deallocate(pat%ar, pat%ss, pat%as)
!
      end subroutine dealloc_position_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_dat_on_patch_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      deallocate(pat%dat_psf, pat%tmp_psf)
!
      end subroutine dealloc_dat_on_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_patch_data_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      deallocate(pat%ie_tri)
!
      end subroutine dealloc_patch_data_psf
!
!  ---------------------------------------------------------------------
!
      end module t_psf_geometry_list
