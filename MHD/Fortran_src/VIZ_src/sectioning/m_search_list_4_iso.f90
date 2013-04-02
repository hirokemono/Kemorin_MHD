!
!      module m_search_list_4_iso
!
      module m_search_list_4_iso
!
!      Written by H. Matsui on June, 2006
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: nele_search_iso_tot
      integer(kind = kint), allocatable :: istack_ele_search_iso_s(:)
      integer(kind = kint), allocatable :: iele_search_iso(:)
!
!
      integer(kind = kint) :: nsurf_search_iso_tot
      integer(kind = kint), allocatable :: istack_surf_search_iso_s(:)
      integer(kind = kint), allocatable :: isurf_search_iso(:)
!
!
      integer(kind = kint) :: nedge_search_iso_tot
      integer(kind = kint), allocatable :: istack_edge_search_iso_s(:)
      integer(kind = kint), allocatable :: iedge_search_iso(:)
!
!
      integer(kind = kint) :: nnod_search_iso_tot
      integer(kind = kint), allocatable :: istack_nod_search_iso_s(:)
      integer(kind = kint), allocatable :: inod_search_iso(:)
!
!
      integer(kind = kint), allocatable :: istack_nod_iso_on_e(:)
      integer(kind = kint), allocatable :: istack_edge_iso_on_e(:)
      integer(kind = kint), allocatable :: nwork_n_iso_on_e(:)
      integer(kind = kint), allocatable :: nwork_e_iso_on_e(:)
!
!
!      subroutine allocate_element_num_4_iso(np_smp, num_iso)
!      subroutine allocate_surf_num_4_iso(np_smp, num_iso)
!      subroutine allocate_edge_num_4_iso(np_smp, num_psf)
!      subroutine allocate_node_num_4_iso(np_smp, num_iso)
!
!      subroutine allocate_element_list_4_iso
!      subroutine allocate_surface_list_4_iso
!      subroutine allocate_edge_list_4_iso
!      subroutine allocate_node_list_4_iso
!
!      subroutine allocate_num_n_e_in_ele_iso
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_element_num_4_iso(np_smp, num_iso)
!
      integer(kind = kint), intent(in) :: np_smp, num_iso
!
      allocate( istack_ele_search_iso_s(0:num_iso*np_smp) )
      istack_ele_search_iso_s = 0
!
      end subroutine allocate_element_num_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surf_num_4_iso(np_smp, num_iso)
!
      integer(kind = kint), intent(in) :: np_smp, num_iso
!
      allocate( istack_surf_search_iso_s(0:num_iso*np_smp) )
      istack_surf_search_iso_s = 0
!
      end subroutine allocate_surf_num_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_num_4_iso(np_smp, num_iso)
!
      integer(kind = kint), intent(in) :: np_smp, num_iso
!
      allocate( istack_edge_search_iso_s(0:num_iso*np_smp) )
      istack_edge_search_iso_s = 0
!
      end subroutine allocate_edge_num_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_node_num_4_iso(np_smp, num_iso)
!
      integer(kind = kint), intent(in) :: np_smp, num_iso
!
      allocate( istack_nod_search_iso_s(0:num_iso*np_smp) )
      istack_nod_search_iso_s = 0
!
      end subroutine allocate_node_num_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_element_list_4_iso
!
      allocate( iele_search_iso(nele_search_iso_tot) )
      iele_search_iso = 0
!
      end subroutine allocate_element_list_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surface_list_4_iso
!
      allocate( isurf_search_iso(nsurf_search_iso_tot) )
      isurf_search_iso = 0
!
      end subroutine allocate_surface_list_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_list_4_iso
!
      allocate( iedge_search_iso(nedge_search_iso_tot) )
      iedge_search_iso = 0
!
      end subroutine allocate_edge_list_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_node_list_4_iso
!
      allocate( inod_search_iso(nnod_search_iso_tot) )
      inod_search_iso = 0
!
      end subroutine allocate_node_list_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_n_e_in_ele_iso
!
      allocate( istack_nod_iso_on_e(nele_search_iso_tot) )
      allocate( istack_edge_iso_on_e(nele_search_iso_tot) )
      allocate( nwork_n_iso_on_e(nele_search_iso_tot) )
      allocate( nwork_e_iso_on_e(nele_search_iso_tot) )
      istack_nod_iso_on_e = 0
      istack_edge_iso_on_e = 0
      nwork_n_iso_on_e = 0
      nwork_e_iso_on_e = 0
!
      end subroutine allocate_num_n_e_in_ele_iso
!
!  ---------------------------------------------------------------------
!
      end module m_search_list_4_iso
