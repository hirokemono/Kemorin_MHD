!
!      module m_search_list_4_psf
!
      module m_search_list_4_psf
!
!      Written by H. Matsui on June, 2006
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: nele_search_psf_tot
      integer(kind = kint), allocatable :: istack_ele_search_psf_s(:)
      integer(kind = kint), allocatable :: iele_search_psf(:)
!
!
      integer(kind = kint) :: nsurf_search_psf_tot
      integer(kind = kint), allocatable :: istack_surf_search_psf_s(:)
      integer(kind = kint), allocatable :: isurf_search_psf(:)
!
!
      integer(kind = kint) :: nedge_search_psf_tot
      integer(kind = kint), allocatable :: istack_edge_search_psf_s(:)
      integer(kind = kint), allocatable :: iedge_search_psf(:)
!
!
      integer(kind = kint) :: nnod_search_psf_tot
      integer(kind = kint), allocatable :: istack_nod_search_psf_s(:)
      integer(kind = kint), allocatable :: inod_search_psf(:)
!
!
!
!      subroutine allocate_element_num_4_psf(np_smp, num_psf)
!      subroutine allocate_surf_num_4_psf(np_smp, num_psf)
!      subroutine allocate_edge_num_4_psf(np_smp, num_psf)
!      subroutine allocate_node_num_4_psf(np_smp, num_psf)
!
!      subroutine allocate_element_list_4_psf
!      subroutine allocate_surface_list_4_psf
!      subroutine allocate_edge_list_4_psf
!      subroutine allocate_node_list_4_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_element_num_4_psf(np_smp, num_psf)
!
      integer(kind = kint), intent(in) :: np_smp, num_psf
!
      allocate( istack_ele_search_psf_s(0:num_psf*np_smp) )
      istack_ele_search_psf_s = 0
!
      end subroutine allocate_element_num_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surf_num_4_psf(np_smp, num_psf)
!
      integer(kind = kint), intent(in) :: np_smp, num_psf
!
      allocate( istack_surf_search_psf_s(0:num_psf*np_smp) )
      istack_surf_search_psf_s = 0
!
      end subroutine allocate_surf_num_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_num_4_psf(np_smp, num_psf)
!
      integer(kind = kint), intent(in) :: np_smp, num_psf
!
      allocate( istack_edge_search_psf_s(0:num_psf*np_smp) )
      istack_edge_search_psf_s = 0
!
      end subroutine allocate_edge_num_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_node_num_4_psf(np_smp, num_psf)
!
      integer(kind = kint), intent(in) :: np_smp, num_psf
!
      allocate( istack_nod_search_psf_s(0:num_psf*np_smp) )
      istack_nod_search_psf_s = 0
!
      end subroutine allocate_node_num_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_element_list_4_psf
!
      allocate( iele_search_psf(nele_search_psf_tot) )
      iele_search_psf = 0
!
      end subroutine allocate_element_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surface_list_4_psf
!
      allocate( isurf_search_psf(nsurf_search_psf_tot) )
      isurf_search_psf = 0
!
      end subroutine allocate_surface_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_list_4_psf
!
      allocate( iedge_search_psf(nedge_search_psf_tot) )
      iedge_search_psf = 0
!
      end subroutine allocate_edge_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_node_list_4_psf
!
      allocate( inod_search_psf(nnod_search_psf_tot) )
      inod_search_psf = 0
!
      end subroutine allocate_node_list_4_psf
!
!  ---------------------------------------------------------------------
!
      end module m_search_list_4_psf
