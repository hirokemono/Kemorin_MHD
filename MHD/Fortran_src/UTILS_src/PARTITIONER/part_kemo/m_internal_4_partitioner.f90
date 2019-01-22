!m_internal_4_partitioner.f90
!      module m_internal_4_partitioner
!
      module m_internal_4_partitioner
!
!      Written by H. Matsui on Aug., 2007
!
      use m_precision
      use t_internal_4_partitioner
!
      implicit none
!
!>      internal nodes for each subdomains
      type(internal_4_partitioner) :: itl_nod_part
!
!>      internal elements for each subdomains
      type(internal_4_partitioner) :: itl_ele_part
!
!>      internal surfaces for each subdomains
      type(internal_4_partitioner) :: itl_surf_part
!
!>      internal edges for each subdomains
      type(internal_4_partitioner) :: itl_edge_part
!
!      subroutine deallocate_interele_4_part
!      subroutine deallocate_iele_4_subdomain
!      subroutine deallocate_intersurf_4_part
!      subroutine deallocate_interedge_4_part
!      subroutine deallocate_iedge_4_subdomain
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_interele_4_part
!
      call dealloc_internal_4_part(itl_ele_part)
!
      end subroutine deallocate_interele_4_part
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_iele_4_subdomain
!
      call dealloc_id_4_subdomain(itl_ele_part)
      call dealloc_num_4_subdomain(itl_ele_part)
!
      end subroutine deallocate_iele_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_intersurf_4_part
!
      call dealloc_internal_4_part(itl_surf_part)
!
      end subroutine deallocate_intersurf_4_part
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_isurf_4_subdomain
!
      call dealloc_id_4_subdomain(itl_surf_part)
      call dealloc_num_4_subdomain(itl_surf_part)
!
      end subroutine deallocate_isurf_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_interedge_4_part
!
      call dealloc_internal_4_part(itl_edge_part)
!
      end subroutine deallocate_interedge_4_part
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_iedge_4_subdomain
!
      call dealloc_id_4_subdomain(itl_edge_part)
      call dealloc_num_4_subdomain(itl_edge_part)
!
      end subroutine deallocate_iedge_4_subdomain
!
!   --------------------------------------------------------------------
!
      end module m_internal_4_partitioner
