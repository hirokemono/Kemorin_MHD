!>@file   set_edge_data_4_IO.f90
!!@brief  module set_edge_data_4_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Copy edge data between IO buffer
!!
!!@verbatim
!!      subroutine copy_edge_connect_to_IO(edge, nele, nsurf)
!!      subroutine copy_edge_geometry_to_IO(edge)
!!      subroutine copy_edge_geometry_to_IO_sph(edge)
!!      subroutine copy_edge_geometry_to_IO_cyl(edge)
!!
!!      subroutine copy_edge_connect_from_IO(edge, nele, nsurf)
!!@endverbatim
!
      module set_edge_data_4_IO
!
      use m_precision
!
      use t_edge_data
      use m_read_mesh_data
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine copy_edge_connect_to_IO(edge, nele, nsurf)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele, nsurf
      type(edge_data), intent(in) :: edge
!
      integer(kind = kint) :: iele, isurf, iedge
!
!
      ele_IO%numele = nele
!
      nsf_4_ele_IO =    nsurf
      nsurf_in_ele_IO = nedge_4_surf
!
      ned_4_ele_IO = nele
!
      ele_IO%numele =     edge%numedge
      ele_IO%nnod_4_ele = edge%nnod_4_edge
!
      call allocate_ele_connect_type(ele_IO)
      call allocate_surface_connect_IO
      call allocate_edge_connect_IO
!
      if      (edge%nnod_4_edge .eq. num_linear_edge) then
        ele_IO%elmtyp(1:edge%numedge) = 111
      else if (edge%nnod_4_edge .eq. num_quad_edge) then
        ele_IO%elmtyp(1:edge%numedge) = 112
      end if
!
!omp parallel do
      do iedge = 1, edge%numedge
        ele_IO%iele_global(iedge) = edge%iedge_global(iedge)
        ele_IO%nodelm(iedge) =      edge%nnod_4_edge
        ele_IO%ie(iedge,1:edge%nnod_4_edge)                             &
     &        = edge%ie_edge(iedge,1:edge%nnod_4_edge)
      end do
!omp end parallel do
!
!omp parallel do
      do isurf = 1, nsurf
        isf_4_ele_IO(isurf,1:nedge_4_surf)                              &
     &        = edge%iedge_4_sf(isurf,1:nedge_4_surf)
      end do
!omp end parallel do
!
!omp parallel do
      do iele = 1, nele
        iedge_4_ele_IO(iele,1:nedge_4_ele)                              &
     &        = edge%iedge_4_ele(iele,1:nedge_4_ele)
      end do
!omp end parallel do
!
      end subroutine copy_edge_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_edge_geometry_to_IO(edge)
!
      type(edge_data), intent(inout) :: edge
      integer(kind = kint) :: iedge
!
!
      nod_IO%numnod =        edge%numedge
      nod_IO%internal_node = edge%internal_edge
!
      call alloc_node_geometry_base(nod_IO)
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
!omp parallel do
      do iedge = 1, edge%numedge
        nod_IO%inod_global(iedge) = edge%iedge_global(iedge)
        nod_IO%xx(iedge,1) =        edge%x_edge(iedge,1)
        nod_IO%xx(iedge,2) =        edge%x_edge(iedge,2)
        nod_IO%xx(iedge,3) =        edge%x_edge(iedge,3)
        ele_scalar_IO(iedge) =     edge%edge_length(iedge)
        ele_vector_IO(iedge,1) =   edge%edge_vect(iedge,1)
        ele_vector_IO(iedge,2) =   edge%edge_vect(iedge,2)
        ele_vector_IO(iedge,3) =   edge%edge_vect(iedge,3)
      end do
!omp end parallel do
!
      end subroutine copy_edge_geometry_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_edge_geometry_to_IO_sph(edge)
!
      type(edge_data), intent(inout) :: edge
      integer(kind = kint) :: iedge
!
!
      nod_IO%numnod =        edge%numedge
      nod_IO%internal_node = edge%internal_edge
!
      call alloc_node_geometry_base(nod_IO)
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
!omp parallel do
      do iedge = 1, edge%numedge
        nod_IO%inod_global(iedge) = edge%iedge_global(iedge)
        nod_IO%xx(iedge,1) =        edge%r_edge(iedge)
        nod_IO%xx(iedge,2) =        edge%theta_edge(iedge)
        nod_IO%xx(iedge,3) =        edge%phi_edge(iedge)
        ele_scalar_IO(iedge) =     edge%edge_length(iedge)
        ele_vector_IO(iedge,1) =   edge%edge_vect_sph(iedge,1)
        ele_vector_IO(iedge,2) =   edge%edge_vect_sph(iedge,2)
        ele_vector_IO(iedge,3) =   edge%edge_vect_sph(iedge,3)
      end do
!omp end parallel do
!
      end subroutine copy_edge_geometry_to_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_edge_geometry_to_IO_cyl(edge)
!
      type(edge_data), intent(inout) :: edge
      integer(kind = kint) :: iedge
!
!
      nod_IO%numnod =        edge%numedge
      nod_IO%internal_node = edge%internal_edge
!
      call alloc_node_geometry_base(nod_IO)
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
!omp parallel do
      do iedge = 1, edge%numedge
        nod_IO%inod_global(iedge) = edge%iedge_global(iedge)
        nod_IO%xx(iedge,1) =        edge%s_edge(iedge)
        nod_IO%xx(iedge,2) =        edge%phi_edge(iedge)
        nod_IO%xx(iedge,3) =        edge%x_edge(iedge,3)
        ele_scalar_IO(iedge) =     edge%edge_length(iedge)
        ele_vector_IO(iedge,1) =   edge%edge_vect_cyl(iedge,1)
        ele_vector_IO(iedge,2) =   edge%edge_vect_cyl(iedge,2)
        ele_vector_IO(iedge,3) =   edge%edge_vect_cyl(iedge,3)
      end do
!omp end parallel do
!
      end subroutine copy_edge_geometry_to_IO_cyl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_edge_connect_from_IO(edge, nele, nsurf)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele, nsurf
      type(edge_data), intent(inout) :: edge
!
      integer(kind = kint) :: iele, isurf, iedge
!
      edge%numedge = ele_IO%numele
!
      call allocate_edge_connect_type(edge, nsurf)
      call allocate_edge_4_ele_type(edge, nele)
!
      do iedge = 1, edge%numedge
        edge%iedge_global(iedge) = ele_IO%iele_global(iedge)
        edge%ie_edge(iedge,1:edge%nnod_4_edge)                          &
     &        = ele_IO%ie(iedge,1:edge%nnod_4_edge)
      end do
!
      do isurf = 1, nsurf
        edge%iedge_4_sf(isurf,1:nedge_4_surf)                           &
     &        = isf_4_ele_IO(isurf,1:nedge_4_surf)
      end do
!
      do iele = 1, nele
        edge%iedge_4_ele(iele,1:nedge_4_ele)                            &
     &        = iedge_4_ele_IO(iele,1:nedge_4_ele)
      end do
!
      call deallocate_surface_connect_IO
      call deallocate_ele_connect_type(ele_IO)
      call deallocate_edge_connect_IO
!
      end subroutine copy_edge_connect_from_IO
!
!------------------------------------------------------------------
!
      end module set_edge_data_4_IO
