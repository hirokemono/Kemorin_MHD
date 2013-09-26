!
!      module m_cubed_sph_radius
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine allocate_shell_radius
!      subroutine allocate_ref_edge_latitude
!
!      subroutine deallocate_shell_radius
!      subroutine deallocate_ref_edge_latitude
!
      module m_cubed_sph_radius
!
      use m_precision
!
      implicit none
!
!>   num. of layer
      integer(kind = kint) :: n_shell, nr_adj, nr_back
!>   radius
      real(kind = kreal), allocatable :: r_nod(:)
!
      integer(kind = kint) :: nr_ocore, nr_exter
!
!
!  corner edge information
!
      integer(kind = kint) :: num_edge_latitude_ref
      integer(kind = kint), allocatable :: kr_edge_latitude_ref(:)
      real(kind = kreal), allocatable :: edge_latitude_ref(:)
      real(kind = kreal), allocatable :: edge_latitude(:)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_shell_radius
!
      allocate(r_nod(n_shell))
      r_nod = 0.0d0
!
      end subroutine allocate_shell_radius
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ref_edge_latitude
!
      allocate(kr_edge_latitude_ref(0:num_edge_latitude_ref+1))
      allocate(edge_latitude_ref(num_edge_latitude_ref))
      allocate(edge_latitude(n_shell))
!
      if(num_edge_latitude_ref .gt. 0) then
        kr_edge_latitude_ref(1:num_edge_latitude_ref) = 0
        edge_latitude_ref(1:num_edge_latitude_ref) = 0.0d0
      end if
      kr_edge_latitude_ref(0) = 0
      kr_edge_latitude_ref(num_edge_latitude_ref+1) = n_shell
!
      edge_latitude = 45.0d0
!
      end subroutine allocate_ref_edge_latitude
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_shell_radius
!
      deallocate(r_nod)
!
      end subroutine deallocate_shell_radius
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ref_edge_latitude
!
      deallocate(kr_edge_latitude_ref, edge_latitude_ref)
      deallocate(edge_latitude)
!
      end subroutine deallocate_ref_edge_latitude
!
!  ---------------------------------------------------------------------
!
      end module m_cubed_sph_radius
