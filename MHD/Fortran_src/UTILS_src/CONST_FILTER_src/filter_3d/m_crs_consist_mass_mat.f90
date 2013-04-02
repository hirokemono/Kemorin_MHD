!
!     module m_crs_consist_mass_mat
!
      module m_crs_consist_mass_mat
!
!     Written by H. Matsui on Oct., 2006
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: ntot_mass_l, ntot_mass_u
!
      integer(kind = kint), allocatable :: istack_mass_l(:)
      integer(kind = kint), allocatable :: istack_mass_u(:)
!
      integer(kind = kint), allocatable :: item_mass_l(:)
      integer(kind = kint), allocatable :: item_mass_u(:)
!
      real(kind = kreal), allocatable :: aiccg_mass(:)
!
      integer (kind = kint) :: num_mass_mat
!   total number of component
      integer (kind = kint) :: im_mass_d
!   pointer for diagonal component
      integer (kind = kint) :: im_mass_u
!   pointer for upper part of matrix
      integer (kind = kint) :: im_mass_l
!   pointer for lower part of matrix
!
!      subroutine allocate_mass_connect
!      subroutine allocate_aiccg_mass
!
!      subroutine deallocate_mass_connect
!      subroutine deallocate_aiccg_mass
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_mass_connect
!
       use m_geometry_parameter
!
       allocate(istack_mass_l(0:numnod) )
       allocate(istack_mass_u(0:numnod) )
!
       allocate(item_mass_l(ntot_mass_l) )
       allocate(item_mass_u(ntot_mass_u) )
!
       istack_mass_l = 0
       istack_mass_u = 0
       item_mass_l =   0
       item_mass_u =   0
!
      end subroutine allocate_mass_connect
!
!-----------------------------------------------------------------------
!
      subroutine allocate_aiccg_mass
!
      allocate(aiccg_mass(0:num_mass_mat))
      aiccg_mass = 0.0d0
!
      end subroutine allocate_aiccg_mass
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_mass_connect
!
       deallocate( istack_mass_l )
       deallocate( istack_mass_u )
!
       deallocate( item_mass_l )
       deallocate( item_mass_u )
!
      end subroutine deallocate_mass_connect
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_aiccg_mass
!
      deallocate( aiccg_mass )
!
      end subroutine deallocate_aiccg_mass
!
!-----------------------------------------------------------------------
!
      end module m_crs_consist_mass_mat
