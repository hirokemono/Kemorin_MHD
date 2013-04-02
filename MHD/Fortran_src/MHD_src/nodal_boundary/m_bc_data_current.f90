!
!     module m_bc_data_current
!.......................................................................
!
!     Written by Kemorin
!
!       subroutine allocate_bc_current
!       subroutine deallocate_ibc_4_current
!
      module m_bc_data_current
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: ibc_j(:,:)
      integer (kind=kint), allocatable :: ibc2_j(:,:)
!
!
      integer (kind=kint) :: nmax_bc_j_nod
      integer (kind=kint) :: num_bc_j_nod(3)
      integer (kind=kint), allocatable :: ibc_j_id(:,:)
      real (kind=kreal),   allocatable :: bc_j_id_apt(:,:)
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_current
!
       use m_geometry_parameter
!
       allocate(ibc_j(numnod,3))
       allocate(ibc2_j(numnod,3))
!
       ibc_j =  0
       ibc2_j = 0
! 
       allocate(ibc_j_id(nmax_bc_j_nod,3))
       allocate(bc_j_id_apt(nmax_bc_j_nod,3))
       if (nmax_bc_j_nod/=0) then
        ibc_j_id=0 
        bc_j_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_current
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_current
!
        deallocate( ibc_j )
        deallocate( ibc2_j )
!
       end subroutine deallocate_ibc_4_current
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_current
