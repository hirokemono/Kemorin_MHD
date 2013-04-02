!
!     module m_2nd_phys_data
!.......................................................................
!
!     Written by H. Matsui
!
!       subroutine allocate_2nd_phys_name
!       subroutine allocate_2nd_data_arrays
!
!       subroutine deallocate_2nd_phys_name
!       subroutine deallocate_2nd_data_arrays
!
!       subroutine disconnect_2nd_phys_name
!       subroutine disconnect_2nd_data_arrays
!
!
      module m_2nd_phys_data
!
      use m_precision
!
      implicit  none
! 
      integer (kind=kint) :: num_nod_phys_2nd
!    number of physical data
      integer (kind=kint) :: ntot_nod_phys_2nd
!
      integer (kind=kint), pointer :: ncomps_nod_2nd(:)
! 
      integer (kind=kint), pointer :: istack_nod_comps_2nd(:)
!
      integer (kind=kint), pointer :: iorder_nod_phys_2nd(:)
!
      character (len=kchara), pointer :: phys_nod_name_2nd(:)
! 
      real (kind=kreal), pointer :: d_nod_2nd(:,:)
!!
!     paraamaters to visualizer
!
      integer (kind=kint) :: num_nod_phys_2nd_vis
!    number of physical data to visualizer
      integer (kind=kint) :: ntot_nod_phys_2nd_vis
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
       subroutine allocate_2nd_phys_name
!
          allocate( phys_nod_name_2nd(num_nod_phys_2nd) )
          allocate( iorder_nod_phys_2nd(num_nod_phys_2nd) )
          allocate( ncomps_nod_2nd(num_nod_phys_2nd) )
          allocate( istack_nod_comps_2nd(0:num_nod_phys_2nd) )
!
          phys_nod_name_2nd = ''
          ncomps_nod_2nd =    0
          istack_nod_comps_2nd = 0
          iorder_nod_phys_2nd = 1
!
       end subroutine allocate_2nd_phys_name
!
!  --------------------------------------------------------------------
!
       subroutine allocate_2nd_data_arrays
!
       use m_2nd_geometry_param
!
       allocate( d_nod_2nd(nnod_2nd,ntot_nod_phys_2nd) )
       d_nod_2nd = 0.0d0
!
       end subroutine allocate_2nd_data_arrays
!
!  --------------------------------------------------------------------
! -------------------------------------------------------------------
!
       subroutine deallocate_2nd_phys_name
!
       deallocate( phys_nod_name_2nd, iorder_nod_phys_2nd )
       deallocate( ncomps_nod_2nd, istack_nod_comps_2nd )
!
       end subroutine deallocate_2nd_phys_name
!
!  --------------------------------------------------------------------
!
       subroutine deallocate_2nd_data_arrays
!
       deallocate( d_nod_2nd )
!
       end subroutine deallocate_2nd_data_arrays
!
!  --------------------------------------------------------------------
!
       subroutine disconnect_2nd_phys_name
!
       nullify( phys_nod_name_2nd, iorder_nod_phys_2nd )
       nullify( ncomps_nod_2nd, istack_nod_comps_2nd )
!
       end subroutine disconnect_2nd_phys_name
!
!  --------------------------------------------------------------------
!
       subroutine disconnect_2nd_data_arrays
!
       nullify( d_nod_2nd )
!
       end subroutine disconnect_2nd_data_arrays
!
!  --------------------------------------------------------------------
!
      end module m_2nd_phys_data
