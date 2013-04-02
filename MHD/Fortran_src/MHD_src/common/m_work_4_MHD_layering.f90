!
!      module m_work_4_MHD_layering
!
!      Written by H. Matsui on Feb., 2008
!
!      subroutine allocate_lists_4_layer
!      subroutine deallocate_lists_4_layer
!
      module m_work_4_MHD_layering
!
      use m_precision
!
      implicit  none
!
!
      integer(kind=kint), allocatable :: mat_flag_mhd(:)
! 
      integer(kind=kint), allocatable  :: old2newele_layer(:)
!   element index for new ordering
      integer(kind=kint), allocatable  :: new2oldele_layer(:)
!   element index for new ordering
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_lists_4_layer(numele)
!
      integer(kind = kint), intent(in) :: numele
!
      allocate( new2oldele_layer(numele) )
      allocate( old2newele_layer(numele) )
      allocate( mat_flag_mhd(numele) )
!
      new2oldele_layer = 0
      old2newele_layer = 0
      mat_flag_mhd = -10
!
       end subroutine allocate_lists_4_layer
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_lists_4_layer
!
      deallocate( new2oldele_layer )
      deallocate( old2newele_layer )
      deallocate( mat_flag_mhd )
!
      end subroutine deallocate_lists_4_layer
!
! ----------------------------------------------------------------------
!
      end module m_work_4_MHD_layering
