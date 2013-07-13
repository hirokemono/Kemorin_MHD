!copy_pick_udt_data_plane.f90
!      module copy_pick_udt_data_plane
!
!      subroutine copy_and_pick_udt_data_merge(nnod, internod,          &
!     &          nnod_target, inod_gl, nfield_target, icomp_target,     &
!     &          ifield_target, phys_data, ucd)
!
!      written by H. Matsui
!
      module copy_pick_udt_data_plane
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_and_pick_udt_data_merge(nnod, internod,           &
     &          nnod_target, inod_gl, nfield_target, icomp_target,      &
     &          ifield_target, phys_data, ucd)
!
      use t_ucd_data
!
      type(ucd_data), intent(in) :: ucd
!
      integer(kind = kint), intent(in) :: nnod, internod
      integer(kind = kint), intent(in) :: nfield_target, nnod_target
      integer(kind = kint), intent(in) :: icomp_target(nfield_target)
      integer(kind = kint), intent(in) :: ifield_target(nfield_target)
      integer(kind = kint), intent(in) :: inod_gl(nnod)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: phys_data(nnod_target*nfield_target)
!
      integer(kind = kint) :: j, ic, ii
      integer(kind = kint) :: inod, inod_global
!
!
      do inod =1, internod
        inod_global = inod_gl(inod)
!
        if (inod_global .le. nnod_target) then
          ic  = 0
          do j = 1, nfield_target
            if ( icomp_target(j) .ge. 0) then
              ic = icomp_target(j) + ifield_target(j)
              ii = inod_global+(j-1)*nnod_target
!
              phys_data(ii) = ucd%d_ucd(inod,ic)
            end if
          end do
!
        end if
      end do
!
      end subroutine copy_and_pick_udt_data_merge
!
! -----------------------------------------------------------------------
!
      end module copy_pick_udt_data_plane
