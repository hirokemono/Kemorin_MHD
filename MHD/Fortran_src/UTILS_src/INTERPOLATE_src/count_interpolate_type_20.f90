!
!      module count_interpolate_type_20
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_count_interpolate_type_20(ist, ied,                 &
!     &          nnod_interpolate_type)
!
      module count_interpolate_type_20
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_count_interpolate_type_20(ist, ied,                  &
     &          nnod_interpolate_type)
!
      use m_interpolate_table_dest
      use m_interpolate_coefs_dest
!
      integer(kind = kint), intent(in) :: ist, ied
!
      integer(kind = kint), intent(inout)                               &
     &      :: nnod_interpolate_type(4)
!
      integer(kind = kint) :: inod, icou
!
!
      nnod_interpolate_type(1:4) = 0
      do inod = ist, ied
!
!   for nodes
!
        if (     coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
!
!
        else if( coef_inter_dest(inod,1) .eq. zero                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. zero                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq. zero                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. zero                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
!
        else if( coef_inter_dest(inod,1) .eq. zero                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. zero                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq. zero                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. zero                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. zero) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. zero) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. zero) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. zero) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
!
!
!   for edges
!
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &     .and. coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
!
!
!   for surfaces
!
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
!
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
!
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
!
!
!   for volume
!
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(4) = nnod_interpolate_type(4) + 1
!
        end if
!
      end do
!
      end subroutine s_count_interpolate_type_20
!
!-----------------------------------------------------------------------
!
      end module count_interpolate_type_20
