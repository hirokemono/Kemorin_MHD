!
!     module set_reference_value
!
!      Written by H.Matsui
!      Modified by H.Matsui on Sep., 2007
!
!!      subroutine set_reference_temp(numnod, nnod_fl, inod_fluid,      &
!!     &          xx, radius, a_radius, ncomp_nod, i_ref_t, i_gref_t,   &
!!     &          d_nod)
!
      module set_reference_value
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_reference_temp(numnod, nnod_fl, inod_fluid,        &
     &          xx, radius, a_radius, ncomp_nod, i_ref_t, i_gref_t,     &
     &          d_nod)
!
      use m_physical_property
      use m_control_parameter
!
      integer(kind = kint), intent(in) :: numnod, nnod_fl, ncomp_nod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      integer (kind = kint), intent(in) :: inod_fluid(nnod_fl)
      integer(kind = kint), intent(in) :: i_ref_t, i_gref_t
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, inum
!
!
! set reference temperature (for spherical shell)
!
      if ( iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        d_nod(1:numnod,i_ref_t) = zero
!
        do inum = 1, nnod_fl
          inod = inod_fluid(inum)
          d_nod(inod,i_ref_t) = ( (high_temp - low_temp)                &
     &                        * depth_high_t*depth_low_t*a_radius(inod) &
     &                        - depth_high_t*high_temp                  &
     &                        + depth_low_t* low_temp )                 &
     &                         / ( depth_low_t - depth_high_t )
          d_nod(inod,i_gref_t) = (-(high_temp - low_temp)               &
     &                           * depth_high_t*depth_low_t)            &
     &                          / ( (depth_low_t-depth_high_t)          &
     &                           * radius(inod)**2 )
        end do
      end if
!
      if ( iflag_4_ref_temp .eq. id_linear_r_ref_temp) then
        d_nod(1:numnod,i_ref_t) = zero
!
        do inum = 1, nnod_fl
          inod = inod_fluid(inum)
          d_nod(inod,i_ref_t) = high_temp                               &
     &                     - (high_temp - low_temp)                     &
     &                     * (radius(inod) - depth_high_t)              &
     &                     / (depth_low_t - depth_high_t)
          d_nod(inod,i_gref_t) = -(high_temp - low_temp)                &
     &                     / (depth_low_t - depth_high_t)
        end do
      end if
!
!
      if (iflag_4_ref_temp .ge. 1 .and. iflag_4_ref_temp .le. 3) then
        d_nod(1:numnod,i_ref_t) = zero
        do inum = 1, nnod_fl
          inod = inod_fluid(inum)
          d_nod(inod,i_ref_t) = (high_temp - low_temp)                  &
     &        * ( xx(inod,iflag_4_ref_temp) - depth_low_t )             &
     &         / ( depth_high_t - depth_low_t ) + low_temp
          d_nod(inod,i_gref_t) = (high_temp - low_temp)                 &
     &         / ( depth_high_t - depth_low_t )
        end do
      end if
!
      if (iflag_t_strat .gt. id_turn_OFF) then
        d_nod(1:numnod,i_gref_t) = zero
!
        do inum = 1, nnod_fl
!
          inod = inod_fluid(inum)
!
          d_nod(inod,i_gref_t)                                          &
     &         = - 0.5d0 * ( radius(inod) + stratified_sigma )          &
     &          * ( 1.0d0 - tanh( (radius(inod)-stratified_outer_r)     &
     &           / stratified_width ) ) + stratified_sigma
!
          d_nod(inod,i_gref_t)                                          &
     &         = d_nod(inod,i_gref_t) * a_radius(inod)
        end do
      end if
!
      end subroutine set_reference_temp
!
! -----------------------------------------------------------------------
!
      end module set_reference_value

