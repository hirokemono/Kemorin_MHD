!products_at_poles.f90
!      module products_at_poles
!
!        programmed by H.Matsui on Oct., 2009
!
!      subroutine pole_sph_dot_prod_w_const(numnod, internal_node, xx,  &
!     &          nnod_rtp, nidx_rtp_r, coef, d_vec1, d_vec2, d_flux)
!      subroutine pole_sph_cross_prod_w_const(numnod, internal_node, xx,&
!     &          nnod_rtp, nidx_rtp_r, coef, d_vec1, d_vec2, d_prod)
!      subroutine pole_vec_scalar_prod_w_const(numnod, internal_node,   &
!     &          xx, nnod_rtp, nidx_rtp_r, coef, d_vec1, d_scl, d_prod)
!
      module products_at_poles
!
      use m_precision
      use m_constants
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine pole_sph_dot_prod_w_const(numnod, internal_node, xx,   &
     &          nnod_rtp, nidx_rtp_r, coef, d_vec1, d_vec2, d_flux)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: d_vec1(numnod,3)
      real(kind = kreal), intent(in) :: d_vec2(numnod,3)
      real(kind = kreal), intent(inout) :: d_flux(numnod)
!
      integer(kind = kint) :: inod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_flux(inod) =   coef * (d_vec1(inod,1)*d_vec2(inod,1)        &
     &                           + d_vec1(inod,2)*d_vec2(inod,2)        &
     &                           + d_vec1(inod,3)*d_vec2(inod,3)  )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_flux(inod) =   coef * (d_vec1(inod,1)*d_vec2(inod,1)        &
     &                           + d_vec1(inod,2)*d_vec2(inod,2)        &
     &                           + d_vec1(inod,3)*d_vec2(inod,3)  )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_flux(inod) =   coef * (d_vec1(inod,1)*d_vec2(inod,1)            &
     &                       + d_vec1(inod,2)*d_vec2(inod,2)            &
     &                       + d_vec1(inod,3)*d_vec2(inod,3)  )
!
      end subroutine pole_sph_dot_prod_w_const
!
! -----------------------------------------------------------------------
!
      subroutine pole_sph_cross_prod_w_const(numnod, internal_node, xx, &
     &          nnod_rtp, nidx_rtp_r, coef, d_vec1, d_vec2, d_prod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: d_vec1(numnod,3)
      real(kind = kreal), intent(in) :: d_vec2(numnod,3)
      real(kind = kreal), intent(inout) :: d_prod(numnod,3)
!
      integer(kind = kint) :: inod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_prod(inod,1) =   coef * (d_vec1(inod,2)*d_vec2(inod,3)      &
     &                             - d_vec1(inod,3)*d_vec2(inod,2) )
          d_prod(inod,2) =   coef * (d_vec1(inod,3)*d_vec2(inod,1)      &
     &                             - d_vec1(inod,1)*d_vec2(inod,3) )
          d_prod(inod,3) =   coef * (d_vec1(inod,1)*d_vec2(inod,2)      &
     &                             - d_vec1(inod,2)*d_vec2(inod,1) )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_prod(inod,1) =   coef * (d_vec1(inod,2)*d_vec2(inod,3)      &
     &                             - d_vec1(inod,3)*d_vec2(inod,2) )
          d_prod(inod,2) =   coef * (d_vec1(inod,3)*d_vec2(inod,1)      &
     &                             - d_vec1(inod,1)*d_vec2(inod,3) )
          d_prod(inod,3) =   coef * (d_vec1(inod,1)*d_vec2(inod,2)      &
     &                             - d_vec1(inod,2)*d_vec2(inod,1) )
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_prod(inod,1) =   coef * (d_vec1(inod,2)*d_vec2(inod,3)          &
     &                         - d_vec1(inod,3)*d_vec2(inod,2) )
      d_prod(inod,2) =   coef * (d_vec1(inod,3)*d_vec2(inod,1)          &
     &                         - d_vec1(inod,1)*d_vec2(inod,3) )
      d_prod(inod,3) =   coef * (d_vec1(inod,1)*d_vec2(inod,2)          &
     &                         - d_vec1(inod,2)*d_vec2(inod,1) )
!
      end subroutine pole_sph_cross_prod_w_const
!
! -----------------------------------------------------------------------
!
      subroutine pole_vec_scalar_prod_w_const(numnod, internal_node,    &
     &          xx, nnod_rtp, nidx_rtp_r, coef, d_vec1, d_scl, d_prod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: d_vec1(numnod,3)
      real(kind = kreal), intent(in) :: d_scl(numnod)
      real(kind = kreal), intent(inout) :: d_prod(numnod,3)
!
      integer(kind = kint) :: inod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_prod(inod,1) =   coef * d_vec1(inod,1)*d_scl(inod)
          d_prod(inod,2) =   coef * d_vec1(inod,2)*d_scl(inod)
          d_prod(inod,3) =   coef * d_vec1(inod,3)*d_scl(inod)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_prod(inod,1) =   coef * d_vec1(inod,1)*d_scl(inod)
          d_prod(inod,2) =   coef * d_vec1(inod,2)*d_scl(inod)
          d_prod(inod,3) =   coef * d_vec1(inod,3)*d_scl(inod)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
          d_prod(inod,1) =   coef * d_vec1(inod,1)*d_scl(inod)
          d_prod(inod,2) =   coef * d_vec1(inod,2)*d_scl(inod)
          d_prod(inod,3) =   coef * d_vec1(inod,3)*d_scl(inod)
!
      end subroutine pole_vec_scalar_prod_w_const
!
! -----------------------------------------------------------------------
!
      end module products_at_poles
