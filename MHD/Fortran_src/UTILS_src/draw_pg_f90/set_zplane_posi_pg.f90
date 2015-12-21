!
!      module set_zplane_posi_pg
!
!      subroutine set_zplane_graph_position(nnod_pg, nele_pg,           &
!     &          r_o, frame, xx, ie, xg, ie_pg)
!      subroutine set_xplane_graph_position(nnod_pg, nele_pg,           &
!     &          r_o, frame, xx, ie, xg, ie_pg)
!      subroutine set_yplane_graph_position(nnod_pg, nele_pg,           &
!     &          r_o, frame, xx, ie, xg, ie_pg)
!
!      subroutine set_zplane_vector(iflag_vec, nnod_pg, xx_psf, vector, &
!     &          vect_pg, vert_pg)
!      subroutine set_xplane_vector(iflag_vec, nnod_pg, xx_psf, vector, &
!     &          vect_pg, vert_pg)
!      subroutine set_yplane_vector(iflag_vec, nnod_pg, xx_psf, vector, &
!     &          vect_pg, vert_pg)
!
!      subroutine zcframe
!      subroutine mdframe
!
      module set_zplane_posi_pg
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_zplane_graph_position(nnod_pg, nele_pg,            &
     &          r_o, frame, xx, ie, xg, ie_pg)
!
      integer(kind = kint), intent(in) :: nnod_pg, nele_pg
      integer(kind = kint), intent(in) :: ie(nele_pg,3)
      real(kind = kreal), intent(in) ::   xx(nnod_pg,3)
      real(kind = kreal), intent(in) :: r_o
!
      integer(kind = kint), intent(inout) :: ie_pg(3,nele_pg)
      real(kind = kreal), intent(inout) :: xg(3,nnod_pg)
      real(kind = kreal), intent(inout) :: frame
!
!
      xg(1,1:nnod_pg) = real( xx(1:nnod_pg,1) )
      xg(2,1:nnod_pg) = real( xx(1:nnod_pg,2) )
      xg(3,1:nnod_pg) = real( xx(1:nnod_pg,3) )
!
      write(*,*) 'z= ', xx(1,3)
!
      frame = r_o*1.4d0
!
      ie_pg(1,1:nele_pg) = ie(1:nele_pg,1)
      ie_pg(2,1:nele_pg) = ie(1:nele_pg,2)
      ie_pg(3,1:nele_pg) = ie(1:nele_pg,3)
!
      end subroutine set_zplane_graph_position
!
! ----------------------------------------------------------------------
!
      subroutine set_xplane_graph_position(nnod_pg, nele_pg,            &
     &          r_o, frame, xx, ie, xg, ie_pg)
!
      integer(kind = kint), intent(in) :: nnod_pg, nele_pg
      integer(kind = kint), intent(in) :: ie(nele_pg,3)
      real(kind = kreal), intent(in) ::   xx(nnod_pg,3)
      real(kind = kreal), intent(in) :: r_o
!
      integer(kind = kint), intent(inout) :: ie_pg(3,nele_pg)
      real(kind = kreal), intent(inout) :: xg(3,nnod_pg)
      real(kind = kreal), intent(inout) :: frame
!
!
      xg(3,1:nnod_pg) = real( xx(1:nnod_pg,1) )
      xg(1,1:nnod_pg) = real( xx(1:nnod_pg,2) )
      xg(2,1:nnod_pg) = real( xx(1:nnod_pg,3) )
!
      write(*,*) 'x= ', xx(1,1)
!
      frame = r_o*1.4d0
!
      ie_pg(1,1:nele_pg) = ie(1:nele_pg,1)
      ie_pg(2,1:nele_pg) = ie(1:nele_pg,2)
      ie_pg(3,1:nele_pg) = ie(1:nele_pg,3)
!
      end subroutine set_xplane_graph_position
!
! ----------------------------------------------------------------------
!
      subroutine set_yplane_graph_position(nnod_pg, nele_pg,            &
     &          r_o, frame, xx, ie, xg, ie_pg)
!
      integer(kind = kint), intent(in) :: nnod_pg, nele_pg
      integer(kind = kint), intent(in) :: ie(nele_pg,3)
      real(kind = kreal), intent(in) ::   xx(nnod_pg,3)
      real(kind = kreal), intent(in) :: r_o
!
      integer(kind = kint), intent(inout) :: ie_pg(3,nele_pg)
      real(kind = kreal), intent(inout) :: xg(3,nnod_pg)
      real(kind = kreal), intent(inout) :: frame
!
!
      xg(1,1:nnod_pg) = xx(1:nnod_pg,1)
      xg(3,1:nnod_pg) = xx(1:nnod_pg,2)
      xg(2,1:nnod_pg) = xx(1:nnod_pg,3)
!
      write(*,*) 'y= ', xx(1,2)
!
      frame = r_o*1.4d0
!
      ie_pg(1,1:nele_pg) = ie(1:nele_pg,1)
      ie_pg(2,1:nele_pg) = ie(1:nele_pg,2)
      ie_pg(3,1:nele_pg) = ie(1:nele_pg,3)
!
      end subroutine set_yplane_graph_position
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_zplane_vector(iflag_vec, nnod_pg, xx_psf,          &
     &          ncomp_pg, ifld_pg, vector, vect_pg, vert_pg)
!
      use set_components_flags
      use set_zplane_xyz_vect_pg
      use set_zplane_cyl_vect_pg
      use set_zplane_sph_vect_pg
!
      integer(kind = kint), intent(in) :: iflag_vec, nnod_pg
      integer(kind = kint), intent(in) :: ncomp_pg, ifld_pg
      real(kind = kreal), intent(in) :: xx_psf(nnod_pg,3)
      real(kind = kreal), intent(in) :: vector(nnod_pg,ncomp_pg)
!
      real(kind = kreal), intent(inout) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: vert_pg(nnod_pg)
!
!
      if(iflag_vec .eq. icomp_SPH_VECTOR) then
        call set_zplane_sph_vector(nnod_pg, xx_psf, vector(1,ifld_pg),  &
     &      vect_pg, vert_pg)
      else if(iflag_vec .eq. icomp_CYL_VECTOR) then
        call set_zplane_cyl_vector(nnod_pg, xx_psf, vector(1,ifld_pg),  &
     &      vect_pg, vert_pg)
      else
        call set_zplane_xyz_vector(nnod_pg, vector(1,ifld_pg),          &
     &      vect_pg, vert_pg)
      end if
!
      end subroutine set_zplane_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_xplane_vector(iflag_vec, nnod_pg, xx_psf,          &
     &          ncomp_pg, ifld_pg, vector, vect_pg, vert_pg)
!
      use set_components_flags
      use set_zplane_xyz_vect_pg
      use set_zplane_cyl_vect_pg
      use set_zplane_sph_vect_pg
!
      integer(kind = kint), intent(in) :: iflag_vec, nnod_pg
      integer(kind = kint), intent(in) :: ncomp_pg, ifld_pg
      real(kind = kreal), intent(in) :: xx_psf(nnod_pg,3)
      real(kind = kreal), intent(in) :: vector(nnod_pg,ncomp_pg)
!
      real(kind = kreal), intent(inout) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: vert_pg(nnod_pg)
!
!
      if(iflag_vec .eq. icomp_SPH_VECTOR) then
        call set_xplane_sph_vector(nnod_pg, xx_psf, vector(1,ifld_pg),  &
     &      vect_pg, vert_pg)
      else if(iflag_vec .eq. icomp_CYL_VECTOR) then
        call set_xplane_cyl_vector(nnod_pg, xx_psf, vector(1,ifld_pg),  &
     &      vect_pg, vert_pg)
      else
        call set_xplane_xyz_vector(nnod_pg, vector(1,ifld_pg),          &
     &      vect_pg, vert_pg)
      end if
!
      end subroutine set_xplane_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_yplane_vector(iflag_vec, nnod_pg, xx_psf,          &
     &          ncomp_pg, ifld_pg, vector, vect_pg, vert_pg)
!
      use set_components_flags
      use set_zplane_xyz_vect_pg
      use set_zplane_cyl_vect_pg
      use set_zplane_sph_vect_pg
!
      integer(kind = kint), intent(in) :: iflag_vec, nnod_pg
      integer(kind = kint), intent(in) :: ncomp_pg, ifld_pg
      real(kind = kreal), intent(in) :: xx_psf(nnod_pg,3)
      real(kind = kreal), intent(in) :: vector(nnod_pg,ncomp_pg)
!
      real(kind = kreal), intent(inout) :: vect_pg(2,nnod_pg)
      real(kind = kreal), intent(inout) :: vert_pg(nnod_pg)
!
!
      if(iflag_vec .eq. icomp_SPH_VECTOR) then
        call set_yplane_sph_vector(nnod_pg, xx_psf, vector(1,ifld_pg),  &
     &      vect_pg, vert_pg)
      else if(iflag_vec .eq. icomp_CYL_VECTOR) then
        call set_yplane_cyl_vector(nnod_pg, xx_psf, vector(1,ifld_pg),  &
     &      vect_pg, vert_pg)
      else
        call set_yplane_xyz_vector(nnod_pg, vector(1,ifld_pg),          &
     &      vect_pg, vert_pg)
      end if
!
      end subroutine set_yplane_vector
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine zcframe
!*************************************************
!*
!*  subroutine for draw frame of z-plane after draw data
!*
!*************************************************
!*
!
      real :: zz, rin, rout
      real :: rin_d, rout_d
!*
!*
!* -----   lead radious of frame ---------------------
!*
      if ( zz .lt. rin )then
       rin_d = sqrt(rin**2-zz**2)
      end if
      if ( zz .lt. rout )then
       rout_d = sqrt(rout**2-zz**2)
      end if
!
!*  -----  draw frame lines of outer core ---------------
!*
      call pgslw(itwo)
      call pgsci(ione)
      call pgsfs(itwo)
!
      if ( zz .lt. rin )then
        call pgcirc(rzero, rzero, rin_d)
      end if
      if ( zz .lt. rout )then
        call pgcirc(rzero, rzero, rout_d)
      end if
!
      call pgslw(ione)
!
      end subroutine zcframe
!
! ----------------------------------------------------------------------
!
      subroutine mdframe
!*
      real :: rin, rout
!*
!*
      call pgsci(1)
      call pgslw(1)
      call pgmove( 0.0 ,  rout )
      call pgdraw( 0.0 , -rout )
!*
      call pgsfs (2)
      call pgcirc( 0.0 , 0.0 , rout )
      call pgcirc( 0.0 , 0.0 , rin )
!
      call pgslw(ione)
!
      end subroutine mdframe
!
! ----------------------------------------------------------------------
!
      end module set_zplane_posi_pg
