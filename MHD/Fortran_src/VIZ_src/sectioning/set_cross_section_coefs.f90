!set_cross_section_coefs.f90
!      module set_cross_section_coefs
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine set_coefs_4_psf(num_const, c_name_psf, const_psf_ctl, &
!                c_surf)
!      subroutine set_coefs_4_sphere(psf, c_surf)
!      subroutine set_coefs_4_ellipsode(psf, c_surf)
!      subroutine set_coefs_4_hyperboloide(psf, c_surf)
!      subroutine set_coefs_4_parabolic(psf, c_surf)
!
      module set_cross_section_coefs
!
      use m_precision
!
      use m_constants
      use m_control_data_4_psf
!
      implicit  none
!
      private :: set_parameter_2_vectors
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_psf(num_const, c_name_psf, const_psf_ctl,  &
     &          c_surf)
!
      integer(kind = kint), intent(in) :: num_const
      character(len=kchara), intent(in) :: c_name_psf(num_const)
      real(kind = kreal), intent(in) :: const_psf_ctl(num_const)
!
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_const
!
        if      (c_name_psf(i).eq."x2" .or. c_name_psf(i).eq."X^2"      &
     &      .or. c_name_psf(i).eq.'x2' .or.  c_name_psf(i).eq.'X2'      &
     &      .or. c_name_psf(i).eq.'xx' .or.  c_name_psf(i).eq.'XX'      &
     &         ) then
          c_surf(1) = const_psf_ctl(i)
        else if (c_name_psf(i).eq.'y^2' .or. c_name_psf(i).eq.'Y^2'     &
     &      .or. c_name_psf(i).eq.'y2' .or.  c_name_psf(i).eq.'Y2'      &
     &      .or. c_name_psf(i).eq.'yy' .or.  c_name_psf(i).eq.'YY'      &
     &         ) then
          c_surf(2) = const_psf_ctl(i)
        else if (c_name_psf(i).eq.'z^2' .or. c_name_psf(i).eq.'Z^2'     &
     &      .or. c_name_psf(i).eq.'z2' .or.  c_name_psf(i).eq.'Z2'      &
     &      .or. c_name_psf(i).eq.'zz' .or.  c_name_psf(i).eq.'ZZ'      &
     &         ) then
          c_surf(3) = const_psf_ctl(i)
!
        else if (c_name_psf(i).eq.'xy' .or. c_name_psf(i).eq.'XY'       &
     &      .or. c_name_psf(i).eq.'yx' .or. c_name_psf(i).eq.'YX') then
          c_surf(4) = const_psf_ctl(i)
        else if (c_name_psf(i).eq.'yz' .or. c_name_psf(i).eq.'YZ'       &
     &      .or. c_name_psf(i).eq.'zy' .or. c_name_psf(i).eq.'ZY') then
          c_surf(5) = const_psf_ctl(i)
        else if (c_name_psf(i).eq.'zx' .or. c_name_psf(i).eq.'ZX'       &
     &      .or. c_name_psf(i).eq.'xz' .or. c_name_psf(i).eq.'xz') then
          c_surf(6) = const_psf_ctl(i)
!
        else if (c_name_psf(i).eq.'x^1' .or. c_name_psf(i).eq.'X^1'     &
     &      .or. c_name_psf(i).eq.'x1' .or.  c_name_psf(i).eq.'X1'      &
     &      .or. c_name_psf(i).eq.'x' .or.   c_name_psf(i).eq.'X'       &
     &         ) then
          c_surf(7) = const_psf_ctl(i)
        else if (c_name_psf(i).eq.'y^1' .or. c_name_psf(i).eq.'Y^1'     &
     &      .or. c_name_psf(i).eq.'y1' .or.  c_name_psf(i).eq.'Y1'      &
     &      .or. c_name_psf(i).eq.'y' .or.  c_name_psf(i).eq.'Y'        &
     &         ) then
          c_surf(8) = const_psf_ctl(i)
        else if (c_name_psf(i).eq.'z^1' .or. c_name_psf(i).eq.'Z^1'     &
     &      .or. c_name_psf(i).eq.'z1' .or.  c_name_psf(i).eq.'Z1'      &
     &      .or. c_name_psf(i).eq.'z' .or.  c_name_psf(i).eq.'Z'        &
     &         ) then
          c_surf(9) = const_psf_ctl(i)
!
        else if (c_name_psf(i).eq.'const' .or. c_name_psf(i).eq.'Const' &
     &      .or. c_name_psf(i).eq.'CONST' .or. c_name_psf(i).eq.'c'     &
     &      .or. c_name_psf(i).eq.'C' ) then
          c_surf(10) = const_psf_ctl(i)
!
        end if
      end do
!
      end subroutine set_coefs_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_sphere(psf, c_surf)
!
      type(psf_ctl), intent(in) :: psf
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3)
!
!
      call set_parameter_2_vectors(psf%num_center_psf_ctl,              &
     &    psf%center_name_psf_ctl, psf%center_psf_ctl, xc)
!
      c_surf( 1) =  one
      c_surf( 2) =  one
      c_surf( 3) =  one
      c_surf( 4) = zero
      c_surf( 5) = zero
      c_surf( 6) = zero
      c_surf( 7) = -two*xc(1)
      c_surf( 8) = -two*xc(2)
      c_surf( 9) = -two*xc(3)
      c_surf(10) = xc(1)*xc(1) + xc(2)*xc(2) + xc(3)*xc(3)              &
     &            - (psf%radius_psf_ctl*psf%radius_psf_ctl)
!
      end subroutine set_coefs_4_sphere
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_ellipsode(psf, c_surf)
!
      type(psf_ctl), intent(in) :: psf
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf%num_center_psf_ctl,              &
     &    psf%center_name_psf_ctl, psf%center_psf_ctl, xc)
      call set_parameter_2_vectors(psf%num_axis_psf_ctl,                &
     &    psf%axis_name_psf_ctl, psf%axis_psf_ctl, axc)
!
      c_surf( 1) =  one / (axc(1)*axc(1))
      c_surf( 2) =  one / (axc(2)*axc(2))
      c_surf( 3) =  one / (axc(3)*axc(3))
      c_surf( 4) = zero
      c_surf( 5) = zero
      c_surf( 6) = zero
      c_surf( 7) = -two*xc(1) / (axc(1)*axc(1))
      c_surf( 8) = -two*xc(2) / (axc(2)*axc(2))
      c_surf( 9) = -two*xc(3) / (axc(3)*axc(3))
      c_surf(10) =  xc(1)*xc(1) / (axc(1)*axc(1))                       &
     &            + xc(2)*xc(2) / (axc(2)*axc(2))                       &
     &            + xc(3)*xc(3) / (axc(3)*axc(3))                       &
     &            - one
!
      end subroutine set_coefs_4_ellipsode
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_hyperboloide(psf, c_surf)
!
      type(psf_ctl), intent(in) :: psf
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf%num_center_psf_ctl,              &
     &    psf%center_name_psf_ctl, psf%center_psf_ctl, xc)
      call set_parameter_2_vectors(psf%num_axis_psf_ctl,                &
     &    psf%axis_name_psf_ctl, psf%axis_psf_ctl, axc)
!
      c_surf( 1) =  one / (axc(1)*axc(1))
      c_surf( 2) =  one / (axc(2)*axc(2))
      c_surf( 3) = -one / (axc(3)*axc(3))
      c_surf( 4) = zero
      c_surf( 5) = zero
      c_surf( 6) = zero
      c_surf( 7) = -two*xc(1) / (axc(1)*axc(1))
      c_surf( 8) = -two*xc(2) / (axc(2)*axc(2))
      c_surf( 9) =  two*xc(3) / (axc(3)*axc(3))
      c_surf(10) =  xc(1)*xc(1) / (axc(1)*axc(1))                       &
     &            + xc(2)*xc(2) / (axc(2)*axc(2))                       &
     &            - xc(3)*xc(3) / (axc(3)*axc(3))                       &
     &            - one
!
      end subroutine set_coefs_4_hyperboloide
!
!  ---------------------------------------------------------------------
!
      subroutine set_coefs_4_parabolic(psf, c_surf)
!
      type(psf_ctl), intent(in) :: psf
      real(kind = kreal), intent(inout) :: c_surf(10)
!
      real(kind = kreal) :: xc(3), axc(3)
!
!
      call set_parameter_2_vectors(psf%num_center_psf_ctl,              &
     &    psf%center_name_psf_ctl, psf%center_psf_ctl, xc)
      call set_parameter_2_vectors(psf%num_axis_psf_ctl,                &
     &    psf%axis_name_psf_ctl, psf%axis_psf_ctl, axc)
!
      c_surf( 1) =  one / (axc(1)*axc(1))
      c_surf( 2) = -one / (axc(2)*axc(2))
      c_surf( 3) =  zero
      c_surf( 4) = zero
      c_surf( 5) = zero
      c_surf( 6) = zero
      c_surf( 7) = -two*xc(1) / (axc(1)*axc(1))
      c_surf( 8) =  two*xc(2) / (axc(2)*axc(2))
      c_surf( 9) = -one
      c_surf(10) =  xc(1)*xc(1) / (axc(1)*axc(1))                       &
     &            - xc(2)*xc(2) / (axc(2)*axc(2))                       &
     &            + xc(3)
!
      end subroutine set_coefs_4_parabolic
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_parameter_2_vectors(num_vect, ctl_name, vect_ctl,  &
     &          vector)
!
      integer(kind = kint), intent(in) :: num_vect
      character(len=kchara), intent(in) :: ctl_name(num_vect)
      real(kind = kreal), intent(in) :: vect_ctl(num_vect)
!
      real(kind = kreal), intent(inout) :: vector(num_vect)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_vect
        if      (ctl_name(i).eq.'x' .or. ctl_name(i).eq.'X') then
          vector(1) = vect_ctl(i)
        else if (ctl_name(i).eq.'y' .or. ctl_name(i).eq.'Y') then
          vector(2) = vect_ctl(i)
        else if (ctl_name(i).eq.'z' .or. ctl_name(i).eq.'Z') then
          vector(3) = vect_ctl(i)
        end if
      end do
!
      end subroutine set_parameter_2_vectors
!
!  ---------------------------------------------------------------------
!
      end module set_cross_section_coefs
