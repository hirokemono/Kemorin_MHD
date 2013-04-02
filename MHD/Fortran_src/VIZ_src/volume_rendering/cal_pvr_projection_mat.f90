!cal_pvr_projection_mat.f90
!      module cal_pvr_projection_mat
!
!        programmed by H.Matsui on May. 2009
!
!      subroutine set_pvr_orthogonal_params
!
!      subroutine set_pvr_projection_matrix
!      subroutine set_pvr_projection_left_mat
!      subroutine set_pvr_projection_right_mat
!
      module cal_pvr_projection_mat
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_control_params_4_pvr
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_orthogonal_params
!
      integer(kind = kint) :: i_pvr
!
!
      do i_pvr = 1, num_pvr
        if (iflag_perspective_mat(i_pvr) .eq. 0) then
          ortho_pvr(1,i_pvr)                                            &
     &               = max( ortho_pvr(1,i_pvr), ortho_pvr(2,i_pvr) )
          ortho_pvr(2,i_pvr) = ortho_pvr(1,i_pvr)
        end if
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'Orthogonal parameter for PVR ', i_pvr
          write(*,*) 'perspective_angle',  perspective_angle(i_pvr)
          write(*,*) 'perspective_xy_ratio',  perspective_xy_ratio(i_pvr)
          write(*,*) 'perspective_near',  perspective_near(i_pvr)
          write(*,*) 'perspective_far',  perspective_far(i_pvr)
          write(*,*) 'ortho_pvr',  ortho_pvr(1:2,i_pvr)
        end if
      end do
!
      end subroutine set_pvr_orthogonal_params
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_projection_matrix
!
      use set_projection_matrix
!
      integer(kind = kint) :: i_pvr, i
!
!
      do i_pvr = 1, num_pvr
        call set_perspective_mat_by_angle(perspective_angle(i_pvr),     &
     &    perspective_xy_ratio(i_pvr), perspective_near(i_pvr),         &
     &    perspective_far(i_pvr), projection_mat(1,i_pvr))
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'projection_mat for PVR ', i_pvr
          do i = 1, 4
            write(*,'(1p4e16.7)') projection_mat(i:i+12:4,i_pvr)
          end do
        end if
      end do
!
      end subroutine set_pvr_projection_matrix
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_projection_left_mat
!
      use set_projection_matrix
!
      integer(kind = kint) :: i_pvr, i
!
      real(kind = kreal) :: pi_180, wd2, ndfl
      real(kind = kreal) :: view_right, view_left
      real(kind = kreal) :: view_top, view_bottom
      real(kind = kreal) :: view_far, view_near
!
!
      do i_pvr = 1, num_pvr
        view_near = perspective_near(i_pvr)
        view_far =  perspective_far(i_pvr)
!
        pi_180 = four * atan(one) / 180.0d0
        wd2 =  view_near * tan( perspective_angle(i_pvr)*pi_180*half )
        ndfl = view_near / focalLength(i_pvr)
!
        view_bottom = - wd2
        view_top =      wd2
        view_left  = - perspective_xy_ratio(i_pvr) * wd2                &
     &               + half * eye_separation(i_pvr) * ndfl
        view_right =   perspective_xy_ratio(i_pvr) * wd2                &
     &               + half * eye_separation(i_pvr) * ndfl
!
        call set_perspective_mat_by_area(view_left, view_right,         &
     &      view_bottom, view_top, view_near, view_far,                 &
     &      projection_left(1,i_pvr) )
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'projection_left for PVR ', i_pvr
          do i = 1, 4
            write(*,'(1p4e16.7)') projection_left(i:i+12:4,i_pvr)
          end do
        end if
      end do
!
      end subroutine set_pvr_projection_left_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_projection_right_mat
!
      use set_projection_matrix
!
      integer(kind = kint) :: i_pvr, i
!
      real(kind = kreal) :: pi_180, wd2, ndfl
      real(kind = kreal) :: view_right, view_left
      real(kind = kreal) :: view_top, view_bottom
      real(kind = kreal) :: view_far, view_near
!
!
      do i_pvr = 1, num_pvr
        view_near = perspective_near(i_pvr)
        view_far =  perspective_far(i_pvr)
!
        pi_180 = four * atan(one) / 180.0d0
        wd2 =  view_near * tan( perspective_angle(i_pvr)*pi_180*half )
        ndfl = view_near / focalLength(i_pvr)
!
        view_bottom = - wd2
        view_top =      wd2
        view_left  = - perspective_xy_ratio(i_pvr) * wd2                &
     &               - half * eye_separation(i_pvr) * ndfl
        view_right =   perspective_xy_ratio(i_pvr) * wd2                &
     &               - half * eye_separation(i_pvr) * ndfl
!
        call set_perspective_mat_by_area(view_left, view_right,         &
     &      view_bottom, view_top, view_near, view_far,                 &
     &      projection_right(1,i_pvr) )
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'projection_right for PVR ', i_pvr
          do i = 1, 4
            write(*,'(1p4e16.7)') projection_right(i:i+12:4,i_pvr)
          end do
        end if
      end do
!
      end subroutine set_pvr_projection_right_mat
!
! -----------------------------------------------------------------------
!
      end module cal_pvr_projection_mat
