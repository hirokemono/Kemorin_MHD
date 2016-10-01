!>@file  cal_pvr_modelview_mat.f90
!!       module cal_pvr_modelview_mat
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Get model view matrix for PVR
!!
!!@verbatim
!!      subroutine cal_pvr_modelview_matrix                             &
!!     &          (i_rot, outline, view_param, color_param, pvr_screen)
!!        type(pvr_domain_outline), intent(in) :: outline
!!        type(pvr_colormap_parameter), intent(inout) :: color_param
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!        type(pvr_projected_data), intent(inout) :: pvr_screen
!!@endverbatim
!
      module cal_pvr_modelview_mat
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
!
      implicit none
!
      integer(kind = kint) :: ione_stack(0:1) = (/0,1/)
      private :: ione_stack
!
      private :: cal_modelview_mat_by_views
      private :: cal_pvr_rotate_mat_by_views
      private :: set_rot_mat_from_viewpts
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_pvr_modelview_matrix                               &
     &          (i_rot, outline, view_param, color_param, pvr_screen)
!
      use t_surf_grp_4_pvr_domain
      use cal_inverse_small_matrix
      use cal_matrix_vector_smp
!
      integer(kind = kint), intent(in) :: i_rot
      type(pvr_domain_outline), intent(in) :: outline
      type(pvr_colormap_parameter), intent(inout) :: color_param
      type(pvr_view_parameter), intent(inout) :: view_param
      type(pvr_projected_data), intent(inout) :: pvr_screen
!
      integer(kind = kint) :: i, ierr2
      integer(kind = kint) :: istack_l(0:1)
      real(kind = kreal) ::  vec_tmp(4)
      real(kind = kreal) ::  posi_zero(4) = (/zero,zero,zero,one/)
!
!
        if(i_rot .eq. 0) then
          if (view_param%iflag_modelview_mat .eq. 0) then
            call cal_modelview_mat_by_views                             &
     &         (outline, pvr_screen, view_param)
          end if
        else
          call cal_pvr_rotate_mat_by_views                              &
     &       (i_rot, outline, pvr_screen, view_param)
        end if
!
        call cal_inverse_44_matrix(view_param%modelview_mat,            &
     &      view_param%modelview_inv, ierr2)
!
        istack_l(0) = 0
        istack_l(1) = color_param%num_pvr_lights
        do i = 1, color_param%num_pvr_lights
          call cal_mat44_vec3_on_node                                   &
     &       (ione, ione, ione_stack(0), view_param%modelview_mat,      &
     &        color_param%xyz_pvr_lights(1:3,i), vec_tmp(1))
          color_param%view_pvr_lights(1:3,i) = vec_tmp(1:3)
        end do
!
        call cal_mat44_vec3_on_node(ione, ione, ione_stack(0),          &
     &      view_param%modelview_inv, posi_zero(1), vec_tmp(1))
        pvr_screen%viewpoint_vec(1:3) = vec_tmp(1:3)
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'modelview_mat'
          do i = 1, 4
            write(*,'(1p4e16.7)') view_param%modelview_mat(i,1:4)
          end do
!
          write(*,*) 'modelview_inv'
          do i = 1, 4
            write(*,'(1p4e16.7)') view_param%modelview_inv(i,1:4)
          end do
!
          write(*,*) 'lookat_vec', view_param%lookat_vec(1:3)
          write(*,*) 'scale_factor_pvr',                                &
     &              view_param%scale_factor_pvr(1:3)
          write(*,*) 'viewpoint_vec', pvr_screen%viewpoint_vec(1:3)
          write(*,*) 'viewpt_in_view',                                  &
     &              view_param%viewpt_in_viewer_pvr(1:3)
!
          do i = 1, color_param%num_pvr_lights
            write(*,*) 'view_pvr_lights',                               &
     &                i, color_param%view_pvr_lights(1:3,i)
          end do
        end if
!
      end subroutine cal_pvr_modelview_matrix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_modelview_mat_by_views                             &
     &         (outline, pvr_screen, view_param)
!
      use t_surf_grp_4_pvr_domain
      use transform_mat_operations
      use cal_matrix_vector_smp
!
      type(pvr_domain_outline), intent(in) :: outline
      type(pvr_projected_data), intent(in) :: pvr_screen
      type(pvr_view_parameter), intent(inout) :: view_param
!
      real(kind = kreal) :: rev_lookat(3)
      real(kind = kreal) :: rev_eye(3)
!
!
      if(view_param%iflag_lookpoint .eq. 0) then
        view_param%lookat_vec(1:3) = outline%center_g(1:3)
        view_param%iflag_lookpoint = 1
      end if
      rev_lookat(1:3) =   - view_param%lookat_vec(1:3)
!
      if(view_param%iflag_scale_fact .eq. 0) then
        view_param%scale_factor_pvr(1) = 1.0d0 / outline%rmax_g
        view_param%scale_factor_pvr(2) = 1.0d0 / outline%rmax_g
        view_param%scale_factor_pvr(3) = 1.0d0 / outline%rmax_g
        view_param%iflag_scale_fact = 1
      end if
!
!
      if (view_param%iflag_rotation .gt. 0) then
        call Kemo_Unit( view_param%modelview_mat)
        call Kemo_Rotate( view_param%modelview_mat,                     &
     &      view_param%rotation_pvr(1), view_param%rotation_pvr(2:4))
      else
        call set_rot_mat_from_viewpts(pvr_screen, view_param)
      end if
!
      call Kemo_Scale(view_param%modelview_mat,                         &
     &                view_param%scale_factor_pvr)
      call Kemo_Translate(view_param%modelview_mat, rev_lookat)
      view_param%iflag_modelview_mat = 1
!
!
      rev_eye(1:3) = - view_param%viewpt_in_viewer_pvr(1:3)
      if (view_param%iflag_viewpt_in_view .eq. 0) then
        call cal_mat44_vec3_on_node(ione, ione, ione_stack,             &
     &    view_param%modelview_mat, pvr_screen%viewpoint_vec, rev_eye)
        call Kemo_Translate(view_param%modelview_mat,                   &
     &      rev_eye)
        view_param%iflag_viewpt_in_view = 1
      else
        call Kemo_Translate(view_param%modelview_mat, rev_eye)
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'viewpt_in_view',                                    &
     &            view_param%viewpt_in_viewer_pvr(1:3)
      end if
!
!
      end subroutine cal_modelview_mat_by_views
!
! -----------------------------------------------------------------------
!
      subroutine cal_pvr_rotate_mat_by_views                            &
     &         (i_rot, outline, pvr_screen, view_param)
!
      use t_surf_grp_4_pvr_domain
      use transform_mat_operations
      use cal_matrix_vector_smp
!
      integer(kind = kint), intent(in) :: i_rot
      type(pvr_domain_outline), intent(in) :: outline
      type(pvr_projected_data), intent(in) :: pvr_screen
      type(pvr_view_parameter), intent(inout) :: view_param
!
      integer(kind = kint) :: iaxis_rot
      real(kind = kreal) :: rotation_axis(3), rev_lookat(3)
      real(kind = kreal) :: rev_eye(3)
      real(kind = kreal) :: angle_deg
!
!
!
      if(view_param%iflag_lookpoint .eq. 0) then
        view_param%lookat_vec(1:3) = outline%center_g(1:3)
        view_param%iflag_lookpoint = 1
      end if
      rev_lookat(1:3) =   - view_param%lookat_vec(1:3)
!
      if(view_param%iflag_scale_fact .eq. 0) then
        view_param%scale_factor_pvr(1) = 1.0d0 / outline%rmax_g
        view_param%scale_factor_pvr(2) = 1.0d0 / outline%rmax_g
        view_param%scale_factor_pvr(3) = 1.0d0 / outline%rmax_g
        view_param%iflag_scale_fact = 1
      end if
!
!
      if (view_param%iflag_rotate_snap .gt. 0) then
        call Kemo_Unit(view_param%modelview_mat)
!
        iaxis_rot = view_param%iprm_pvr_rot(1)
        rotation_axis(1:3) =       zero
        rotation_axis(iaxis_rot) = one
        angle_deg = 360.0d0 * dble(i_rot-1)                             &
     &           / dble(view_param%iprm_pvr_rot(2))
        call Kemo_Rotate(view_param%modelview_mat,                      &
     &    angle_deg, rotation_axis(1) )
!
        call Kemo_Rotate(view_param%modelview_mat,                      &
     &      view_param%rotation_pvr(1), view_param%rotation_pvr(2:4))
      else
        call set_rot_mat_from_viewpts(pvr_screen, view_param)
      end if
!
      call Kemo_Scale(view_param%modelview_mat,                         &
     &    view_param%scale_factor_pvr)
      call Kemo_Translate(view_param%modelview_mat, rev_lookat)
      view_param%iflag_modelview_mat = 1
!
!    rotation matrix for movie
!
!
      rev_eye(1:3) = - view_param%viewpt_in_viewer_pvr(1:3)
      if (view_param%iflag_viewpt_in_view .eq. 0) then
        call cal_mat44_vec3_on_node(ione, ione, ione_stack,             &
     &    view_param%modelview_mat, pvr_screen%viewpoint_vec, rev_eye)
        call Kemo_Translate(view_param%modelview_mat, rev_eye)
        view_param%iflag_viewpt_in_view = 1
      else
        call Kemo_Translate(view_param%modelview_mat, rev_eye)
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'viewpt_in_view',                                    &
     &             view_param%viewpt_in_viewer_pvr(1:3)
      end if
!
      end subroutine cal_pvr_rotate_mat_by_views
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rot_mat_from_viewpts(pvr_screen, view_param)
!
      use mag_of_field_smp
      use cal_products_smp
!
      type(pvr_projected_data), intent(in) :: pvr_screen
      type(pvr_view_parameter), intent(inout) :: view_param
!
      integer(kind = kint) :: i
      real(kind = kreal) :: viewing_dir(3), u(3), v(3), size(1)
      real(kind = kreal) :: look_norm(3), view_norm(3)
!
!
      viewing_dir(1:3) = view_param%lookat_vec(1:3)                     &
     &                  - pvr_screen%viewpoint_vec(1:3)
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack(0),              &
     &    viewing_dir(1), size(1) )
!$omp end parallel
      viewing_dir(1:3) = viewing_dir(1:3) / size(1)
!
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack(0),              &
     &    viewing_dir(1), size(1) )
!$omp end parallel
      look_norm(1:3) = view_param%lookat_vec(1:3) / size(1)
!
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack(0),              &
     &    pvr_screen%viewpoint_vec, size(1) )
!$omp end parallel
      view_norm(1:3) = pvr_screen%viewpoint_vec(1:3) / size(1)
!
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack(0),              &
     &    view_param%up_direction_vec(1), size(1) )
!$omp end parallel
      view_param%up_direction_vec(1:3)                                  &
     &     = view_param%up_direction_vec(1:3) / size(1)
!
!    /* find the direction of axis U */
      call cal_cross_prod_no_coef_smp(ione, ione, ione_stack(0),        &
     &    view_param%up_direction_vec(1), viewing_dir(1), u(1))
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack(0),              &
     &    u(1), size(1) )
!$omp end parallel
      u(1:3) = u(1:3) / size(1)
!
!    /*find the direction of axix V */
      call cal_cross_prod_no_coef_smp(ione, ione, ione_stack(0),        &
     &    viewing_dir(1), u(1), v(1) )
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack(0),              &
     &    v(1), size(1) )
!$omp end parallel
      v(1:3) = v(1:3) / size(1)
!
      do i = 1, 3
        view_param%modelview_mat(1,i) = u(i)
        view_param%modelview_mat(2,i) = v(i)
        view_param%modelview_mat(3,i) = viewing_dir(i)
        view_param%modelview_mat(4,i) = zero
      end do
      view_param%modelview_mat(1:3,4) = zero
      view_param%modelview_mat(4,4) = one
!
      end subroutine set_rot_mat_from_viewpts
!
! -----------------------------------------------------------------------
!
      end module cal_pvr_modelview_mat
