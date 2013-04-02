!cal_pvr_modelview_mat.f90
!      module cal_pvr_modelview_mat
!
!        programmed by H.Matsui on May. 2009
!
!      subroutine set_pvr_projection_matrix
!      subroutine cal_pvr_modelview_matrix
!      subroutine cal_pvr_rotate_mat_by_views(i_pvr, i_rot)
!
      module cal_pvr_modelview_mat
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_control_params_4_pvr
!
      implicit none
!
      integer(kind = kint) :: ione_stack(0:1) = (/0,1/)
      private :: ione_stack
!
      private :: cal_modelview_mat_by_views
      private :: set_rot_mat_from_viewpts
!
!      subroutine cal_pvr_modelview_matrix
!      subroutine cal_pvr_rotate_mat_by_views(i_pvr, i_rot)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_pvr_modelview_matrix(i_rot)
!
      use cal_inverse_small_matrix
      use cal_matrix_vector_smp
!
      integer(kind = kint), intent(in) :: i_rot
!
      integer(kind = kint) :: i_pvr, i, ierr2
      integer(kind = kint) :: ist, ied, istack_l(0:1)
      real(kind = kreal) ::  vec_tmp(4)
      real(kind = kreal) ::  posi_zero(4) = (/zero,zero,zero,one/)
!
!
      do i_pvr = 1, num_pvr
!
        if(i_rot .eq. 0) then
          if (iflag_modelview_mat(i_pvr) .eq. 0) then
            call cal_modelview_mat_by_views(i_pvr)
          end if
        else
          call cal_pvr_rotate_mat_by_views(i_pvr,i_rot)
        end if
!
        call cal_inverse_44_matrix(modelview_mat(1,i_pvr),              &
     &      modelview_inv(1,i_pvr), ierr2)
!
        istack_l(0) = 0
        istack_l(1) = num_pvr_lights(i_pvr)
        ist = istack_pvr_lights(i_pvr-1) + 1
        ied = istack_pvr_lights(i_pvr  )
        do i = ist, ied
          call cal_mat44_vec3_on_node(ione, ione, ione_stack(0),        &
     &        modelview_mat(1,i_pvr), xyz_pvr_lights(1,i),              &
     &        vec_tmp(1) )
          view_pvr_lights(1:3,i) = vec_tmp(1:3)
        end do
!
        call cal_mat44_vec3_on_node(ione, ione, ione_stack(0),          &
     &      modelview_inv(1,i_pvr), posi_zero(1),                       &
     &     vec_tmp(1) )
        viewpoint_vec(1:3,i_pvr) = vec_tmp(1:3)

!
        if (iflag_debug .gt. 0) then
          write(*,*) 'modelview_mat'
          do i = 1, 4
            write(*,'(1p4e16.7)') modelview_mat(i:i+12:4,i_pvr)
          end do
!
          write(*,*) 'modelview_inv'
          do i = 1, 4
            write(*,'(1p4e16.7)') modelview_inv(i:i+12:4,i_pvr)
          end do
!
          write(*,*) 'lookat_vec', lookat_vec(1:3,i_pvr)
          write(*,*) 'scale_factor_pvr', scale_factor_pvr(1:3,i_pvr)
          write(*,*) 'viewpoint_vec', viewpoint_vec(1:3,i_pvr)
          write(*,*) 'viewpt_in_view', viewpt_in_viewer_pvr(1:3,i_pvr)
!
          do i = istack_pvr_lights(i_pvr-1)+1, istack_pvr_lights(i_pvr)
            write(*,*) 'view_pvr_lights', i, view_pvr_lights(1:3,i)
          end do
        end if
!
      end do
!
      end subroutine cal_pvr_modelview_matrix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_modelview_mat_by_views(i_pvr)
!
      use m_mesh_outline_pvr
      use transform_mat_operations
      use cal_matrix_vector_smp
!
      integer(kind = kint), intent(in) :: i_pvr
!
      real(kind = kreal) :: rev_lookat(3)
!
!
!
      if(iflag_lookpoint_vec(i_pvr) .eq. 0) then
        lookat_vec(1:3,i_pvr) = center_g(1:3,i_pvr)
        iflag_lookpoint_vec(i_pvr) = 1
      end if
      rev_lookat(1:3) =   - lookat_vec(1:3,i_pvr)
!
      if(iflag_scale_fact_pvr(i_pvr) .eq. 0) then
        scale_factor_pvr(1,i_pvr) = 1.0d0 / rmax_g(i_pvr)
        scale_factor_pvr(2,i_pvr) = 1.0d0 / rmax_g(i_pvr)
        scale_factor_pvr(3,i_pvr) = 1.0d0 / rmax_g(i_pvr)
        iflag_scale_fact_pvr(i_pvr) = 1
      end if
!
!
      if (iflag_rotation_pvr(i_pvr) .gt. 0) then
        call Kemo_Unit( modelview_mat(1,i_pvr) )
        call Kemo_Rotate( modelview_mat(1,i_pvr),                       &
     &      rotation_pvr(1,i_pvr), rotation_pvr(2,i_pvr) )
      else
        call set_rot_mat_from_viewpts(i_pvr, modelview_mat(1,i_pvr) )
      end if
!
      call Kemo_Scale(modelview_mat(1,i_pvr),                           &
     &    scale_factor_pvr(1,i_pvr) )
      call Kemo_Translate( modelview_mat(1,i_pvr), rev_lookat)
      iflag_modelview_mat(i_pvr) = 1
!
!
      if (iflag_viewpt_in_view_pvr(i_pvr) .eq. 0) then
        call cal_mat44_vec3_on_node(ione, ione, ione_stack,             &
     &    modelview_mat(1,i_pvr), viewpoint_vec(1,i_pvr),               &
     &    viewpt_in_viewer_pvr(1,i_pvr) )
        call Kemo_Translate( modelview_mat(1,i_pvr),                    &
     &      viewpt_in_viewer_pvr(1,i_pvr))
        iflag_viewpt_in_view_pvr(i_pvr) = 1
      else
        call Kemo_Translate( modelview_mat(1,i_pvr),                    &
     &      viewpt_in_viewer_pvr(1,i_pvr))
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'viewpt_in_view', viewpt_in_viewer_pvr(1:3,i_pvr)
      end if
!
!
      end subroutine cal_modelview_mat_by_views
!
! -----------------------------------------------------------------------
!
      subroutine cal_pvr_rotate_mat_by_views(i_pvr, i_rot)
!
      use m_mesh_outline_pvr
      use transform_mat_operations
      use cal_matrix_vector_smp
!
      integer(kind = kint), intent(in) :: i_pvr, i_rot
!
      integer(kind = kint) :: iaxis_rot
      real(kind = kreal) :: rotation_axis(3), rev_lookat(3)
      real(kind = kreal) :: movie_mat(4,4), angle_deg
!
!
!
      if(iflag_lookpoint_vec(i_pvr) .eq. 0) then
        lookat_vec(1:3,i_pvr) = center_g(1:3,i_pvr)
        iflag_lookpoint_vec(i_pvr) = 1
      end if
      rev_lookat(1:3) =   - lookat_vec(1:3,i_pvr)
!
      if(iflag_scale_fact_pvr(i_pvr) .eq. 0) then
        scale_factor_pvr(1,i_pvr) = 1.0d0 / rmax_g(i_pvr)
        scale_factor_pvr(2,i_pvr) = 1.0d0 / rmax_g(i_pvr)
        scale_factor_pvr(3,i_pvr) = 1.0d0 / rmax_g(i_pvr)
        iflag_scale_fact_pvr(i_pvr) = 1
      end if
!
!
      if (iflag_rotation_pvr(i_pvr) .gt. 0) then
        call Kemo_Unit( modelview_mat(1,i_pvr) )
!
      iaxis_rot = iprm_pvr_rot(1,i_pvr)
      rotation_axis(1:3) =       zero
      rotation_axis(iaxis_rot) = one
      angle_deg = 360.0d0 * dble(i_rot-1) / dble(iprm_pvr_rot(2,i_pvr))
      call Kemo_Rotate( modelview_mat(1,i_pvr),                       &
     &    angle_deg, rotation_axis(1) )
!
        call Kemo_Rotate( modelview_mat(1,i_pvr),                       &
     &      rotation_pvr(1,i_pvr), rotation_pvr(2,i_pvr) )
      else
        call set_rot_mat_from_viewpts(i_pvr, modelview_mat(1,i_pvr) )
      end if
!
      call Kemo_Scale(modelview_mat(1,i_pvr),                           &
     &    scale_factor_pvr(1,i_pvr) )
      call Kemo_Translate( modelview_mat(1,i_pvr), rev_lookat)
      iflag_modelview_mat(i_pvr) = 1
!
!    rotation matrix for movie
!
!
      if (iflag_viewpt_in_view_pvr(i_pvr) .eq. 0) then
        call cal_mat44_vec3_on_node(ione, ione, ione_stack,             &
     &    modelview_mat(1,i_pvr), viewpoint_vec(1,i_pvr),               &
     &    viewpt_in_viewer_pvr(1,i_pvr) )
        call Kemo_Translate( modelview_mat(1,i_pvr),                    &
     &      viewpt_in_viewer_pvr(1,i_pvr))
        iflag_viewpt_in_view_pvr(i_pvr) = 1
      else
        call Kemo_Translate( modelview_mat(1,i_pvr),                    &
     &      viewpt_in_viewer_pvr(1,i_pvr))
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'viewpt_in_view', viewpt_in_viewer_pvr(1:3,i_pvr)
      end if
!
      end subroutine cal_pvr_rotate_mat_by_views
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rot_mat_from_viewpts(i_pvr, rot_mat)
!
      use mag_of_field_smp
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: i_pvr
      real(kind = kreal), intent(inout) :: rot_mat(16)
!
      integer(kind = kint) :: i
      real(kind = kreal) :: viewing_dir(3), u(3), v(3), size(1)
      real(kind = kreal) :: look_norm(3), view_norm(3)
!
!
      viewing_dir(1:3) = lookat_vec(1:3,i_pvr)                          &
     &                  - viewpoint_vec(1:3,i_pvr)
      call cal_vector_magnitude(ione, ione, ione_stack(0), size(1),     &
     &    viewing_dir(1) )
      viewing_dir(1:3) = viewing_dir(1:3) / size(1)
!
      call cal_vector_magnitude(ione, ione, ione_stack(0), size(1),     &
     &    viewing_dir(1) )
      look_norm(1:3) = lookat_vec(1:3,i_pvr) / size(1)
!
      call cal_vector_magnitude(ione, ione, ione_stack(0), size(1),     &
     &    viewpoint_vec(1,i_pvr) )
      view_norm(1:3) = viewpoint_vec(1:3,i_pvr) / size(1)
!
      call cal_vector_magnitude(ione, ione, ione_stack(0), size(1),     &
     &    up_direction_vec(1,i_pvr) )
      up_direction_vec(1:3,i_pvr) = up_direction_vec(1:3,i_pvr)         &
     &                             / size(1)
!
!    /* find the direction of axis U */
      call cal_vect_prod_no_coef_smp(ione, ione, ione_stack(0),         &
     &    up_direction_vec(1,i_pvr), viewing_dir(1), u(1) )
      call cal_vector_magnitude(ione, ione, ione_stack(0),              &
     &    size(1), u(1) )
      u(1:3) = u(1:3) / size(1)
!
!    /*find the direction of axix V */
      call cal_vect_prod_no_coef_smp(ione, ione, ione_stack(0),         &
     &    viewing_dir(1), u(1), v(1) )
      call cal_vector_magnitude(ione, ione, ione_stack(0),              &
     &    size(1), v(1) )
      v(1:3) = v(1:3) / size(1)
!
      do i = 1, 3
        rot_mat(4*i-3) = u(i)
        rot_mat(4*i-2) = v(i)
        rot_mat(4*i-1) = viewing_dir(i)
        rot_mat(4*i  ) = zero
      end do
      rot_mat(13:15) = zero
      rot_mat(16   ) = one
!
      end subroutine set_rot_mat_from_viewpts
!
! -----------------------------------------------------------------------
!
      end module cal_pvr_modelview_mat
