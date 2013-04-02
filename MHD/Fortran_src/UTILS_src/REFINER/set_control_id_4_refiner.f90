!
!      module set_control_id_4_refiner
!
      module set_control_id_4_refiner
!
!      Written by Kemorin on Oct., 2007
!
      use m_precision
!
      use m_control_param_4_refiner
!
      implicit none
!
!      subroutine set_ele_grp_id_4_refine
!      subroutine set_refine_type_to_id
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_grp_id_4_refine
!
      use m_element_group
!
      integer(kind = kint) :: i, j
!
!
      if (   refined_ele_grp(1) .eq. 'all'                              &
     &  .or. refined_ele_grp(1) .eq. 'All'                              &
     &  .or. refined_ele_grp(1) .eq. 'ALL') then
        id_refined_ele_grp = -1
      else
!
        do j = 1, num_refine_type
          do i = 1, num_mat
            if (refined_ele_grp(j) .eq. mat_name(i) ) then
              id_refined_ele_grp(j) = i
              exit
            end if
          end do
        end do
!
      end if
!
      call deallocate_refine_param_chara
!
      end subroutine set_ele_grp_id_4_refine
!
! -----------------------------------------------------------------------
!
      subroutine set_refine_type_to_id
!
      use m_refine_flag_parameters
!
      integer(kind = kint) :: j
!
!
      if (iflag_redefine_tri .eq. 1) then
!
        do j = 1, num_refine_type
!
          if   (   refined_ele_type(j) .eq. 'quad_20'                   &
     &        .or. refined_ele_type(j) .eq. 'Quad_20'                   &
     &        .or. refined_ele_type(j) .eq. 'QUAD_20') then
            iflag_refine_type(j) = iflag_8_to_20
          else if (refined_ele_type(j) .eq. 'quad_27'                   &
     &        .or. refined_ele_type(j) .eq. 'Quad_27'                   &
     &        .or. refined_ele_type(j) .eq. 'QUAD_27') then
            iflag_refine_type(j) = iflag_8_to_27
!
          else if (refined_ele_type(j) .eq. 'double'                    &
     &        .or. refined_ele_type(j) .eq. 'Double'                    &
     &        .or. refined_ele_type(j) .eq. 'DOUBLE') then
            iflag_refine_type(j) = iflag_double
          else if (refined_ele_type(j) .eq. 'double_x_plane'            &
     &        .or. refined_ele_type(j) .eq. 'Double_x_plane'            &
     &        .or. refined_ele_type(j) .eq. 'DOUBLE_X_PLANE'            &
     &        .or. refined_ele_type(j) .eq. 'double_x'                  &
     &        .or. refined_ele_type(j) .eq. 'Double_x'                  &
     &        .or. refined_ele_type(j) .eq. 'DOUBLE_X') then
            iflag_refine_type(j) = iflag_double_x
          else if (refined_ele_type(j) .eq. 'double_y_plane'            &
     &        .or. refined_ele_type(j) .eq. 'Double_y_plane'            &
     &        .or. refined_ele_type(j) .eq. 'DOUBLE_Y_PLANE'            &
     &        .or. refined_ele_type(j) .eq. 'double_y'                  &
     &        .or. refined_ele_type(j) .eq. 'Double_y'                  &
     &        .or. refined_ele_type(j) .eq. 'DOUBLE_Y') then
            iflag_refine_type(j) = iflag_double_y
          else if (refined_ele_type(j) .eq. 'double_z_plane'            &
     &        .or. refined_ele_type(j) .eq. 'Double_z_plane'            &
     &        .or. refined_ele_type(j) .eq. 'DOUBLE_Z_PLANE'            &
     &        .or. refined_ele_type(j) .eq. 'double_z'                  &
     &        .or. refined_ele_type(j) .eq. 'Double_z'                  &
     &        .or. refined_ele_type(j) .eq. 'DOUBLE_Z') then
            iflag_refine_type(j) = iflag_double_z
!
          else if (refined_ele_type(j) .eq. 'triple'                    &
     &        .or. refined_ele_type(j) .eq. 'Triple'                    &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE') then
            iflag_refine_type(j) = iflag_tri_full
!
          else if (refined_ele_type(j) .eq. 'triple_along_x_axis'       &
     &        .or. refined_ele_type(j) .eq. 'Triple_along_x_axis'       &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_ALONG_X_AXIS'       &
     &        .or. refined_ele_type(j) .eq. 'triple_along_x'            &
     &        .or. refined_ele_type(j) .eq. 'Triple_along_x'            &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_along_X') then
            iflag_refine_type(j) = iflag_tri_x
          else if (refined_ele_type(j) .eq. 'triple_along_y_axis'       &
     &        .or. refined_ele_type(j) .eq. 'Triple_along_y_axis'       &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_ALONG_Y_AXIS'       &
     &        .or. refined_ele_type(j) .eq. 'triple_along_y'            &
     &        .or. refined_ele_type(j) .eq. 'Triple_along_y'            &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_along_Y') then
            iflag_refine_type(j) = iflag_tri_y
          else if (refined_ele_type(j) .eq. 'triple_along_z_axis'       &
     &        .or. refined_ele_type(j) .eq. 'Triple_along_z_axis'       &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_ALONG_Z_AXIS'       &
     &        .or. refined_ele_type(j) .eq. 'triple_along_z'            &
     &        .or. refined_ele_type(j) .eq. 'Triple_along_z'            &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_along_Z') then
            iflag_refine_type(j) = iflag_tri_z
!
          else if (refined_ele_type(j) .eq. 'triple_on_x1_plane'        &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_x1_plane'        &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_X1_PLANE'        &
     &        .or. refined_ele_type(j) .eq. 'triple_on_x1'              &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_x1'              &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_X1') then
            iflag_refine_type(j) = iflag_tri_xs1
          else if (refined_ele_type(j) .eq. 'triple_on_x2_plane'        &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_x2_plane'        &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_X2_PLANE'        &
     &        .or. refined_ele_type(j) .eq. 'triple_on_x2'              &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_x2'              &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_X2') then
            iflag_refine_type(j) = iflag_tri_xs2
          else if (refined_ele_type(j) .eq. 'triple_on_y3_plane'        &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_y3_plane'        &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_Y3_PLANE'        &
     &        .or. refined_ele_type(j) .eq. 'triple_on_y3'              &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_y3'              &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_Y3') then
            iflag_refine_type(j) = iflag_tri_ys3
          else if (refined_ele_type(j) .eq. 'triple_on_y4_plane'        &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_y4_plane'        &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_Y4_PLANE'        &
     &        .or. refined_ele_type(j) .eq. 'triple_on_y4'              &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_y4'              &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_Y4') then
            iflag_refine_type(j) = iflag_tri_ys4
          else if (refined_ele_type(j) .eq. 'triple_on_z5_plane'        &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_z5_plane'        &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_Z5_PLANE'        &
     &        .or. refined_ele_type(j) .eq. 'triple_on_z5'              &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_z5'              &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_Z5') then
            iflag_refine_type(j) = iflag_tri_zs5
          else if (refined_ele_type(j) .eq. 'triple_on_z6_plane'        &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_z6_plane'        &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_Z6_PLANE'        &
     &        .or. refined_ele_type(j) .eq. 'triple_on_z6'              &
     &        .or. refined_ele_type(j) .eq. 'Triple_on_z6'              &
     &        .or. refined_ele_type(j) .eq. 'TRIPLE_on_Z6') then
            iflag_refine_type(j) = iflag_tri_zs6
          end if
!
        end do
!
      end if
!
      end subroutine set_refine_type_to_id
!
! -----------------------------------------------------------------------
!
      end module set_control_id_4_refiner
