!
!      module set_control_id_4_refiner
!
!      Written by Kemorin on Oct., 2007
!
!      subroutine set_ele_grp_id_4_refine(ele_grp)
!      subroutine set_refine_type_to_id
!
      module set_control_id_4_refiner
!
      use m_precision
!
      use m_control_param_4_refiner
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_grp_id_4_refine(ele_grp)
!
      use t_group_data
      use skip_comment_f
!
      type(group_data), intent(in) :: ele_grp
      integer(kind = kint) :: i, j
!
!
      if (cmp_no_case(refined_ele_grp(1),'all')) then
        id_refined_ele_grp = -1
      else
!
        do j = 1, num_refine_type
          do i = 1, ele_grp%num_grp
            if (refined_ele_grp(j) .eq. ele_grp%grp_name(i) ) then
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
      use skip_comment_f
!
      integer(kind = kint) :: j
!
!
      if (iflag_redefine_tri .eq. 1) then
!
        do j = 1, num_refine_type
!
          if     (cmp_no_case(refined_ele_type(j),'quad_20')) then
            iflag_refine_type(j) = iflag_8_to_20
          else if(cmp_no_case(refined_ele_type(j),'quad_27')) then
            iflag_refine_type(j) = iflag_8_to_27
!
          else if(cmp_no_case(refined_ele_type(j),'double')) then
            iflag_refine_type(j) = iflag_double
          else if(cmp_no_case(refined_ele_type(j),'double_x_plane')     &
     &       .or. cmp_no_case(refined_ele_type(j),'double_x')) then
            iflag_refine_type(j) = iflag_double_x
          else if(cmp_no_case(refined_ele_type(j),'double_y_plane')     &
     &       .or. cmp_no_case(refined_ele_type(j),'double_y')) then
            iflag_refine_type(j) = iflag_double_y
          else if(cmp_no_case(refined_ele_type(j),'double_z_plane')     &
     &       .or. cmp_no_case(refined_ele_type(j),'double_z')) then
            iflag_refine_type(j) = iflag_double_z
!
          else if(cmp_no_case(refined_ele_type(j),'triple')) then
            iflag_refine_type(j) = iflag_tri_full
!
          else if                                                       &
     &        (cmp_no_case(refined_ele_type(j),'triple_along_x_axis')   &
     &    .or. cmp_no_case(refined_ele_type(j),'triple_along_x')) then
            iflag_refine_type(j) = iflag_tri_x
          else if                                                       &
     &        (cmp_no_case(refined_ele_type(j),'triple_along_y_axis')   &
     &    .or. cmp_no_case(refined_ele_type(j),'triple_along_y')) then
            iflag_refine_type(j) = iflag_tri_y
          else if                                                       &
     &        (cmp_no_case(refined_ele_type(j),'triple_along_z_axis')   &
     &    .or. cmp_no_case(refined_ele_type(j),'triple_along_z')) then
            iflag_refine_type(j) = iflag_tri_z
!
          else if                                                       &
     &        (cmp_no_case(refined_ele_type(j),'triple_on_x1_plane')    &
     &    .or. cmp_no_case(refined_ele_type(j),'triple_on_x1')) then
            iflag_refine_type(j) = iflag_tri_xs1
          else if                                                       &
     &         (cmp_no_case(refined_ele_type(j),'triple_on_x2_plane')   &
     &     .or. cmp_no_case(refined_ele_type(j),'triple_on_x2')) then
            iflag_refine_type(j) = iflag_tri_xs2
          else if                                                       &
     &        (cmp_no_case(refined_ele_type(j),'triple_on_y3_plane')    &
     &    .or. cmp_no_case(refined_ele_type(j),'triple_on_y3')) then
            iflag_refine_type(j) = iflag_tri_ys3
          else if                                                       &
     &       (cmp_no_case(refined_ele_type(j),'triple_on_y4_plane')     &
     &   .or. cmp_no_case(refined_ele_type(j),'triple_on_y4')) then
            iflag_refine_type(j) = iflag_tri_ys4
          else if                                                       &
     &        (cmp_no_case(refined_ele_type(j),'triple_on_z5_plane')    &
     &    .or. cmp_no_case(refined_ele_type(j),'triple_on_z5')) then
            iflag_refine_type(j) = iflag_tri_zs5
          else if                                                       &
     &        (cmp_no_case(refined_ele_type(j),'triple_on_z6_plane')    &
     &    .or. cmp_no_case(refined_ele_type(j),'triple_on_z6')) then
            iflag_refine_type(j) = iflag_tri_zs6
          end if
!
        end do
      end if
!
      end subroutine set_refine_type_to_id
!
! -----------------------------------------------------------------------
!
      end module set_control_id_4_refiner
