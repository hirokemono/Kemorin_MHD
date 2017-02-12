!
!      module set_3d_filtering_group_id
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine s_set_3d_filtering_group_id(filter, filter_param)
!!      subroutine s_set_w_filtering_group_id(wide_filter, filter_param)
!!        type(filter_coefficients_type), intent(in) :: filter
!!        type(filter_coefficients_type), intent(in) :: wide_filter
!!        type(SGS_filtering_params), intent(inout) :: filter_param
!
      module set_3d_filtering_group_id
!
      use m_precision
!
      use m_machine_parameter
      use t_SGS_control_parameter
      use t_filter_coefficients
!
      implicit none
!
      private :: set_each_filtering_group_id
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_3d_filtering_group_id(filter, filter_param)
!
      type(filter_coefficients_type), intent(in) :: filter
      type(SGS_filtering_params), intent(inout) :: filter_param
!
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &     'igrp, id_whole_filter_grp, whole_filter_grp'
      call set_each_filtering_group_id(filter, filter_param%whole)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &      'igrp, id_fluid_filter_grp, fluid_filter_grp'
      call set_each_filtering_group_id(filter, filter_param%fluid)
!
      end subroutine s_set_3d_filtering_group_id
!
! ----------------------------------------------------------------------
!
      subroutine s_set_w_filtering_group_id(wide_filter, filter_param)
!
      type(filter_coefficients_type), intent(in) :: wide_filter
      type(SGS_filtering_params), intent(inout) :: filter_param
!
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &     'igrp, id_whole_w_filter_grp, whole_w_filter_grp'
      call set_each_filtering_group_id                                  &
     &   (wide_filter, filter_param%whole_wide)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &      'igrp, id_fluid_w_filter_grp, fluid_w_filter_grp'
      call set_each_filtering_group_id                                  &
     &   (wide_filter, filter_param%fluid_wide)
!
      end subroutine s_set_w_filtering_group_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_each_filtering_group_id(filter, f_area)
!
      type(filter_coefficients_type), intent(in) :: filter
      type(SGS_filter_area_params), intent(inout) :: f_area
!
      integer(kind = kint) :: i, j
!
      do i = 1, f_area%num_f_group
        do j = 1, filter%ngrp_node
          if (f_area%f_gourp_name(i) .eq. filter%group_name(j) ) then
            f_area%id_f_group(i) = j
            exit
          end if
        end do
      end do
!
      if (iflag_debug .eq. 1) then
        write(*,*)  f_area%num_f_group
        do i = 1, f_area%num_f_group
          write(*,*) i,f_area%id_f_group(i),                            &
     &           trim(f_area%f_gourp_name(i))
        end do
      end if
!
      end subroutine set_each_filtering_group_id
!
! ----------------------------------------------------------------------
!
      end module set_3d_filtering_group_id
