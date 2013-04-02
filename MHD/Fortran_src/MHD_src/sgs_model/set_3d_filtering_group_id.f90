!
!      module set_3d_filtering_group_id
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine s_set_3d_filtering_group_id
!      subroutine s_set_w_filtering_group_id
!
      module set_3d_filtering_group_id
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_3d_filtering_group_id
!
      use m_filter_coef_combained
!
      integer(kind = kint) :: i, j
!
      do i = 1, num_whole_filter_grp
        do j = 1, ngrp_nod_3d_filter
          if (whole_filter_grp(i) .eq. grp_name_3d_filter(j) ) then
            id_whole_filter_grp(i) = j
            exit
          end if
        end do
      end do
!
      do i = 1, num_fluid_filter_grp
        do j = 1, ngrp_nod_3d_filter
          if (fluid_filter_grp(i) .eq. grp_name_3d_filter(j) ) then
            id_fluid_filter_grp(i) = j
            exit
          end if
        end do
      end do
!
      if (iflag_debug.eq.1) then
        write(*,*) 'igrp, id_whole_filter_grp, whole_filter_grp',       &
     &            num_whole_filter_grp
        do i = 1, num_whole_filter_grp
          write(*,*) i,id_whole_filter_grp(i),trim(whole_filter_grp(i))
        end do
        write(*,*) 'igrp, id_fluid_filter_grp, fluid_filter_grp',       &
     &            num_fluid_filter_grp
        do i = 1, num_fluid_filter_grp
          write(*,*) i,id_fluid_filter_grp(i),trim(fluid_filter_grp(i))
        end do
      end if
!
      end subroutine s_set_3d_filtering_group_id
!
! ----------------------------------------------------------------------
!
      subroutine s_set_w_filtering_group_id
!
      use m_3d_w_filter_coef
!
      integer(kind = kint) :: i, j
!
      do i = 1, num_whole_w_filter_grp
        do j = 1, ngrp_nod_3d_w_fil
          if (whole_w_filter_grp(i) .eq. grp_name_3d_w_fil(j) ) then
            id_whole_w_filter_grp(i) = j
            exit
          end if
        end do
      end do
!
      do i = 1, num_fluid_w_filter_grp
        do j = 1, ngrp_nod_3d_w_fil
          if (fluid_w_filter_grp(i) .eq. grp_name_3d_w_fil(j) ) then
            id_fluid_w_filter_grp(i) = j
            exit
          end if
        end do
      end do
!
      if (iflag_debug.eq.1) then
        write(*,*) 'grp_name_3d_w_fil', grp_name_3d_w_fil
        write(*,*) 'igrp, id_whole_w_filter_grp, whole_w_filter_grp',   &
     &            num_whole_w_filter_grp
        do i = 1, num_whole_w_filter_grp
          write(*,*) i, id_whole_w_filter_grp(i),                       &
     &         trim(whole_w_filter_grp(i))
        end do
        write(*,*) 'igrp, id_fluid_w_filter_grp, fluid_w_filter_grp',   &
     &            num_fluid_w_filter_grp
        do i = 1, num_fluid_w_filter_grp
          write(*,*) i, id_fluid_w_filter_grp(i),                       &
     &         trim(fluid_w_filter_grp(i))
        end do
      end if
!
      end subroutine s_set_w_filtering_group_id
!
! ----------------------------------------------------------------------
!
      end module set_3d_filtering_group_id
