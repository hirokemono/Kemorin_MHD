!>@file   t_control_data_pvr_area.f90
!!@brief  module  t_control_data_pvr_area
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_pvr_render_area_ctl                             &
!!     &         (id_control, hd_block, render_area_c, c_buf)
!!        type(pvr_render_area_ctl), intent(inout) :: render_area_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dup_pvr_render_area_ctl(org_rarea_c, new_rarea_c)
!!        type(pvr_render_area_ctl), intent(in) :: org_rarea_c
!!        type(pvr_render_area_ctl), intent(inout) :: new_rarea_c
!!      subroutine dealloc_pvr_render_area_ctl(render_area_c)
!!        type(pvr_render_area_ctl), intent(inout) :: render_area_c
!!      subroutine bcast_pvr_render_area_ctl(render_area_c)
!!        type(pvr_render_area_ctl), intent(inout) :: render_area_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  begin plot_area_ctl
!!    array chosen_ele_grp_ctl  1
!!      chosen_ele_grp_ctl   outer_core
!!    end array chosen_ele_grp_ctl
!!
!!    array surface_enhanse_ctl  2
!!      surface_enhanse_ctl   ICB   reverse_surface   0.7
!!      surface_enhanse_ctl   CMB   forward_surface   0.4
!!    end array surface_enhanse_ctl
!!  end  plot_area_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module  t_control_data_pvr_area
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_elements
      use t_control_array_character
      use t_control_array_chara2real
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_render_area_ctl
        type(ctl_array_chara) :: pvr_area_ctl
        type(ctl_array_c2r) :: surf_enhanse_ctl
!
        integer(kind = kint) :: i_plot_area = 0
      end type pvr_render_area_ctl
!
!     4th level for area group
!
      character(len=kchara) :: hd_plot_grp = 'chosen_ele_grp_ctl'
      character(len=kchara) :: hd_sf_enhanse = 'surface_enhanse_ctl'
!
      private :: hd_plot_grp, hd_sf_enhanse
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_render_area_ctl                               &
     &         (id_control, hd_block, render_area_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_render_area_ctl), intent(inout) :: render_area_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(render_area_c%i_plot_area .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_array_c1(id_control, hd_plot_grp,             &
     &      render_area_c%pvr_area_ctl, c_buf)
        call read_control_array_c2_r(id_control, hd_sf_enhanse,         &
     &       render_area_c%surf_enhanse_ctl, c_buf)
      end do
      render_area_c%i_plot_area = 1
!
      end subroutine read_pvr_render_area_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_render_area_ctl(org_rarea_c, new_rarea_c)
!
      use copy_control_elements
!
      type(pvr_render_area_ctl), intent(in) :: org_rarea_c
      type(pvr_render_area_ctl), intent(inout) :: new_rarea_c
!
!
      call dup_control_array_c1(org_rarea_c%pvr_area_ctl,               &
     &                          new_rarea_c%pvr_area_ctl)
      call dup_control_array_c2_r(org_rarea_c%surf_enhanse_ctl,         &
     &                            new_rarea_c%surf_enhanse_ctl)
!
      new_rarea_c%i_plot_area = org_rarea_c%i_plot_area
!
      end subroutine dup_pvr_render_area_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_render_area_ctl(render_area_c)
!
      type(pvr_render_area_ctl), intent(inout) :: render_area_c
!
!
      call dealloc_control_array_chara(render_area_c%pvr_area_ctl)
      call dealloc_control_array_c2_r(render_area_c%surf_enhanse_ctl)
      render_area_c%pvr_area_ctl%icou =     0
      render_area_c%surf_enhanse_ctl%icou = 0
!
      render_area_c%i_plot_area = 0
!
      end subroutine dealloc_pvr_render_area_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_render_area_ctl(render_area_c)
!
      use calypso_mpi
      use bcast_control_arrays
!
      type(pvr_render_area_ctl), intent(inout) :: render_area_c
!
!
      call MPI_BCAST(render_area_c%i_plot_area,  1,                     &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_array_c1(render_area_c%pvr_area_ctl)
      call bcast_ctl_array_c2r(render_area_c%surf_enhanse_ctl)
!
      end subroutine bcast_pvr_render_area_ctl
!
!  ---------------------------------------------------------------------
!
      end module  t_control_data_pvr_area
