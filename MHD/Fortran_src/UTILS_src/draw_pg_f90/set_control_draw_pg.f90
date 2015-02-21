!
!      module set_control_draw_pg
!
      module set_control_draw_pg
!
!      Written by H. Matsui
!
      use m_precision
!
      use m_machine_parameter
      use m_ctl_param_plot_pg
!
      implicit none
!
!      subroutine s_set_control_draw_pg
!      subroutine set_control_draw_zplane
!      subroutine set_control_draw_map
!      subroutine set_psffield_id_4_plot_pg
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_control_draw_pg
!
      use m_ctl_data_plot_pg
      use m_ctl_data_4_time_steps
      use m_isoline_dat_pg
      use m_file_format_switch
      use m_field_file_format
      use m_psf_results
      use set_components_flags
      use skip_comment_f
!
      integer(kind= kint) :: i, j
!
!
      start_time_pg = 0.0d0
      if(time_init_ctl%iflag .gt. 0) then
        start_time_pg = time_init_ctl%realvalue
      end if
!
      delta_time_pg = 0.0d0
      if(dt_ctl%iflag .gt. 0) then
        delta_time_pg = dt_ctl%realvalue
      end if
!
      ist_pg = 1
      if(i_step_init_ctl%iflag .gt. 0) then
        ist_pg = i_step_init_ctl%intvalue
      end if
!
      ied_pg = 1
      if(i_step_number_ctl%iflag .gt. 0) then
        ied_pg = i_step_number_ctl%intvalue
      end if
!
      inc_pg = 1
      if(i_step_psf_ctl%iflag .gt. 0) then
        inc_pg = i_step_psf_ctl%intvalue
      end if
!
!
      npanel_window = 1
      if(i_num_panels_ctl .gt. 0) then
        npanel_window = num_panels_ctl
      end if
!
!
      if     (cmp_no_case(contour_type_ctl, 'Both')                     &
     &   .or. cmp_no_case(contour_type_ctl, 'Line_and_Fill')) then
        idisp_mode = 3
      else if(cmp_no_case(contour_type_ctl, 'Fill')                     &
     &   .or. cmp_no_case(contour_type_ctl, 'Filled')       ) then
        idisp_mode = 2
      else
        idisp_mode = 1
      end if
!
!
      if     (cmp_no_case(contour_type_ctl, 'Rainbow')                  &
     &   .or. cmp_no_case(contour_type_ctl, 'Color')        ) then
        icolor_mode = 1
      else if(cmp_no_case(contour_type_ctl, 'Yellow_Green') ) then
        icolor_mode = -1
      else if(cmp_no_case(contour_type_ctl, 'Grayscale')                &
     &   .or. cmp_no_case(contour_type_ctl, 'Gray')         ) then
        icolor_mode = 0
      else
        icolor_mode = 1
      end if
!
!
      if(i_psf_data_ctl .gt. 0) then
        psf_file_header = psf_file_head_ctl
      else
        write(*,*) 'set file header for psf data'
        stop
      end if
      call choose_ucd_file_format(psf_data_fmt_ctl,                     &
     &    i_psf_data_fmt_ctl, iflag_psf_fmt)
!
      if(i_map_grid_file .gt. 0) then
        fhead_map_grid =  map_grid_file_ctl
      end if
!
      if(plot_field_ctl%icou .gt. 0) then
        ntot_plot_pg = plot_field_ctl%num
      else
        write(*,*) 'set number of component to plot'
        stop
      end if
!
      call allocate_plot_param_pg
!
        field_name_4_plot(1:ntot_plot_pg)                               &
     &       = plot_field_ctl%c1_tbl(1:ntot_plot_pg)
        comp_name_4_plot(1:ntot_plot_pg)                                &
     &       = plot_field_ctl%c2_tbl(1:ntot_plot_pg)
        field_label_4_plot(1:ntot_plot_pg)                              &
     &       = plot_field_ctl%c3_tbl(1:ntot_plot_pg)
!
      do i = 1, ntot_plot_pg
        call s_set_components_flags( comp_name_4_plot(i),               &
     &      field_name_4_plot(i), id_comp_4_plot(i),                    &
     &      num_comp_4_plot(i), ncomp_org_4_plot(i),                    &
     &      viz_name_4_plot(i) )
      end do
!
!
      if(contour_range_ctl%num .gt. 0) then
        do i = 1, contour_range_ctl%num
          j = contour_range_ctl%int1(i)
          num_line_pg(j) =  contour_range_ctl%int2(i)
          range_pg(1,j) =   contour_range_ctl%vec1(i)
          range_pg(2,j) =   contour_range_ctl%vec2(i)
        end do
!
        do i = 1, contour_range_ctl%num
          nmax_line = max(contour_range_ctl%int2(i),nmax_line)
        end do
        call dealloc_control_array_i2_r2(contour_range_ctl)
      end if
!
      call allocate_data_4_isoline
!
      if(vector_scale_ctl%num .gt. 0) then
        do i = 1, vector_scale_ctl%num
          j = vector_scale_ctl%int1(i)
          nskip_vect_pg(j) =  vector_scale_ctl%int2(i)
          scale_pg(j) = vector_scale_ctl%vect(i)
        end do
!
        call dealloc_control_array_i2_r(vector_scale_ctl)
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'npanel_window', npanel_window
        write(*,*) 'idisp_mode',    idisp_mode
        write(*,*) 'icolor_mode',   icolor_mode
        write(*,*) 'ist_pg',        ist_pg
        write(*,*) 'ied_pg',        ied_pg
        write(*,*) 'inc_pg',        inc_pg
!
        write(*,*) 'nmax_line',            nmax_line
        write(*,*) 'iflag_psf_fmt',   iflag_psf_fmt
        write(*,*) 'fhead_map_grid',  trim(fhead_map_grid)
        write(*,*) 'psf_file_header', trim(psf_file_header)
        do i = 1, ntot_plot_pg
          write(*,*) 'field_name', i, trim(field_name_4_plot(i))
          write(*,*) 'comp_name', trim(comp_name_4_plot(i))
          write(*,*) 'field_label', trim(field_label_4_plot(i))
          write(*,*) 'viz_name_4_plot', trim(viz_name_4_plot(i))
          write(*,*) 'viz_ids', id_comp_4_plot(i), num_comp_4_plot(i),  &
     &               ncomp_org_4_plot(i)
          write(*,*)
        end do
        do i = 1, vector_scale_ctl%num
          write(*,*) 'interval, scale', i, nskip_vect_pg(i), scale_pg(i)
        end do
      end if
!
      end subroutine s_set_control_draw_pg
!
!-----------------------------------------------------------------------
!
      subroutine set_control_draw_zplane
!
      use m_ctl_data_plot_pg
!
!
      if(i_outer_radius_ctl .gt. 0) then
        shell_size = outer_radius_ctl
      else
        shell_size = 20.0d0 / 13.0d0
      end if
!
      if(i_ro_ri_ratio_ctl .gt. 0) then
        shell_ratio = ro_ri_ratio_ctl
      else
        shell_ratio = 0.35d0
      end if
!
      if(pg_plane_size_ctl%iflag .gt. 0) then
        plane_size(1:2) = pg_plane_size_ctl%realvalue(1:2)
      else
        plane_size(1:2) = 1.0d0
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'shell_size', shell_size
        write(*,*) 'shell_ratio', shell_ratio
      end if
      if (iflag_debug .gt. 0) write(*,*) 'plane_size', plane_size(1:2)
!
      end subroutine set_control_draw_zplane
!
!-----------------------------------------------------------------------
!
      subroutine set_control_draw_map
!
      use m_spheric_constants
      use m_ctl_data_plot_pg
      use skip_comment_f
!
!
      if(i_sph_grid_type .gt. 0) then
        if     (cmp_no_case(sph_grid_type_ctl, 'No_pole'   )) then
          id_shell_mode_pg = iflag_MESH_same
        else if(cmp_no_case(sph_grid_type_ctl, 'With_pole' )) then
          id_shell_mode_pg = iflag_MESH_w_pole
        else if(cmp_no_case(sph_grid_type_ctl,'With_center')) then
          id_shell_mode_pg = iflag_MESH_w_center
        else
          id_shell_mode_pg = iflag_MESH_same
        end if
      else
          id_shell_mode_pg = iflag_MESH_same
      end if
!
!
      if(i_radial_ID_ctl .gt. 0) then
        id_radial = radial_ID_ctl
      else
        id_radial = 1
      end if
!
       r_sph = 20.0d0 / 13.0d0
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'id_shell_mode_pg', id_shell_mode_pg
        write(*,*) 'id_radial', id_radial
      end if
!
      end subroutine set_control_draw_map
!
!-----------------------------------------------------------------------
!
      subroutine set_psffield_id_4_plot_pg
!
      use m_psf_results
!
      integer(kind = kint) :: iw, id
!
!
      do iw = 1, ntot_plot_pg
        do id = 1, nfield_psf
          if (field_name_4_plot(iw) .eq. psf_data_name(id)) then
            id_field_4_plot(iw) = id
            exit
          end if
        end do
      end do
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'iw, id_field_4_plot(iw), field_name_4_plot'
        do iw = 1, ntot_plot_pg
          write(*,*) iw, id_field_4_plot(iw), id_comp_4_plot(iw),       &
     &               trim(field_name_4_plot(iw))
        end do
      end if
!
      end subroutine set_psffield_id_4_plot_pg
!
!  ---------------------------------------------------------------------
!
      end module set_control_draw_pg
