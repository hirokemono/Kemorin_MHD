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
      use set_components_flags
!
      integer(kind= kint) :: iint, i, j
!
!
      if(i_time_init .gt. 0) then
        start_time_pg = time_init_ctl
      else
        start_time_pg = 0.0d0
      end if
!
      if(i_dt .gt. 0) then
        delta_time_pg = dt_ctl
      else
        delta_time_pg = 0.0d0
      end if
!
      if(i_i_step_init .gt. 0) then
        ist_pg = i_step_init_ctl
      else
        ist_pg = 1
      end if
!
      if(i_i_step_number .gt. 0) then
        ied_pg = i_step_number_ctl
      else
        ied_pg = 1
      end if
!
      if(i_i_step_psf .gt. 0) then
        inc_pg = i_step_psf_ctl
      else
        inc_pg = 1
      end if
!
!
      if(i_num_panels_ctl .gt. 0) then
        npanel_window = num_panels_ctl
      else
        npanel_window = 1
      end if
!
!
      if     (contour_type_ctl.eq.'both'                                &
     &   .or. contour_type_ctl.eq.'Both'                                &
     &   .or. contour_type_ctl.eq.'BOTH'                                &
     &   .or. contour_type_ctl.eq.'line_and_fill'                       &
     &   .or. contour_type_ctl.eq.'Line_and_Fill'                       &
     &   .or. contour_type_ctl.eq.'LINE_and_FILL'                       &
     &   .or. contour_type_ctl.eq.'LINE_AND_FILL') then
        idisp_mode = 3
      else if(contour_type_ctl.eq.'fill'                                &
     &   .or. contour_type_ctl.eq.'Fill'                                &
     &   .or. contour_type_ctl.eq.'FILL'                                &
     &   .or. contour_type_ctl.eq.'filled'                              &
     &   .or. contour_type_ctl.eq.'Filled'                              &
     &   .or. contour_type_ctl.eq.'FILLED') then
        idisp_mode = 2
      else
        idisp_mode = 1
      end if
!
!
      if     (color_mode_ctl.eq.'rainbow'                               &
     &   .or. color_mode_ctl.eq.'Rainbow'                               &
     &   .or. color_mode_ctl.eq.'RAINBOW'                               &
     &   .or. color_mode_ctl.eq.'color'                                 &
     &   .or. color_mode_ctl.eq.'Color'                                 &
     &   .or. color_mode_ctl.eq.'COLOR') then
        icolor_mode = 1
      else if(color_mode_ctl.eq.'yellow_green'                          &
     &   .or. color_mode_ctl.eq.'Yellow_Green'                          &
     &   .or. color_mode_ctl.eq.'yellow_green') then
        icolor_mode = -1
      else if(color_mode_ctl.eq.'grayscale'                             &
     &   .or. color_mode_ctl.eq.'Grayscale'                             &
     &   .or. color_mode_ctl.eq.'GRAYSCALE'                             &
     &   .or. color_mode_ctl.eq.'gray'                                  &
     &   .or. color_mode_ctl.eq.'Gray'                                  &
     &   .or. color_mode_ctl.eq.'GRAY') then
        icolor_mode = 0
      else
        icolor_mode = 1
      end if
!
!
      if(i_psf_data_ctl .gt. 0) then
        fhead_plot_data = psf_file_head_ctl
      else
        write(*,*) 'set file header for psf data'
        stop
      end if
      call choose_ucd_file_format(psf_data_fmt_ctl,                     &
     &    i_psf_data_fmt_ctl, itype_draw_psf_file )
!
      if(i_map_grid_file .gt. 0) then
        fhead_map_grid =  map_grid_file_ctl
      end if
!
      if(i_ntot_plotting_ctl .gt. 0) then
        ntot_plot_pg = ntot_plotting_ctl
      else
        write(*,*) 'set number of component to plot'
        stop
      end if
!
      call allocate_plot_param_pg
!
        field_name_4_plot(1:ntot_plot_pg)                               &
     &       = plot_field_ctl(1:ntot_plot_pg)
        comp_name_4_plot(1:ntot_plot_pg)                                &
     &       = plot_comp_ctl(1:ntot_plot_pg)
        field_label_4_plot(1:ntot_plot_pg)                              &
     &       = plot_label_ctl(1:ntot_plot_pg)
!
      do i = 1, ntot_plot_pg
        call s_set_components_flags( comp_name_4_plot(i),               &
     &      field_name_4_plot(i), id_comp_4_plot(i),                    &
     &      num_comp_4_plot(i), ncomp_org_4_plot(i),                    &
     &      viz_name_4_plot(i) )
      end do
!
!
        write(*,*) 'ntot_range_ctl', ntot_range_ctl, num_line_ctl
      if(ntot_range_ctl .gt. 0) then
        do i = 1, ntot_range_ctl
          j = id_4_contour_ctl(i)
          num_line_pg(j) =  num_line_ctl(i)
          range_pg(1:2,j) = contour_range_ctl(i,1:2)
        end do
      end if
!
      do i = 1, ntot_range_ctl
        nmax_line = max(num_line_ctl(i),nmax_line)
      end do
      call allocate_data_4_isoline
!
      if(ntot_scale_ctl .gt. 0) then
        do i = 1, ntot_scale_ctl
          j = id_4_vector_ctl(i)
          nskip_vect_pg(j) =  nskip_vect_ctl(i)
          scale_pg(j) = vector_scale_ctl(i)
        end do
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
        write(*,*) 'itype_draw_psf_file',  itype_draw_psf_file
        write(*,*) 'fhead_map_grid',  trim(fhead_map_grid)
        write(*,*) 'fhead_plot_data', trim(fhead_plot_data)
        do i = 1, ntot_plot_pg
          write(*,*) 'field_name', i, trim(field_name_4_plot(i))
          write(*,*) 'comp_name', trim(comp_name_4_plot(i))
          write(*,*) 'field_label', trim(field_label_4_plot(i))
          write(*,*) 'viz_name_4_plot', trim(viz_name_4_plot(i))
          write(*,*) 'viz_ids', id_comp_4_plot(i), num_comp_4_plot(i),  &
     &               ncomp_org_4_plot(i)
          write(*,*)
        end do
        do i = 1, ntot_scale_ctl
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
      if(i_plane_size_ctl .gt. 0) then
        plane_size(1:2) = plane_size_ctl(1:2)
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
      use m_ctl_data_plot_pg
!
!
      if(i_sph_grid_type .gt. 0) then
        if      (sph_grid_type_ctl .eq. 'no_pole'                       &
     &      .or. sph_grid_type_ctl .eq. 'No_pole'                       &
     &      .or. sph_grid_type_ctl .eq. 'NO_POLE') then
          id_shell_mode_pg = 1
        else if (sph_grid_type_ctl .eq. 'with_pole'                     &
     &      .or. sph_grid_type_ctl .eq. 'With_pole'                     &
     &      .or. sph_grid_type_ctl .eq. 'WITH_POLE') then
          id_shell_mode_pg = 2
        else if (sph_grid_type_ctl .eq. 'with_center'                   &
     &      .or. sph_grid_type_ctl .eq. 'With_center'                   &
     &      .or. sph_grid_type_ctl .eq. 'WITH_CENTER') then
          id_shell_mode_pg = 3
        else
          id_shell_mode_pg = 1
        end if
      else
          id_shell_mode_pg = 1
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
