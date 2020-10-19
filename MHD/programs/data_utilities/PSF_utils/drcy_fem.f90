      program drcy_fem
!*********************************************************************
!*
!*     program for draw const_x plane
!*
!*********************************************************************
!*
      use m_precision
!
      use m_constants
      use m_ctl_param_plot_pg
      use m_drawpg_fem
      use m_isoline_dat_pg
      use m_psf_results
      use t_ctl_data_plot_pg
      use set_control_draw_pg
      use set_components_flags
      use contour_fem_pg
      use drcap_pg
      use draw_colorbar_pg
      use set_zplane_posi_pg
      use set_parallel_file_name
      use rbcolor_pg
!
!
      implicit none
!
      type(controls_with_pgplot), save :: pg_ctl1
!
      integer(kind = kint) :: iw, iw2, iw3
      integer(kind = kint) :: istep,  i_field, ist_comp
!
      integer :: pgopen, id1
      real(kind = kreal) :: fv(2,2)
!
      real :: r_flame
!*
!*  ---------  input setting  ------------
!*
      call read_control_draw_pg(pg_ctl1)
!
      call set_step_control_draw_pg(pg_ctl1%t_pg_ctl)
      call s_set_control_draw_pg                                        &
     &   (pg_ctl1%pg_panel_ctl, pg_ctl1%pg_fld_ctl)
      call set_control_draw_zplane(pg_ctl1%pg_section_ctl)
      call dealloc_plot_ctl_data(pg_ctl1)
!
!*
!* ----------open pgplot  ----------------
!*
      id1 = pgopen('?')
      if (id1 .le. 0 )then
        stop
      endif
!*
      if ( npanel_window .le. 2 ) then
        iw3 = npanel_window
        call pgsubp(npanel_window,1)
      else
        iw2 = mod(npanel_window,2)                                      &
     &       + ( npanel_window - mod(npanel_window,2) ) / 2
        iw3 = iw2*2
        call pgsubp(iw2,2)
      endif
!*
!
!*  ----------  main loop for graphic  ---------------
!*
      psf_file_param%file_prefix = pg_psf_file_prefix
      psf_file_param%iflag_format = iflag_pg_psf_fmt
      do istep = ist_pg, ied_pg, inc_pg
        time = dble(istep)*delta_time_pg
!*
        call load_psf_data(istep, psf_file_param, t_IO_u, psf_u)
        call set_psffield_id_4_plot_pg(psf_u%psf_phys)
!
        nnod_pg =     psf_u%psf_nod%numnod
        nele_pg =     psf_u%psf_ele%numele
        nnod_ele_pg = ithree
        call allocate_pg_nodes
        call allocate_pg_connect
        call allocate_pg_data
!
!  convert to view cordinate
!
        call set_yplane_graph_position                                  &
     &     (psf_u%psf_nod%numnod, psf_u%psf_ele%numele, shell_size,     &
     &      flame, psf_u%psf_nod%xx, psf_u%psf_ele%ie, xg, ie_pg)
        r_flame = real(flame)
!
!   set plotting data
!
          do iw = 1, ntot_plot_pg
            i_field = id_field_4_plot(iw)
            ist_comp = psf_u%psf_phys%istack_component(i_field-1)       &
     &                + mod(id_comp_4_plot(iw),10)
            if (   id_comp_4_plot(iw) .eq. icomp_VECTOR                 &
              .or. id_comp_4_plot(iw) .eq. icomp_SPH_VECTOR             &
              .or. id_comp_4_plot(iw) .eq. icomp_CYL_VECTOR) then
              ist_comp = psf_u%psf_phys%istack_component(i_field-1) + 1
              call set_yplane_vector(id_comp_4_plot(iw),                &
     &            psf_u%psf_nod%numnod, psf_u%psf_nod%xx,               &
     &            psf_u%psf_phys%ntot_phys, ist_comp,                   &
     &            psf_u%psf_phys%d_fld, vect_pg, cont_pg)
!
              if ( scale_pg(iw).eq.0.0d0 ) then
                call cal_drawvec_maxlength(nnod_pg, vect_pg, maxlen)
              else
                maxlen = scale_pg(iw)
              end if
!
            else
              cont_pg(1:psf_u%psf_nod%numnod)                           &
     &          = psf_u%psf_phys%d_fld(1:psf_u%psf_nod%numnod,ist_comp)
!
              if ( range_pg(1,iw).eq.0.0d0                              &
     &        .and. range_pg(2,iw).eq.0.0d0 ) then
                call cal_drawcont_minmax(nnod_pg, cont_pg,              &
     &              xmin_pg, xmax_pg)
              else
                xmin_pg = range_pg(1,iw)
                xmax_pg = range_pg(2,iw)
              end if
            end if
!
!* ---------  open the window of graph  -----------------
!*
            write(*,*) 'flame', flame, -r_flame ,r_flame
            call pgsch(1.2)
            call pgvstd
            call pgenv(0.0, r_flame, -r_flame ,r_flame, 1, -2)
!*
            if (idisp_mode .ge. 2 ) then
              call pgsci(0)
              call pgscr( 15, 1.0, 1.0, 1.0 )
              call pgrect(0.0, r_flame, -r_flame, r_flame)
            end if
!
!   ---------  set colormap  ----------------------------
!
            call set_colormap_pg(icolor_mode, num_color_pg)
!
!   ---------   show axis  ----------------------------
!
            call draw_axis_yplane(flame)
!
!  -------   write label of the graph ---------------------
!
            if( field_label_4_plot(iw) .ne. '') then
              call drcap(npanel_window, iw, time,                       &
     &               field_label_4_plot(iw))
            end if
            call drcap_xplane(npanel_window, iw, psf_u%psf_nod%xx(1,1))
!
            if (   id_comp_4_plot(iw) .eq. icomp_VECTOR                 &
              .or. id_comp_4_plot(iw) .eq. icomp_SPH_VECTOR             &
              .or. id_comp_4_plot(iw) .eq. icomp_CYL_VECTOR) then
!
!   ---  fill vector ----------------------------------------
!
              fv(1,1) =  1.8
              fv(2,1) = -1.8
              fv(1,2) =  0.4
              fv(2,2) =  0.3
              call draw_vc (nnod_pg, ione, nnod_pg,                     &
     &            nskip_vect_pg(iw), xg, vect_pg, maxlen, fv)
!
!   ------- draw flame  ---------------------------------
              call zcframe
!
            else
!
!  --------  set line value
!
              call set_linevalue(xmax_pg, xmin_pg, num_line_pg(iw))
!
!   ---  fill patch ----------------------------------------
!
              call fill_tri_pg(idisp_mode, icolor_mode, num_color_pg,   &
     &            nnod_pg, nele_pg, xg, ie_pg, cont_pg,                 &
     &            xmax_pg, xmin_pg)
!
!   ------- draw flame  ---------------------------------
!
              call mdframe
!
!    -------  draw line -----------------------------------
!
              call drawline_pg(idisp_mode, icolor_mode, num_color_pg,   &
     &            nnod_pg, nele_pg, xg, ie_pg, cont_pg,                 &
     &            num_line_pg(iw), xc, xmax_pg, xmin_pg)
!
!  ----------  draw colorbar
!
              call draw_colorbar(idisp_mode, icolor_mode, num_color_pg, &
     &            num_line_pg(iw), xmin_pg, xmax_pg, xc,                &
     &            (flame*0.7), (flame*0.85) )
!
            end if
          end do
!
          call dealloc_psf_results(psf_u)
!
          call deallocate_pg_data
          call deallocate_pg_grid
!
      end do
!*
      call pgclos
!*
      stop
      end
