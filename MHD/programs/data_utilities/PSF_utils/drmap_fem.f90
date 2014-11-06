      program drmap_fem
!*********************************************************************
!*
!*     program for draw equationnal plane
!*
!*********************************************************************
!*
      use m_precision
!
      use m_constants
      use m_ctl_param_plot_pg
      use m_ctl_data_plot_pg
      use m_drawpg_fem
      use m_isoline_dat_pg
      use m_psf_results
      use set_control_draw_pg
      use set_components_flags
      use contour_fem_pg
      use drcap_pg
      use draw_colorbar_pg
      use load_psf_data
      use set_parallel_file_name
      use set_map_from_1patch
      use draw_mapframe_pg
      use map_contour_fem_pg
!
      use set_zplane_posi_pg
      use rbcolor_pg
!
      implicit none
!
!
      integer(kind = kint) :: iw, iw2, iw3
      integer(kind = kint) :: istep, i_field, ist_comp
!
      real(kind= kreal), parameter :: xframe = 2.25, yframe = 1.125
!
      integer :: pgopen, id1
!*
!*  ---------  input setting  ------------
!*
      call read_control_draw_pg
!
      call s_set_control_draw_pg
      call set_control_draw_zplane
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
      do istep = ist_pg, ied_pg, inc_pg
        time = dble(istep)*delta_time_pg
!*
        call s_load_psf_data(istep)
        call set_psffield_id_4_plot_pg
!
!     convert to map data
!
!          do iele = 1, numele_psf
!            write(*,*) 'set_map_patch_from_1patch', iele
!            call set_map_patch_from_1patch(iele, icomp)
!          end do
!
          nnod_pg =     numnod_psf
          nele_pg =     numele_psf
          nnod_ele_pg = ithree
          call allocate_pg_nodes
          call allocate_pg_connect
          call allocate_pg_data
!
!   set plotting data
!
          do iw = 1, ntot_plot_pg
            i_field = id_field_4_plot(iw)
            ist_comp = istack_comp_psf(i_field-1)                       &
     &                + mod(id_comp_4_plot(iw),10)
            if (id_comp_4_plot(iw) .eq. icomp_VECTOR                    &
     &           .and. num_comp_4_plot(iw) .eq. ncomp_VECTOR) then
              call set_map_vector(numnod_psf, d_nod_psf(1,ist_comp),    &
     &            vect_pg, cont_pg)
!
              if ( scale_pg(iw).eq.0.0d0 ) then
                call cal_drawvec_maxlength(numnod_psf, vect_pg, maxlen)
              else
                maxlen = scale_pg(iw)
              end if
!
            else
              call set_zplane_scalar(numnod_psf, d_nod_psf(1,ist_comp), &
     &            cont_pg)
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
            call pgsch(1.2)
            call pgvstd
            call pgenv(-xframe, xframe, -yframe ,yframe*1.2, 1, -2)
!*
            if (idisp_mode .ge. 2 ) then
              call pgsci(0)
              call pgscr( 15, 1.0, 1.0, 1.0 )
              call pgrect(-xframe, xframe, -yframe ,yframe*1.2)
            end if
!
!   ---------  set colormap  ----------------------------
!
            call set_colormap_pg(icolor_mode, num_color_pg)
!
!*  -------   write label of the graph ---------------------
!*
            if( field_label_4_plot(iw) .ne. '') then
              call drcap(npanel_window, iw, time,                       &
     &               field_label_4_plot(iw))
            end if
            call drcap_map_radius(npanel_window, iw, r_sph)
!
!
!   ---  fill vector ----------------------------------------
!
            if (id_comp_4_plot(iw) .eq. icomp_VECTOR                    &
     &         .and. num_comp_4_plot(iw) .eq. ncomp_VECTOR) then
!
!   ------- draw flame  ---------------------------------
!
              call vecmap_frame
              call draw_vc_map(nnod_pg, ione, nnod_pg,                  &
     &            nskip_vect_pg(iw), xx_psf, vect_pg, maxlen)
!*
!   ---  fill patch ----------------------------------------
            else
!
!  --------  set line value
!
              call set_linevalue(xmax_pg, xmin_pg, num_line_pg(iw))
!
!   ------- draw patch  ---------------------------------
!
              call fill_tri_map(idisp_mode, icolor_mode, num_color_pg,  &
     &            nnod_pg, nele_pg, xx_psf, ie_psf, cont_pg,            &
     &            xmax_pg, xmin_pg)
!
!   ------- draw flame  ---------------------------------
!
              call mapframe
!
!    -------  draw line -----------------------------------
!
              call drawline_map_fem(idisp_mode, icolor_mode,            &
     &            num_color_pg, nnod_pg, nele_pg, xx_psf, ie_psf,       &
     &            cont_pg, num_line_pg(iw), xc, xmax_pg, xmin_pg)
!
!  ----------  draw colorbar
              call draw_colorbar(idisp_mode, icolor_mode, num_color_pg, &
     &            num_line_pg(iw), xmin_pg, xmax_pg, xc,                &
     &            (xframe*0.87d0), (yframe*0.9d0) )
!
            end if
          end do
!
          call deallocate_psf_results
!
          call deallocate_pg_data
          call deallocate_pg_grid
!
      end do
!*
      call pgclos
!*
      stop
      end program drmap_fem
