!set_sleeve_nod_peri_cube.f90
!     module set_sleeve_nod_peri_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine set_sleeve_node_peri(ipe, jpe, kpe, inod)
!      subroutine set_sleeve_node_peri_quad(ipe, jpe, kpe, inod)
!
      module set_sleeve_nod_peri_cube
!
      use m_precision
!
      use m_size_4_plane
      use m_size_of_cube
      use m_neighb_range_cube
      use m_sleeve_cube
      use m_sleeve_nod_side_cube
      use m_sleeve_nod_corner_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sleeve_node_peri(ipe, jpe, kpe, inod)
!
      integer (kind = kint) :: ipe, jpe, kpe
      integer (kind = kint) :: inod
!
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inp, jnp, knp
!
!  outdside (x<xmin)
!
            ioff_gl = nod_gltot
            if (ipe .eq. 1 ) then
             do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end

               call set_sleeve_size(inp, jnp, knp)
               is = 1
               ie = ndepth
!
               call set_sleeve_node_xmin(inod, ioff_gl)

              enddo
             enddo
            endif

!
!  outdside (x>xmax)
!
            ioff_gl = nod_gltot + ndepth*ny_all*nz_all
            if (ipe .eq. ndx ) then
             do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end

               call set_sleeve_size(inp, jnp, knp)
               is = 1
               ie = ndepth

               call set_sleeve_node_xmax(inod, ioff_gl)
!
              enddo
             enddo
            endif

!
!  outdside (y<ymin)
!
            ioff_gl = nod_gltot + 2*ndepth*ny_all*nz_all
            if ( jpe .eq. 1 ) then
             do knp=knp_st,knp_end
              do inp=inp_st,inp_end

               call set_sleeve_size(inp, jnp, knp)
               js = 1
               je = ndepth

               call set_sleeve_node_ymin(inod, ioff_gl)

              enddo
             enddo
            endif
!
!  outdside (y<ymax)
!
            ioff_gl = nod_gltot                                         &
     &               + 2*ndepth*ny_all*nz_all + ndepth*nx_all*nz_all 
            if ( jpe .eq. ndy ) then
             do knp=knp_st,knp_end
              do inp=inp_st,inp_end

               call set_sleeve_size(inp, jnp, knp)
               js = 1
               je = ndepth

               call set_sleeve_node_ymax(inod, ioff_gl)

              enddo
             enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            ioff_gl = nod_gltot                                         &
     &               + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all 
            if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
             do knp=knp_st,knp_end

               call set_sleeve_size(inp, jnp, knp)

               call set_sleeve_node_xmin_ymin(inod, ioff_gl)

             enddo
            endif
!
!  outdside (x>xmax, y<ymin)
!
            ioff_gl = nod_gltot                                         &
     &               + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all  &
     &               + ndepth*ndepth*nz_all
            if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
             do knp=knp_st,knp_end

               call set_sleeve_size(inp, jnp, knp)

               call set_sleeve_node_xmax_ymin(inod, ioff_gl)

             enddo
            end if
!
!  outdside (x>xmax, y>ymax)
!
            ioff_gl = nod_gltot                                         &
     &               + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all  &
     &               + 2*ndepth*ndepth*nz_all
            if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
             do knp=knp_st,knp_end
!                                       .. start side

               call set_sleeve_size(inp, jnp, knp)

               call set_sleeve_node_xmax_ymax(inod, ioff_gl)

             enddo
            end if
!
!  outdside (x<xmax, y<ymax)
!
            ioff_gl = nod_gltot                                         &
     &               + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all  &
     &               + 3*ndepth*ndepth*nz_all
            if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
             do knp=knp_st,knp_end

               call set_sleeve_size(inp, jnp, knp)

               call set_sleeve_node_xmin_ymax(inod, ioff_gl)

              enddo
            end if
!
      end subroutine set_sleeve_node_peri
!
! ----------------------------------------------------------------------
!
      subroutine set_sleeve_node_peri_quad(ipe, jpe, kpe, inod)
!
      integer (kind = kint) :: ipe, jpe, kpe
      integer (kind = kint) :: inod
!
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inp, jnp, knp
!
! ***** set and write for sleeve area nodes for periodical boundary
!  outdside (x<xmin)
!
            ioff_gl = nod_gltot + edge_gltot
            if (ipe .eq. 1 ) then
             do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end
!
               call set_sleeve_size(inp, jnp, knp)
               is = 1
               ie = ndepth
               call set_sleeve_node_xmin(inod, ioff_gl)

              enddo
             enddo
            endif
!
!  outdside (x>xmax)
!
            ioff_gl = nod_gltot + edge_gltot + ndepth*ny_all*nz_all
            if (ipe .eq. ndx ) then
             do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end

               call set_sleeve_size(inp, jnp, knp)
               is = 1
               ie = ndepth
               call set_sleeve_node_xmax(inod, ioff_gl)

              enddo
             enddo
            endif
!
!  outdside (y<ymin)
!
            ioff_gl = nod_gltot + edge_gltot + 2*ndepth*ny_all*nz_all
            if ( jpe .eq. 1 ) then
             do knp=knp_st,knp_end
              do inp=inp_st,inp_end

               call set_sleeve_size(inp, jnp, knp)
               js = 1
               je = ndepth
               call set_sleeve_node_ymin(inod, ioff_gl)

              enddo
             enddo
            endif
!
!  outdside (y<ymax)
!
            ioff_gl = nod_gltot + edge_gltot                            &
     &               + 2*ndepth*ny_all*nz_all + ndepth*nx_all*nz_all 
            if ( jpe .eq. ndy ) then
             do knp=knp_st,knp_end
              do inp=inp_st,inp_end

               call set_sleeve_size(inp, jnp, knp)
               js = 1
               je = ndepth
               call set_sleeve_node_ymax(inod, ioff_gl)

              enddo
             enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            ioff_gl = nod_gltot + edge_gltot                            &
     &               + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all 
            if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
             do knp=knp_st,knp_end

               call set_sleeve_size(inp, jnp, knp)
               call set_sleeve_node_xmin_ymin(inod, ioff_gl)

             enddo
            endif
!
!  outdside (x>xmax, y<ymin)
!
            ioff_gl = nod_gltot + edge_gltot                            &
     &               + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all  &
     &               + ndepth*ndepth*nz_all
            if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
             do knp=knp_st,knp_end

               call set_sleeve_size(inp, jnp, knp)
               call set_sleeve_node_xmax_ymin(inod, ioff_gl)

            enddo
           end if
!
!  outdside (x>xmax, y>ymax)
!
            ioff_gl = nod_gltot + edge_gltot                            &
     &               + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all  &
     &               + 2*ndepth*ndepth*nz_all
            if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
             do knp=knp_st,knp_end
!                                       .. start side

               call set_sleeve_size(inp, jnp, knp)
               call set_sleeve_node_xmax_ymax(inod, ioff_gl)

             enddo
            end if
!
!  outdside (x<xmax, y<ymax)
!
            ioff_gl = nod_gltot + edge_gltot                            &
     &               + 2*ndepth*ny_all*nz_all + 2*ndepth*nx_all*nz_all  &
     &               + 3*ndepth*ndepth*nz_all
            if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
             do knp=knp_st,knp_end

               call set_sleeve_size(inp, jnp, knp)
               call set_sleeve_node_xmin_ymax(inod, ioff_gl)

             enddo
            end if
!
      end subroutine set_sleeve_node_peri_quad
!
! ----------------------------------------------------------------------
!
      end module set_sleeve_nod_peri_cube
