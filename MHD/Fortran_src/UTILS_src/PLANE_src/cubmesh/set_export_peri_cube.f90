!set_export_peri_cube.f90
!     module set_export_peri_cube
!
      module set_export_peri_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
      use m_precision
!
      use m_size_4_plane
      use m_size_of_cube
      use m_comm_data_cube_kemo
      use m_neighb_range_cube
      use m_sleeve_cube
      use set_comm_nod_4_cube
!
      implicit none
!
!      subroutine set_export_peri(ipe, jpe, inod)
!      subroutine set_export_peri_quad(ipe, jpe, kpe, inod)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_export_peri(ipe, jpe, inod)
!
      integer (kind = kint) :: ipe, jpe
      integer (kind = kint) :: inod
!
      integer (kind = kint) :: inp, jnp, knp
!
!
            if (ipe .eq. ndx) then
             inp = 1
             do knp = nb_rng1%knp_st, nb_rng1%knp_end
              do jnp = nb_rng1%jnp_st, nb_rng1%jnp_end

               call set_boundary_size(inp, jnp, knp)
               is = nxi+1
               ie = nxi+ndepth

               neibpetot = neibpetot  + 1
               call set_ex_node(inod)

              enddo
             enddo
            endif

!    ---   outside wall (x<xmin)
!!
            if (ipe .eq. 1) then
             do knp = nb_rng1%knp_st, nb_rng1%knp_end
              do jnp = nb_rng1%jnp_st, nb_rng1%jnp_end

               call set_boundary_size(inp, jnp, knp)
               is = ndepth+1
               ie = 2*ndepth

               neibpetot = neibpetot  + 1
               call set_ex_node(inod)

              enddo
             enddo
            endif
!
!
!  outdside (y<ymax)
!
            if ( jpe .eq. ndy ) then
             jnp = 1
             do knp = nb_rng1%knp_st, nb_rng1%knp_end
              do inp = nb_rng1%inp_st, nb_rng1%inp_end

               call set_boundary_size(inp, jnp, knp)
               js = nyi+1
               je = nyi+ndepth

                  neibpetot = neibpetot  + 1
               call set_ex_node(inod)

              enddo
             enddo
            endif

!
!  outdside (y<ymin)
!
            if ( jpe .eq. 1 ) then
             do knp = nb_rng1%knp_st, nb_rng1%knp_end
              do inp = nb_rng1%inp_st, nb_rng1%inp_end

               call set_boundary_size(inp, jnp, knp)
               js = ndepth+1
               je = 2*ndepth

               neibpetot = neibpetot  + 1
               call set_ex_node(inod)

              enddo
             enddo
            endif

!
!  outdside (x>xmax, y>ymax)
!
            if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
             inp = 1
             jnp = 1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

               call set_boundary_size(inp, jnp, knp)
               is = nxi+1
               ie = nxi+ndepth
               js = nyi+1
               je = nyi+ndepth

               neibpetot = neibpetot  + 1
               call set_ex_node(inod)

              enddo
            endif

!
!  outdside (x>xmin, y<ymax)
!
            if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
             jnp = 1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

               call set_boundary_size(inp, jnp, knp)
               is = ndepth+1
               ie = 2*ndepth
               js = nyi+1
               je = nyi+ndepth

               neibpetot = neibpetot  + 1
               call set_ex_node(inod)

              enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

               call set_boundary_size(inp, jnp, knp)
               is = ndepth+1
               ie = 2*ndepth
               js = ndepth+1
               je = 2*ndepth

               neibpetot = neibpetot  + 1
               call set_ex_node(inod)

              enddo
            endif


!  outdside (x>xmax, y<ymin)
!
            if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
             inp = 1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

               call set_boundary_size(inp, jnp, knp)
               is = nxi+1
               ie = nxi+ndepth
               js = ndepth+1
               je = 2*ndepth

               neibpetot = neibpetot  + 1
               call set_ex_node(inod)

              enddo
            endif
!
      end subroutine set_export_peri
!
! ----------------------------------------------------------------------
!
      subroutine set_export_peri_quad(ipe, jpe, kpe, inod)
!
      use set_comm_edge_4_cube
!
      integer (kind = kint) :: ipe, jpe, kpe
      integer (kind = kint) :: inod
!
      integer (kind = kint) :: inp, jnp, knp
      integer (kind = kint) :: nd
!
!  outdside (x>xmax)
!
            if (ipe .eq. ndx) then
             inp = 1
             do knp = nb_rng1%knp_st, nb_rng1%knp_end
              do jnp = nb_rng1%jnp_st, nb_rng1%jnp_end
!
               call set_boundary_size(inp, jnp, knp)
               is = nxi+1
               ie = nxi+ndepth

               neibpetot = neibpetot  + 1
               call set_ex_node(inod)

               nd = 1
               call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

               nd = 2
               call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

               nd = 3
               call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
               write(*,*) 'export 1 from',                              &
     &             (neibpe(neibpetot)-1), inp, jnp, knp, inod

              enddo
             enddo
            endif

!    ---   outside wall (x<xmin)
!!
            if (ipe .eq. 1) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end
                do jnp = nb_rng1%jnp_st, nb_rng1%jnp_end

                 call set_boundary_size(inp, jnp, knp)
                 is = ndepth+1

                  neibpetot = neibpetot  + 1
                 ie = 2*ndepth
                 call set_ex_node(inod)

                 nd = 1
                 ie = 2*ndepth - 1
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 ie = 2*ndepth
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 ie = 2*ndepth
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'export 2 from',                            &
     &               (neibpe(neibpetot)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif
!
!
!  outdside (y<ymax)
!
            if ( jpe .eq. ndy ) then
             jnp = 1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end
                do inp = nb_rng1%inp_st, nb_rng1%inp_end

                 call set_boundary_size(inp, jnp, knp)
                 js = nyi+1
                 je = nyi+ndepth

                 neibpetot = neibpetot  + 1
                 call set_ex_node(inod)

                 nd = 1
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'export 3 from',                            &
     &               (neibpe(neibpetot)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif

!
!  outdside (y<ymin)
!
            if ( jpe .eq. 1 ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end
                do inp = nb_rng1%inp_st, nb_rng1%inp_end

                 call set_boundary_size(inp, jnp, knp)
                  js = ndepth+1

                  neibpetot = neibpetot  + 1
                  je = 2*ndepth
                 call set_ex_node(inod)

                 nd = 1
                 je = 2*ndepth
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 je = 2*ndepth - 1
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 je = 2*ndepth
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'export 4 from',                            &
     &               (neibpe(neibpetot)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif

!
!  outdside (x>xmax, y>ymax)
!
            if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
             inp = 1
             jnp = 1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

               call set_boundary_size(inp, jnp, knp)
               is = nxi+1
               ie = nxi+ndepth
               js = nyi+1
               je = nyi+ndepth

                  neibpetot = neibpetot  + 1
                 call set_ex_node(inod)

                 nd = 1
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'export 5 from',                            &
     &                (neibpe(neibpetot)-1), inp, jnp, knp, inod

              enddo
            endif

!
!  outdside (x>xmin, y<ymax)
!
            if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
             jnp = 1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                 call set_boundary_size(inp, jnp, knp)
                 js = nyi+1
                 je = nyi+ndepth
                 is = ndepth+1

                 neibpetot = neibpetot  + 1
                 ie = 2*ndepth
                 call set_ex_node(inod)

                 nd = 1
                 ie = 2*ndepth - 1
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 ie = 2*ndepth
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 ie = 2*ndepth
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'export 6 from',                            &
     &               (neibpe(neibpetot)-1), inp, jnp, knp, inod

              enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                 call set_boundary_size(inp, jnp, knp)
                 is = ndepth+1
                 js = ndepth+1

                 neibpetot = neibpetot  + 1
                 ie = 2*ndepth
                 je = 2*ndepth
                 call set_ex_node(inod)

                 nd = 1
                 ie = 2*ndepth - 1
                 je = 2*ndepth
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 ie = 2*ndepth
                 je = 2*ndepth - 1
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 ie = 2*ndepth
                 je = 2*ndepth
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'export 7 from',                            &
     &               (neibpe(neibpetot)-1), inp, jnp, knp, inod

              enddo
            endif


!  outdside (x>xmax, y<ymin)
!
            if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
             inp = 1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                 call set_boundary_size(inp, jnp, knp)
                 is = nxi+1
                 ie = nxi+ndepth
                 js = ndepth+1

                 neibpetot = neibpetot  + 1
                 je = 2*ndepth
                 call set_ex_node(inod)

                 nd = 1
                 je = 2*ndepth
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 je = 2*ndepth - 1
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 je = 2*ndepth
                 call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'export 8 to',                              &
     &              (neibpe(neibpetot)-1), inp, jnp, knp, inod

              enddo
            endif
!
      end subroutine set_export_peri_quad
!
! ----------------------------------------------------------------------
!
      end module set_export_peri_cube
