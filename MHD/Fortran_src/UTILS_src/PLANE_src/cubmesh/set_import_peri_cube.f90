!set_import_peri_cube.f90
!     module set_import_peri_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine set_import_peri(ipe, jpe, inod)
!      subroutine set_import_peri_quad(ipe, jpe, kpe, inod)
!
      module set_import_peri_cube
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
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_import_peri(ipe, jpe, inod)
!
      integer (kind = kint) :: ipe, jpe
      integer (kind = kint) :: inod
!
      integer (kind = kint) :: inp, jnp, knp
!
!
!    ---   outside wall (x<xmin)
!
!                                     .... count nodes 
            if (ipe .eq. 1) then
             do knp = nb_rng1%knp_st, nb_rng1%knp_end
               do jnp = nb_rng1%jnp_st, nb_rng1%jnp_end

                call set_sleeve_size(inp, jnp, knp, nb_rng1)
                is = 1
                ie = ndepth

                neibpetot = neibpetot  + 1
                call set_im_node(inod)

                enddo
              enddo
            endif
!
!  outdside (x>xmax)
!
!                                     .... count nodes 
!
            if (ipe .eq. ndx) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end
                do jnp = nb_rng1%jnp_st, nb_rng1%jnp_end
!
                call set_sleeve_size(inp, jnp, knp, nb_rng1)
                is = nxi+ndepth+1
                ie = nxi+2*ndepth

                neibpetot = neibpetot  + 1
                call set_im_node(inod)

                enddo
              enddo
            endif

!
!  outdside (y<ymin)
!
!                                     .... count nodes 
            if ( jpe .eq. 1 ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end
                do inp = nb_rng1%inp_st, nb_rng1%inp_end

                call set_sleeve_size(inp, jnp, knp, nb_rng1)
                js = 1
                je = ndepth

                neibpetot = neibpetot  + 1
                call set_im_node(inod)

                enddo
              enddo
            endif

!
!  outdside (y<ymax)
!
!                                     .... count nodes 
            if ( jpe .eq. ndy ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end
                do inp = nb_rng1%inp_st, nb_rng1%inp_end

                call set_sleeve_size(inp, jnp, knp, nb_rng1)
                js = nyi+ndepth+1
                je = nyi+2*ndepth

                neibpetot = neibpetot  + 1
                call set_im_node(inod)

                enddo
              enddo
            endif

!
!  outdside (x<xmin, y<ymin)
!
            if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                call set_sleeve_size(inp, jnp, knp, nb_rng1)
                 is = 1
                 ie = ndepth
                 js = 1
                 je = ndepth

                  neibpetot = neibpetot  + 1
                call set_im_node(inod)

              enddo
            endif


!  outdside (x>xmax, y<ymin)
!
            if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                call set_sleeve_size(inp, jnp, knp, nb_rng1)
                 is = nxi+ndepth+1
                 ie = nxi+2*ndepth
                 js = 1
                 je = ndepth

                  neibpetot = neibpetot  + 1
                call set_im_node(inod)

              enddo
            endif

!
!  outdside (x>xmax, y<ymax)
!
            if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                call set_sleeve_size(inp, jnp, knp, nb_rng1)
                 is = nxi+ndepth+1
                 ie = nxi+2*ndepth
                 js = nyi+ndepth+1
                 je = nyi+2*ndepth

                  neibpetot = neibpetot  + 1
                call set_im_node(inod)

              enddo
            endif

!
!  outdside (x>xmin, y<ymax)
!
            if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                call set_sleeve_size(inp, jnp, knp, nb_rng1)
                 is = 1
                 ie = ndepth
                 js = nyi+ndepth+1
                 je = nyi+2*ndepth

                  neibpetot = neibpetot  + 1
                call set_im_node(inod)

              enddo
            endif
!
      end subroutine  set_import_peri
!
! ----------------------------------------------------------------------
!
      subroutine set_import_peri_quad(ipe, jpe, kpe, inod)
!
      use set_comm_edge_4_cube
!
      integer (kind = kint) :: ipe, jpe, kpe
      integer (kind = kint) :: inod
!
      integer (kind = kint) :: inp, jnp, knp
      integer (kind = kint) :: nd
!
!    ---   outside wall (x<xmin)
!
            if (ipe .eq. 1) then
             inp = -1
             do knp = nb_rng1%knp_st, nb_rng1%knp_end
              do jnp = nb_rng1%jnp_st, nb_rng1%jnp_end

               call set_sleeve_size(inp, jnp, knp, nb_rng1)
                 is = 1
                 ie = ndepth

                 neibpetot = neibpetot  + 1
                 call set_im_node(inod)

                 nd = 1
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'import  1 from',                           &
     &                     (neibpe(neibpetot)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif
!
!  outdside (x>xmax)
!                                     .... count nodes
            if (ipe .eq. ndx) then
             do knp = nb_rng1%knp_st, nb_rng1%knp_end
               do jnp = nb_rng1%jnp_st, nb_rng1%jnp_end
!
               call set_sleeve_size(inp, jnp, knp, nb_rng1)
                is = nxi+ndepth+1

                 neibpetot = neibpetot  + 1
                 ie = nxi+2*ndepth
                 call set_im_node(inod)

                 nd = 1
                ie = nxi+2*ndepth - 1
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                ie = nxi+2*ndepth
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                ie = nxi+2*ndepth
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'import 2 from',                            &
     &                      (neibpe(neibpetot)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif
!
!  outdside (y<ymin)
!                                     .... count nodes 
            if ( jpe .eq. 1 ) then
             jnp = -1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end
               do inp = nb_rng1%inp_st, nb_rng1%inp_end

                call set_sleeve_size(inp, jnp, knp, nb_rng1)
                js = 1
                je = ndepth

                 neibpetot = neibpetot  + 1
                 call set_im_node(inod)

                 nd = 1
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'import 3 from',                            &
     &                      (neibpe(neibpetot)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif
!
!  outdside (y>ymax)
!                                     .... count nodes 
            if ( jpe .eq. ndy ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end
                do inp = nb_rng1%inp_st, nb_rng1%inp_end

                 call set_sleeve_size(inp, jnp, knp, nb_rng1)
                 js = nyi+ndepth+1

                 neibpetot = neibpetot  + 1
                 je = nyi+2*ndepth
                 call set_im_node(inod)

                 nd = 1
                 je = nyi+2*ndepth
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 je = nyi+2*ndepth - 1
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 je = nyi+2*ndepth
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'import 4 from',                            &
     &                      (neibpe(neibpetot)-1), inp, jnp, knp, inod

                enddo
              enddo
            endif
!
!  outdside (x<xmin, y<ymin)
!
            if ( ipe .eq. 1  .and. jpe .eq. 1 ) then
             inp = -1
             jnp = -1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                 call set_sleeve_size(inp, jnp, knp, nb_rng1)
                 is = 1
                 ie = ndepth
                 js = 1
                 je = ndepth

                 neibpetot = neibpetot  + 1
                 call set_im_node(inod)

                 nd = 1
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'import 5 from',                            &
     &                      (neibpe(neibpetot)-1), inp, jnp, knp, inod

              enddo
            endif
!
!  outdside (x>xmax, y<ymin)
!
            if ( ipe .eq. ndx  .and. jpe .eq. 1 ) then
             jnp = -1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                 call set_sleeve_size(inp, jnp, knp, nb_rng1)
                 js = 1
                 je = ndepth
                 is = nxi+ndepth+1

                 neibpetot = neibpetot  + 1
                 ie = nxi+2*ndepth
                 call set_im_node(inod)

                 nd = 1
                 ie = nxi+2*ndepth - 1
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 ie = nxi+2*ndepth
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 ie = nxi+2*ndepth
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'import 6 from',                            &
     &                      (neibpe(neibpetot)-1), inp, jnp, knp, inod

              enddo
            endif
!
!  outdside (x>xmax, y<ymax)
!
            if ( ipe .eq. ndx  .and. jpe .eq. ndy ) then
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                 call set_sleeve_size(inp, jnp, knp, nb_rng1)
                 is = nxi+ndepth+1
                 js = nyi+ndepth+1

                 neibpetot = neibpetot  + 1
                 ie = nxi+2*ndepth
                 je = nyi+2*ndepth
                 call set_im_node(inod)

                 nd = 1
                 ie = nxi+2*ndepth - 1
                 je = nyi+2*ndepth
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 ie = nxi+2*ndepth
                 je = nyi+2*ndepth - 1
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 ie = nxi+2*ndepth
                 je = nyi+2*ndepth
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'import 7 from',                            &
     &                      (neibpe(neibpetot)-1), inp, jnp, knp, inod

              enddo
            endif
!
!  outdside (x>xmin, y<ymax)
!
            if ( ipe .eq. 1  .and. jpe .eq. ndy ) then
             inp = -1
              do knp = nb_rng1%knp_st, nb_rng1%knp_end

                 call set_sleeve_size(inp, jnp, knp, nb_rng1)
                 is = 1
                 ie = ndepth
                 js = nyi+ndepth+1

                 neibpetot = neibpetot  + 1
                 je = nyi+2*ndepth
                 call set_im_node(inod)

                 nd = 1
                 je = nyi+2*ndepth
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 2
                 je = nyi+2*ndepth - 1
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)

                 nd = 3
                 je = nyi+2*ndepth
                 call set_im_edge(kpe, inp, jnp, knp, inod, nd)
                 write(*,*) 'import 8 from',                            &
     &                      (neibpe(neibpetot)-1), inp, jnp, knp, inod

              enddo
            endif
!
      end subroutine  set_import_peri_quad
!
! ----------------------------------------------------------------------
!
      end module set_import_peri_cube
