!set_neib_pe_cube.f90
!     module set_neib_pe_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
! ***** set and write for sleeve edges for periodical boundary
!
!!      subroutine set_neighboring_pes(nb_rng, ndx, ndy, pe_id)
!!      subroutine set_neighboring_pes_peri                             &
!!     &         (nb_rng, ndx, ndy, pe_id, ipe, jpe)
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_neib_pe_cube
!
      use m_precision
!
      use t_neib_range_cube
      use m_comm_data_cube_kemo
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_neighboring_pes(nb_rng, ndx, ndy, pe_id)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer(kind = kint), intent(in) :: ndx, ndy
      integer(kind = kint), intent(in) :: pe_id
!
      integer (kind = kint) :: inp, jnp, knp
!
!                                       .. set neighbor pe
!      inside cube
!
      do knp = nb_rng%knp_st, nb_rng%knp_end
        do jnp = nb_rng%jnp_st, nb_rng%jnp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

            neibpetot = neibpetot  + 1
            neibpe(neibpetot) =  pe_id + inp + jnp*ndx + knp*ndx*ndy
!
          enddo
        enddo
      enddo
!
      end subroutine  set_neighboring_pes
!
! ----------------------------------------------------------------------
!
      subroutine set_neighboring_pes_peri                               &
     &         (nb_rng, ndx, ndy, pe_id, ipe, jpe)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer(kind = kint), intent(in) :: ndx, ndy
      integer(kind = kint), intent(in) :: pe_id
      integer(kind = kint), intent(in) :: ipe, jpe
!
      integer(kind = kint) :: inp, jnp, knp
!
!
!
!      edge for x < xmin
!
      if( ipe == 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do jnp = nb_rng%jnp_st, nb_rng%jnp_end

            neibpetot = neibpetot  + 1
            neibpe     (neibpetot) = pe_id                              &
     &                              + ndx-1 + jnp*ndx + knp*ndx*ndy
!
           enddo
         enddo
       end if
!
!      edge for x > xmax
!
      if( ipe == ndx ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do jnp = nb_rng%jnp_st, nb_rng%jnp_end

            neibpetot = neibpetot  + 1
            neibpe     (neibpetot) =  pe_id                             &
     &                               + 1-ndx + jnp*ndx + knp*ndx*ndy
!
           enddo
         enddo
       end if
!
!      edge for y > ymin
!
      if( jpe == 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            neibpetot = neibpetot  + 1
            neibpe     (neibpetot) =  pe_id                             &
     &                              + inp + (ndy-1)*ndx +  knp*ndx*ndy
!
           enddo
         enddo
       end if
!
!      edge for y > ymax
!
!
      if( jpe == ndy ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end
          do inp = nb_rng%inp_st, nb_rng%inp_end

            neibpetot = neibpetot  + 1
            neibpe     (neibpetot) = pe_id                              &
     &                              + inp + (1-ndy)*ndx + knp*ndx*ndy
!
           enddo
         enddo
       end if
!
!
!      edge for x < xmin, y<ymin
!
      if( ipe == 1 .and. jpe == 1) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

            neibpetot = neibpetot  + 1
            neibpe     (neibpetot) = pe_id                              &
     &                              + ndx-1 + (ndy-1)*ndx + knp*ndx*ndy
!
         enddo
       end if
!
!      edge for x > xmax, y<ymin
!
      if( ipe == ndx .and. jpe == 1 ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

            neibpetot = neibpetot  + 1
            neibpe     (neibpetot) =  pe_id                             &
     &                            + 1-ndx + (ndy-1)*ndx + knp*ndx*ndy
!
         enddo
       end if
!
!      edge for x>xmax, y > ymax
!
      if( ipe == ndx .and. jpe == ndy ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

            neibpetot = neibpetot  + 1
            neibpe     (neibpetot) =  pe_id                             &
     &                         + 1-ndx + (1-ndy)*ndx +  knp*ndx*ndy
!
         enddo
       end if
!
!      edge for x<xmin, y > ymax
!
!
      if( ipe == 1 .and. jpe == ndy ) then
        do knp = nb_rng%knp_st, nb_rng%knp_end

            neibpetot = neibpetot  + 1
            neibpe     (neibpetot) = pe_id                              &
     &                            + ndx-1 + (1-ndy)*ndx + knp*ndx*ndy
!
         enddo
       end if
!
      end subroutine set_neighboring_pes_peri
!
! ----------------------------------------------------------------------
!
      end module set_neib_pe_cube
