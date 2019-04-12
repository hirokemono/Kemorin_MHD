!count_import_inside_cube.f90
!     module count_import_inside_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine count_import_inside(nb_rng, icou, inod)
!!      subroutine count_import_inside_quad(nb_rng, kpe, icou, inod)
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module count_import_inside_cube
!
      use m_precision
!
      use t_neib_range_cube
      use m_size_of_cube
      use m_comm_data_cube_kemo
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
      subroutine count_import_inside(nb_rng, icou, inod)
!
      type(neib_range_cube), intent(in) :: nb_rng
!
      integer (kind = kint), intent(inout) :: icou, inod
!
      integer (kind = kint) :: inp, jnp, knp
!
!
            do knp = nb_rng%knp_st, nb_rng%knp_end
             do jnp = nb_rng%jnp_st, nb_rng%jnp_end
              do inp = nb_rng%inp_st, nb_rng%inp_end

               if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

               call set_sleeve_size                                     &
     &            (nb_rng, ndepth, inp, jnp, knp, sl_rng1)

               icou = icou  + 1
               call count_node_id(inod)

               stack_import(icou) = inod

              enddo
             enddo
            enddo
!
      end subroutine count_import_inside
!
! ----------------------------------------------------------------------
!
      subroutine count_import_inside_quad(nb_rng, kpe, icou, inod)
!
      use set_comm_edge_4_cube
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in)  :: kpe
!
      integer (kind = kint), intent(inout) :: icou, inod
!
      integer (kind = kint) :: inp, jnp, knp
      integer (kind = kint) :: nd
!
      do knp = nb_rng%knp_st, nb_rng%knp_end
       do jnp = nb_rng%jnp_st, nb_rng%jnp_end
        do inp = nb_rng%inp_st, nb_rng%inp_end

         if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

         call set_sleeve_size                                           &
     &      (nb_rng, ndepth, inp, jnp, knp, sl_rng1)
!
         icou = icou  + 1

         call count_node_id(inod)

         nd = 1
         call count_im_edge(kpe, inp, jnp, knp, inod, nd)

         nd = 2
         call count_im_edge(kpe, inp, jnp, knp, inod, nd)

         nd = 3
         call count_im_edge(kpe, inp, jnp, knp, inod, nd)

         stack_import(icou) = inod

         enddo
        enddo
       enddo
!
      end subroutine count_import_inside_quad
!
! ----------------------------------------------------------------------
!
      end module count_import_inside_cube
